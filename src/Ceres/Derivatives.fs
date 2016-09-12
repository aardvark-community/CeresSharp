namespace Derivatives

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.NativeInterop
open System
open Aardvark.Base
open System.Threading
open System.Runtime.InteropServices

#nowarn "9"

type ParameterBoundMask =
    | None = 0x00
    | Min = 0x01
    | Max = 0x02

type ParameterBound =
    struct
        val mutable public Mask : ParameterBoundMask
        val mutable public Min : float
        val mutable public Max : float
    end

[<Struct>]
type Parameter<'a>(id : int, loc : ref<nativeptr<float>>, write : 'a -> nativeptr<float> -> int -> unit, read : nativeptr<float> -> int -> 'a) =
    member x.Id = id
    member x.Value = read !loc id


[<AutoOpen>]
module Operators =
    let inline (=~=) (l : 'a) (r : 'b) =
        ((^a or ^b) : (static member Equation : 'a * 'b -> Equation) (l,r))


module Ceres =
    open System.Runtime.InteropServices
    open Microsoft.FSharp.Reflection

    [<DllImport("CeresCpp.dll", EntryPoint="solve")>]
    extern float ceres_solve(int parameterCount, int residualCount, void* evaluate, float* parameter, ParameterBound* bounds)


type CeresFun = delegate of nativeptr<nativeptr<float>> * nativeptr<float> * nativeptr<nativeptr<float>> -> int



type Rot3s(axis : V3s, angle : scalar) =
    member x.Axis = axis
    member x.Angle = angle

    static member (*) (m : Rot3s, v : V3s) =
        M44s.Rotation(m.Axis, m.Angle) * V4s(v.X, v.Y, v.Z, scalar 0.0) |> Vec.xyz


type Solver() =
    let mutable parameterCount = 0
    let mutable storage : nativeptr<float> = NativePtr.zero
    let mutable bounds : nativeptr<ParameterBound> = NativePtr.zero

    let mutable pp : ref<nativeptr<float>> = ref storage
    let mutable capacity = 0

    let resize(additional : int) =
        if parameterCount + additional > capacity then
            let cap = Fun.NextPowerOfTwo (parameterCount + additional)
            let np = NativePtr.alloc cap // Marshal.AllocHGlobal (sizeof<float> * cap)
            let nb = NativePtr.alloc cap
            if capacity <> 0 then
                NativeInt.memcpy (NativePtr.toNativeInt storage) (NativePtr.toNativeInt np) (sizeof<float> * parameterCount)
                NativeInt.memcpy (NativePtr.toNativeInt bounds) (NativePtr.toNativeInt nb) (sizeof<ParameterBound> * parameterCount)
                NativePtr.free storage
                NativePtr.free bounds
                let off = (sizeof<ParameterBound> * parameterCount) |> nativeint
                let size = (sizeof<ParameterBound> * cap) |> nativeint
                NativeInt.memset (NativePtr.toNativeInt nb + off) 0 (int (size - off))
            else
                NativeInt.memset (NativePtr.toNativeInt nb) 0 (sizeof<ParameterBound> * cap)
                

            storage <- np
            bounds <- nb
            pp := storage
            capacity <- cap

    member x.New(value : 'a, dim : int, read : nativeptr<float> -> int -> 'a, write : 'a -> nativeptr<float> -> int -> unit) =
        resize dim
        let id = parameterCount
        parameterCount <- parameterCount + dim

        write value storage id

        Parameter<'a>(id, pp, write, read)

    member inline x.New(value : ^a) =
        let size = (^a : (static member CeresDim : int) ())
        let read ptr id = (^a : (static member CeresRead : nativeptr<float> * int -> ^a) (ptr, id))
        let write value ptr id = (^a : (static member CeresWrite : ^a * nativeptr<float> * int -> unit) (value, ptr, id))
        x.New(value, size, read, write)

    member inline x.New(value : list< ^a >) =
        value |> List.map x.New

    member inline x.New(value : array< ^a >) =
        value |> Array.map x.New

    member x.NewRot3d() =
        let read (ptr : nativeptr<float>) (id : int) =
            let vec = V3s.CeresRead(ptr, id)
            let l = vec.Length
            Rot3s(vec / l, l)

        let write (value : Rot3s) (ptr : nativeptr<float>) (id : int) = 
            V3s.CeresWrite(V3s.IOO, ptr, id)

        x.New(Rot3s(V3s.IOO, scalar 1.0), 3, read, write)



    member x.New(v : float) = x.New (scalar v)
    member x.New(v : V2d) = x.New (V2s v)
    member x.New(v : V3d) = x.New (V3s v)
    member x.New(v : V4d) = x.New (V4s v)


    
    member x.New(v : float, range : Range1d) = 
        let p = x.New (scalar v)
        let mutable b = ParameterBound()

        if range.Min <> Double.NegativeInfinity then
            b.Mask <- ParameterBoundMask.Min ||| b.Mask
            b.Min <- range.Min

        if range.Max <> Double.PositiveInfinity then
            b.Mask <- ParameterBoundMask.Max ||| b.Mask
            b.Max <- range.Max

        NativePtr.set bounds p.Id b

        p

    member x.Solve (f : unit -> scalar[]) =
        
        let blubber = f()
        let residualCount = blubber.Length

        let func = 
            CeresFun (fun parameters residuals jacobians ->
                pp := NativePtr.read parameters
                let res = f()

                for i in 0..residualCount-1 do
                    NativePtr.set residuals i res.[i].Value

                if NativePtr.toNativeInt jacobians <> 0n then
                    let mutable j = NativePtr.read jacobians
                    if NativePtr.toNativeInt j <> 0n then 
                        for i in 0..residualCount-1 do
                            for pi in 0..parameterCount-1 do
                                match Map.tryFind pi res.[i].Jacobian with
                                    | Some v -> NativePtr.write j v
                                    | None -> NativePtr.write j 0.0
                                j <- NativePtr.add j 1
                1
            )

        let gc = GCHandle.Alloc(func, GCHandleType.Normal)
        try 
            let ptr = Marshal.GetFunctionPointerForDelegate func
            let res = Ceres.ceres_solve(parameterCount, residualCount, ptr, storage, bounds)
            pp := storage
            res
        finally
            gc.Free()

    member x.Solve (f : unit -> list<Equation>) =
        x.Solve (fun () -> f() |> List.collect (fun e -> e.Residuals) |> List.toArray)

    member x.Dispose() =
        NativePtr.free storage
        storage <- NativePtr.zero
        pp := NativePtr.zero
        capacity <- 0
        parameterCount <- 0

    interface IDisposable with
        member x.Dispose() = x.Dispose()

module Bla =
    
    let inline (!) (m : ^a) =
        (^a : (member get_Value : unit -> ^b) (m))

    let solveBla() =
        use s = new Solver()

        //let angleAxis = s.New(V3s.IOO)

        let m = s.NewRot3d()

        let p = s.New(0.51, Range1d(0.5, 2.0))

        let res = 
            s.Solve (fun () ->
                [
                    !p * !p =~= !p
                    !m * V3s.IOO =~= V3s.OIO
                    !m * V3s.OIO =~= -V3s.IOO
                ]
            )

        let axis = !m.Value.Axis
        let angle = !m.Value.Angle
        printfn "axis:  %A" axis
        printfn "angle: %A" angle
        printfn "angle: %A" !p.Value



