namespace CeresSharp

open System
open System.Threading
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open CeresSharp.Raw
open Microsoft.FSharp.NativeInterop
open Aardvark.Base
open CeresSharp.Raw 

#nowarn "9"

type Block =
    inherit IDisposable
    abstract member Pointer : nativeptr<float>
    abstract member DoublesPerElement : int
    abstract member Count : int
    abstract member Mark : unit -> unit

type Block<'b> =
    inherit Block
    abstract member Read : offset : int * data : nativeptr<float> * derivatives : bool * target : 'b[] -> unit
   
type Block<'a, 'b> = 
    inherit Block<'b>
    abstract member Result : 'a[]

type private SimpleParameterBlock<'a, 'b when 'a : unmanaged>(data : 'a[], read : int -> 'a -> 'b) =
    static let sa = nativeint sizeof<'a>
    static let doubles = sizeof<'a> / sizeof<double>

    let gc = GCHandle.Alloc(data, GCHandleType.Pinned)
    let ptr : nativeptr<float>  = NativePtr.ofNativeInt (gc.AddrOfPinnedObject())
    let read = OptimizedClosures.FSharpFunc<int, 'a, 'b>.Adapt(read)
    member x.Pointer = ptr
    member x.DoublesPerElement = doubles
    member x.Count = data.Length
    member x.Mark() = ()

    member x.Read(offset : int, src : nativeptr<float>, derivatives : bool, target : 'b[]) =
        if derivatives then
            let mutable ptr = NativePtr.toNativeInt src
            let mutable index = offset
            for i in 0 .. data.Length - 1 do
                let a = NativePtr.read (NativePtr.ofNativeInt<'a> ptr)
                let b = read.Invoke(index, a)
                target.[i] <- b
                index <- index + doubles
                ptr <- ptr + sa
        else
            let mutable ptr = NativePtr.toNativeInt src
            for i in 0 .. data.Length - 1 do
                let a = NativePtr.read (NativePtr.ofNativeInt<'a> ptr)
                let b = read.Invoke(-1, a)
                target.[i] <- b
                ptr <- ptr + sa
            
    member private x.Dispose(disposing : bool) =
        if disposing then GC.SuppressFinalize x
        gc.Free()

    member x.Dispose() = x.Dispose(true)
    override x.Finalize() = x.Dispose(false)

    interface IDisposable with
        member x.Dispose() = x.Dispose()

    interface Block with
        member x.Count = x.Count
        member x.Pointer = x.Pointer
        member x.Mark() = x.Mark()
        member x.DoublesPerElement = x.DoublesPerElement

    interface Block<'b> with
        member x.Read(offset, src, derivatives, target) = x.Read(offset, src, derivatives, target)

    interface Block<'a, 'b> with
        member x.Result = data

type private WrappedParameterBlock<'a, 'b, 'c>(data : 'a[], pickle : 'a -> 'b, unpickle : 'b -> 'a, construct : 'b[] -> Block<'b, 'c>) =
    let mutable dirty = false
    let inner = data |> Array.map pickle |> construct

    let unpickle() =
        if dirty then
            dirty <- false
            let inner = inner.Result
            for i in 0 .. data.Length - 1 do
                data.[i] <- unpickle inner.[i]

    interface IDisposable with
        member x.Dispose() = inner.Dispose()

    interface Block with
        member x.Pointer = inner.Pointer
        member x.DoublesPerElement = inner.DoublesPerElement
        member x.Count = inner.Count
        member x.Mark() = dirty <- true

    interface Block<'c> with
        member x.Read(offset : int, data : nativeptr<float>, derivatives : bool, target : 'c[]) =
            inner.Read(offset, data, derivatives, target)

            
    interface Block<'a, 'c> with
        member x.Result =
            unpickle()
            data
                

type Problem() =
    let mutable handle = CeresRaw.cCreateProblem()

    let blocks = HashSet<Block>()
    let costFunctions = List<CeresCostFunction * GCHandle * IDisposable>()
    let loss = Dict<LossFunction, CeresLossFunctionHandle>()

    let getLoss (l : LossFunction) =
        loss.GetOrCreate(l, fun l ->
            CeresRaw.cCreateLossFunction (LossFunction.toCeresLossFunction l)
        )

    member x.AddBlock(b : Block) =
        blocks.Add b |> ignore

    member x.AddCostFunction(parameters : array<int * nativeptr<float>>, residualCount : int, loss : LossFunction, f : nativeptr<nativeptr<float>> * nativeptr<float> * nativeptr<nativeptr<float>> -> int, free : unit -> unit) =
        let del = 
            CeresCostFunctionDelegate(fun parameters residuals jacobians ->
                f(parameters, residuals, jacobians)
            )

        let gc = GCHandle.Alloc(del)
        let ptr = Marshal.GetFunctionPointerForDelegate(del)
        
        let fhandle = CeresRaw.cCreateCostFunction(parameters.Length, Array.map fst parameters, residualCount, ptr)
        costFunctions.Add(fhandle, gc, { new IDisposable with member x.Dispose() = free() })

        let loss = getLoss loss

        match Array.map snd parameters with
            | [||] -> failwith "cost function has no parameters"
            | [|p0|] -> CeresRaw.cAddResidualFunction1(handle, loss, fhandle, p0)
            | [|p0; p1|] -> CeresRaw.cAddResidualFunction2(handle, loss, fhandle, p0, p1)
            | [|p0; p1; p2|] -> CeresRaw.cAddResidualFunction3(handle, loss, fhandle, p0, p1, p2)
            | [|p0; p1; p2; p3|] -> CeresRaw.cAddResidualFunction4(handle, loss, fhandle, p0, p1, p2, p3)
            | [|p0; p1; p2; p3; p4|] -> CeresRaw.cAddResidualFunction5(handle, loss, fhandle, p0, p1, p2, p3, p4)
            | [|p0; p1; p2; p3; p4; p5|] -> CeresRaw.cAddResidualFunction6(handle, loss, fhandle, p0, p1, p2, p3, p4, p5)
            | [|p0; p1; p2; p3; p4; p5; p6|] -> CeresRaw.cAddResidualFunction7(handle, loss, fhandle, p0, p1, p2, p3, p4, p5, p6)
            | [|p0; p1; p2; p3; p4; p5; p6; p7|] -> CeresRaw.cAddResidualFunction8(handle, loss, fhandle, p0, p1, p2, p3, p4, p5, p6, p7)
            | _ -> failwithf "too many parameter-blocks for cost function: %A" parameters.Length
            
    member x.Solve(options : Config) =
        use termination = fixed [| CeresTerminationType.Convergence |]
        use usable = fixed [| 0 |]
        use pOptions = fixed [| Config.toCeresOptions options |]
        let res = CeresRaw.cSolve(handle, pOptions, termination, usable)
        for b in blocks do b.Mark()
        
        let termination = NativePtr.read termination
        match termination with
        | CeresTerminationType.Convergence
        | CeresTerminationType.UserSuccess ->
            res
        | _ ->
            System.Double.PositiveInfinity
            
    member x.TrySolve(options : Config) =
        use termination = fixed [| CeresTerminationType.Convergence |]
        use usable = fixed [| 0 |]
        use pOptions = fixed [| Config.toCeresOptions options |]
        let res = CeresRaw.cSolve(handle, pOptions, termination, usable)
        for b in blocks do b.Mark()
        
        let termination = NativePtr.read termination
        let usable = NativePtr.read usable
        
        if usable <> 0 then
            match termination with
            | CeresTerminationType.Convergence
            | CeresTerminationType.UserSuccess ->
                Some (true, res)
            | _ ->
                Some (false, res)
        else
            None

    member private x.Dispose(disposing : bool) =
        if disposing then GC.SuppressFinalize(x)
        let o = Interlocked.Exchange(&handle.Handle, 0n)
        if o <> 0n then 
            //for h in loss.Values do
            //    CeresRaw.cReleaseLossFunction(h)
            loss.Clear()

            for (f, gc, cf) in costFunctions do
                gc.Free()
                cf.Dispose()
                //CeresRaw.cReleaseCostFunction f
            costFunctions.Clear()
            
            CeresRaw.cReleaseProblem(CeresProblem(o))

            blocks.Clear()

    member x.Dispose() = x.Dispose true
    override x.Finalize() = x.Dispose false

    interface IDisposable with
        member x.Dispose() = x.Dispose true

