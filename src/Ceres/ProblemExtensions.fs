namespace CeresSharp

open Aardvark.Base
open CeresSharp
open Microsoft.FSharp.NativeInterop

#nowarn "9"

[<AutoOpen>]
module ProblemExtensions =

    type Reader =
        static member Read(offset : int, value : float) = 
            if offset < 0 then
                scalar value
            else
                scalar.Variable(offset, value)

        static member Read(offset : int, value : V2d) = 
            if offset < 0 then
                V2s value
            else
                V2s(
                    scalar.Variable(offset + 0, value.X),
                    scalar.Variable(offset + 1, value.Y)
                )

        static member Read(offset : int, value : V3d) = 
            if offset < 0 then  
                V3s value
            else
                V3s(
                    scalar.Variable(offset + 0, value.X),
                    scalar.Variable(offset + 1, value.Y),
                    scalar.Variable(offset + 2, value.Z)
                )

        static member Read(offset : int, value : V4d) = 
            if offset < 0 then  
                V4s value
            else
                V4s(
                    scalar.Variable(offset + 0, value.X),
                    scalar.Variable(offset + 1, value.Y),
                    scalar.Variable(offset + 2, value.Z),
                    scalar.Variable(offset + 3, value.Z)
                )
                
        static member Read(offset : int, value : M22d) = 
            if offset < 0 then  
                M22s(value.M00, value.M01, value.M10, value.M11)
            else
                M22s(
                    scalar.Variable(offset + 0, value.M00),
                    scalar.Variable(offset + 1, value.M01),
                    scalar.Variable(offset + 2, value.M10),
                    scalar.Variable(offset + 3, value.M11)
                )
                 
        static member Read(offset : int, value : M33d) = 
            if offset < 0 then  
                M33s(value.M00, value.M01, value.M02, value.M10, value.M11, value.M12, value.M20, value.M21, value.M22)
            else
                M33s(
                    scalar.Variable(offset + 0, value.M00),
                    scalar.Variable(offset + 1, value.M01),
                    scalar.Variable(offset + 2, value.M02),
                    
                    scalar.Variable(offset + 3, value.M10),
                    scalar.Variable(offset + 4, value.M11),
                    scalar.Variable(offset + 5, value.M12),
                    
                    scalar.Variable(offset + 6, value.M20),
                    scalar.Variable(offset + 7, value.M21),
                    scalar.Variable(offset + 8, value.M22)
                )
                 
        static member Read(offset : int, value : M44d) = 
            if offset < 0 then  
                M44s(
                    value.M00, value.M01, value.M02, value.M03,
                    value.M10, value.M11, value.M12, value.M13,
                    value.M20, value.M21, value.M22, value.M23,
                    value.M30, value.M31, value.M32, value.M33
                )
            else
                M44s(
                    scalar.Variable(offset + 0,  value.M00),
                    scalar.Variable(offset + 1,  value.M01),
                    scalar.Variable(offset + 2,  value.M02),
                    scalar.Variable(offset + 3,  value.M03),
                                                 
                    scalar.Variable(offset + 4,  value.M10),
                    scalar.Variable(offset + 5,  value.M11),
                    scalar.Variable(offset + 6,  value.M12),
                    scalar.Variable(offset + 7,  value.M13),
                    
                    scalar.Variable(offset + 8,  value.M20),
                    scalar.Variable(offset + 9,  value.M21),
                    scalar.Variable(offset + 10, value.M22),
                    scalar.Variable(offset + 11, value.M23),
                    
                    scalar.Variable(offset + 12, value.M30),
                    scalar.Variable(offset + 13, value.M31),
                    scalar.Variable(offset + 14, value.M32),
                    scalar.Variable(offset + 15, value.M33)
                )

    let inline private readAux (d : ^a) (offset : int) (b : ^b) :  ^c =
        ((^a or ^b or ^c) : (static member Read : int * ^b -> ^c) (offset, b))

    let inline read offset value =
        readAux Unchecked.defaultof<Reader> offset value



    type Problem with   

        member x.AddParameterBlock<'a, 'b when 'a : unmanaged>(data : 'a[], read : int -> 'a -> 'b) =
            let block = new SimpleParameterBlock<_,_>(data, read)
            block :> Block<_,_>
            
        member inline x.AddParameterBlock data = x.AddParameterBlock(data, read)


        member x.AddParameterBlock<'a, 'b, 'c when 'b : unmanaged>(data : 'a[], pickle : 'a -> 'b, unpickle : 'b -> 'a, read : int -> 'b -> 'c) =
            let create (bs : 'b[]) = x.AddParameterBlock(bs, read)
            new WrappedParameterBlock<'a, 'b, 'c>(data, pickle, unpickle, create) :> Block<'a, 'c>
            
        member inline x.AddParameterBlock(data, pickle, unpickle) = x.AddParameterBlock(data, pickle, unpickle, read)
        
        member x.AddCostFunction(count : int, blockSize : int, blocks : array<Block>, readBlock : array<int -> bool -> nativeptr<float> -> unit>, loss : LossFunction, f : int -> scalar[] -> unit) =
            let scalarResCount = count * blockSize
            let c = blocks |> Array.map (fun p -> p.Count * p.DoublesPerElement)
            let s = c |> Array.map (fun c -> sizeof<double> * c |> nativeint)
            let res : scalar[] = Array.zeroCreate blockSize

            let o = 
                let r = Array.zeroCreate c.Length
                let mutable o = 0
                for i in 0 .. c.Length - 1 do
                    r.[i] <- o
                    o <- o + c.[i]
                r
               
            let findBlock (c : int) =
                let mutable i = 0
                let mutable block = -1
                while block < 0 && i < o.Length do
                    let n = if i < o.Length - 1 then o.[i+1] else System.Int32.MaxValue
                    if c < n then block <- i
                    i <- i + 1

                (block, c - o.[block])

            blocks |> Array.iter x.AddBlock

            let evaluate (parameters : nativeptr<nativeptr<float>>, residuals : nativeptr<float>, jacobians : nativeptr<nativeptr<float>>) =
                if NativePtr.isNull jacobians then
                    for i in 0 .. c.Length - 1 do
                        readBlock.[i] o.[i] false (NativePtr.get parameters i)
                   
                    let mutable ri = 0
                    for g in 0 .. count - 1 do
                        f g res
                        for r in res do
                            NativePtr.set residuals ri r.Value
                            ri <- ri + 1
                else
                    let j = Array.init c.Length (NativePtr.get jacobians)
                    let h = j |> Array.map (fun j -> not (NativePtr.isNull j))
                    
                    for i in 0 .. c.Length - 1 do
                        let read = readBlock.[i]
                        read o.[i] h.[i] (NativePtr.get parameters i)
                        if h.[i] then System.Runtime.InteropServices.Marshal.Set(NativePtr.toNativeInt j.[i], 0, s.[i] * nativeint scalarResCount)
                        
                    let mutable ri = 0
                    for g in 0 .. count - 1 do
                        f g res
                        for r in res do
                            NativePtr.set residuals ri r.Value

                            for (KeyValue(c,v)) in r.Jacobian do
                                let (b, i) = findBlock c
                                if h.[b] then NativePtr.set j.[b] i v
                             
                            for i in 0 .. c.Length - 1 do
                                if h.[i] then j.[i] <- NativePtr.add j.[i] c.[i] 
                            ri <- ri + 1
                1

            x.AddCostFunction(
                Array.map2 (fun c (p : Block) -> c, p.Pointer) c blocks, 
                scalarResCount, 
                loss, 
                evaluate, 
                id
            )

        member x.AddCostFunction(residualCount : int, blocks : array<Block>, readBlock : array<int -> bool -> nativeptr<float> -> unit>, loss : LossFunction, f : scalar[] -> unit) =
            x.AddCostFunction(
                1, residualCount,
                blocks, readBlock,
                loss,
                fun _ res -> f res
            )