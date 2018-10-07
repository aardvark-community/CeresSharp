namespace CeresSharp

open Aardvark.Base
open CeresSharp
open Microsoft.FSharp.NativeInterop

#nowarn "9"

[<AutoOpen>]
module ProblemExtensions =
    type Problem with   
        member x.AddParameterBlock(data : float[]) =
            let block = new ParameterBlock<float, scalar>(data, 1, fun i v -> scalar.Variable(i,v))
            block :> IParameterBlock<float, scalar>   

        member x.AddParameterBlock(data : V2d[]) =
            let read (offset : int) (v : V2d) =
                V2s(scalar.Variable(offset, v.X), scalar.Variable(offset + 1, v.Y))
            let block = new ParameterBlock<V2d, V2s>(data, 2, read)
            block :> IParameterBlock<_, _>   
            
        member x.AddParameterBlock(data : V3d[]) =
            let read (offset : int) (v : V3d) =
                V3s(scalar.Variable(offset, v.X), scalar.Variable(offset + 1, v.Y), scalar.Variable(offset + 2, v.Z))
            let block = new ParameterBlock<V3d, V3s>(data, 3, read)
            block :> IParameterBlock<_, _>   

        member x.AddParameterBlock(data : V4d[]) =
            let read (offset : int) (v : V4d) =
                V4s(scalar.Variable(offset, v.X), scalar.Variable(offset + 1, v.Y), scalar.Variable(offset + 2, v.Z), scalar.Variable(offset + 3, v.W))
            let block = new ParameterBlock<V4d, V4s>(data, 4, read)
            block :> IParameterBlock<_, _>   
            
        member inline x.AddParameterBlock< ^a, ^b when ^a : unmanaged and ^b : (static member Read : int * ^a -> ^b) and ^b : (static member Doubles : int)> (data : ^a[]) : IParameterBlock< ^a, ^b > =
            let doubles = (^b : (static member Doubles : int) ())
            let read (offset : int) (v : ^a) = (^b : (static member Read : int * ^a -> ^b) (offset, v))
            let block = new ParameterBlock< ^a, ^b >(data, doubles, read)
            block :> IParameterBlock<_, _>

        member x.AddCostFunction(residualCount : int, p0 : IParameterBlock<'b>, loss : LossFunction, f : 'b[] -> scalar[]) =
            let c0 = p0.DoubleCount
            let evaluate (parameters : nativeptr<nativeptr<float>>, residuals : nativeptr<float>, jacobians : nativeptr<nativeptr<float>>) =
                let parameters = NativePtr.read parameters
                let jacobians = if NativePtr.isNull jacobians then NativePtr.zero else NativePtr.read jacobians
                if NativePtr.isNull jacobians then
                    let args = p0.Read(0, parameters)
                    let res = f args
                    for i in 0 .. residualCount - 1 do
                        NativePtr.set residuals i res.[i].Value
                else
                    let args = p0.Read(0, parameters)
                    let res = f args
                    for i in 0 .. residualCount - 1 do
                        NativePtr.set residuals i res.[i].Value

                        for pi in 0 .. c0 - 1 do
                            let value = 
                                match Map.tryFind pi res.[i].Jacobian with
                                    | Some v -> v
                                    | None -> 0.0

                            let index = i * c0 + pi
                            NativePtr.set jacobians index value
                                
                1

            x.AddCostFunction([| c0 |], residualCount, loss, evaluate, id, [p0.Pointer])

        member x.AddCostFunction(residualCount : int, p0 : IParameterBlock<'b>, p1 : IParameterBlock<'c>, loss : LossFunction, f : 'b[] -> 'c[] -> scalar[]) =
            let c0 = p0.DoubleCount
            let c1 = p1.DoubleCount
            let evaluate (parameters : nativeptr<nativeptr<float>>, residuals : nativeptr<float>, jacobians : nativeptr<nativeptr<float>>) =
                if NativePtr.isNull jacobians then
                    let a0 = p0.Read(0, NativePtr.get parameters 0)
                    let a1 = p1.Read(c0, NativePtr.get parameters 1)
                    let res = f a0 a1
                    for i in 0 .. residualCount - 1 do
                        NativePtr.set residuals i res.[i].Value
                else
                    let a0 = p0.Read(0, NativePtr.get parameters 0)
                    let a1 = p1.Read(c0, NativePtr.get parameters 1)

                    let res = f a0 a1
                    for i in 0 .. residualCount - 1 do
                        NativePtr.set residuals i res.[i].Value

                        let d0, d1 = Map.partition (fun k v -> k < c0) res.[i].Jacobian
                       

                        let j = NativePtr.get jacobians 0
                        if not (NativePtr.isNull j) then
                            for pi in 0 .. c0 - 1 do
                                let value = 
                                    match Map.tryFind pi d0 with
                                        | Some v -> v
                                        | None -> 0.0

                                let index = i * c0 + pi
                                NativePtr.set j index value
                                
                        let j = NativePtr.get jacobians 1
                        if not (NativePtr.isNull j) then
                            for pi in 0 .. c1 - 1 do
                                let value = 
                                    match Map.tryFind (pi + c0) d1 with
                                        | Some v -> v
                                        | None -> 0.0

                                let index = i * c1 + pi
                                NativePtr.set j index value

                1

            x.AddCostFunction([|c0; c1|], residualCount, loss, evaluate, id, [p0.Pointer; p1.Pointer])

        member x.AddCostFunction(residualCount : int, p0 : IParameterBlock<'b>, p1 : IParameterBlock<'c>, p2 : IParameterBlock<'d>, loss : LossFunction, f : 'b[] -> 'c[] -> 'd[] -> scalar[]) =
            let c0 = p0.DoubleCount
            let c1 = p1.DoubleCount
            let c2 = p2.DoubleCount
            let evaluate (parameters : nativeptr<nativeptr<float>>, residuals : nativeptr<float>, jacobians : nativeptr<nativeptr<float>>) =
                if NativePtr.isNull jacobians then
                    let a0 = p0.Read(0, NativePtr.get parameters 0)
                    let a1 = p1.Read(c0, NativePtr.get parameters 1)
                    let a2 = p2.Read(c1, NativePtr.get parameters 2)
                    let res = f a0 a1 a2
                    for i in 0 .. residualCount - 1 do
                        NativePtr.set residuals i res.[i].Value
                else
                    let a0 = p0.Read(0, NativePtr.get parameters 0)
                    let a1 = p1.Read(c0, NativePtr.get parameters 1)
                    let a2 = p2.Read(c1, NativePtr.get parameters 2)

                    let res = f a0 a1 a2
                    for i in 0 .. residualCount - 1 do
                        NativePtr.set residuals i res.[i].Value

                        let d0, rest = Map.partition (fun k v -> k < c0) res.[i].Jacobian
                        let d1, d2 = Map.partition (fun k v -> k < c1) rest

                        let j = NativePtr.get jacobians 0
                        if not (NativePtr.isNull j) then
                            for pi in 0 .. c0 - 1 do
                                let value = 
                                    match Map.tryFind pi d0 with
                                        | Some v -> v
                                        | None -> 0.0

                                let index = i * c0 + pi
                                NativePtr.set j index value
                                
                        let j = NativePtr.get jacobians 1
                        if not (NativePtr.isNull j) then
                            for pi in 0 .. c1 - 1 do
                                let value = 
                                    match Map.tryFind (pi + c0) d1 with
                                        | Some v -> v
                                        | None -> 0.0

                                let index = i * c1 + pi
                                NativePtr.set j index value

                        let j = NativePtr.get jacobians 2
                        if not (NativePtr.isNull j) then
                            for pi in 0 .. c2 - 1 do
                                let value = 
                                    match Map.tryFind (pi + c0 + c1) d2 with
                                        | Some v -> v
                                        | None -> 0.0

                                let index = i * c2 + pi
                                NativePtr.set j index value

                1

            x.AddCostFunction([|c0; c1; c2|], residualCount, loss, evaluate, id, [p0.Pointer; p1.Pointer; p2.Pointer])

        member x.AddCostFunction(residualCount : int, p0 : IParameterBlock<'b>, f : 'b[] -> scalar[]) =
            x.AddCostFunction(residualCount, p0, TrivialLoss, f)
            
        member x.AddCostFunction(residualCount : int, p0 : IParameterBlock<'b>, p1 : IParameterBlock<'c>, f : 'b[] -> 'c[] -> scalar[]) =
            x.AddCostFunction(residualCount, p0, p1, TrivialLoss, f)
        
        member x.AddCostFunction(residualCount : int, p0 : IParameterBlock<'b>, p1 : IParameterBlock<'c>, p2 : IParameterBlock<'d>, f : 'b[] -> 'c[] -> 'd[] -> scalar[]) =
            x.AddCostFunction(residualCount, p0, p1, p2, TrivialLoss, f)
            