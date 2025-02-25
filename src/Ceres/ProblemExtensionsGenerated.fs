namespace CeresSharp
open Aardvark.Base
[<AutoOpen>]
module ``Problem CostFunction Extensions`` = 
    type Problem with
        member x.AddCostFunctionScalar(residualCount : int, p0 : Block<'b>, loss : LossFunction, cost : 'b[] -> scalar[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, loss, fun t ->
                cost a0 t
            )
        member x.AddCostFunctionScalar(residualCount : int, p0 : Block<'b>, cost : 'b[] -> scalar[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 t
            )
        member x.AddCostFunctionV2s(residualCount : int, p0 : Block<'b>, loss : LossFunction, cost : 'b[] -> V2s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(2 * residualCount, blocks, read, loss, fun t ->
                cost a0 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunctionV2s(residualCount : int, p0 : Block<'b>, cost : 'b[] -> V2s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(2 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunctionV3s(residualCount : int, p0 : Block<'b>, loss : LossFunction, cost : 'b[] -> V3s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(3 * residualCount, blocks, read, loss, fun t ->
                cost a0 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunctionV3s(residualCount : int, p0 : Block<'b>, cost : 'b[] -> V3s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(3 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunctionV4s(residualCount : int, p0 : Block<'b>, loss : LossFunction, cost : 'b[] -> V4s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(4 * residualCount, blocks, read, loss, fun t ->
                cost a0 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunctionV4s(residualCount : int, p0 : Block<'b>, cost : 'b[] -> V4s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(4 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, loss : LossFunction, cost : 'b[] -> scalar[]) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0
                for i in 0 .. residualCount - 1 do
                    t.[i] <- myRes.[i]
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, cost : 'b[] -> scalar[]) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0
                for i in 0 .. residualCount - 1 do
                    t.[i] <- myRes.[i]
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, loss : LossFunction, cost : 'b[] -> V2s[]) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            x.AddCostFunction(2 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, cost : 'b[] -> V2s[]) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            x.AddCostFunction(2 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, loss : LossFunction, cost : 'b[] -> V3s[]) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            x.AddCostFunction(3 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, cost : 'b[] -> V3s[]) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            x.AddCostFunction(3 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, loss : LossFunction, cost : 'b[] -> V4s[]) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            x.AddCostFunction(4 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, cost : 'b[] -> V4s[]) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            x.AddCostFunction(4 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, loss : LossFunction, cost : 'b[] -> int -> scalar) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            x.AddCostFunction(residualCount, 1, blocks, read, loss, fun i t ->
                let v = cost a0 i
                t.[0] <- v
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, cost : 'b[] -> int -> scalar) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            x.AddCostFunction(residualCount, 1, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 i
                t.[0] <- v
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, loss : LossFunction, cost : 'b[] -> int -> V2s) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            x.AddCostFunction(residualCount, 2, blocks, read, loss, fun i t ->
                let v = cost a0 i
                t.[0] <- v.X
                t.[1] <- v.Y
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, cost : 'b[] -> int -> V2s) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            x.AddCostFunction(residualCount, 2, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 i
                t.[0] <- v.X
                t.[1] <- v.Y
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, loss : LossFunction, cost : 'b[] -> int -> V3s) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            x.AddCostFunction(residualCount, 3, blocks, read, loss, fun i t ->
                let v = cost a0 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, cost : 'b[] -> int -> V3s) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            x.AddCostFunction(residualCount, 3, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, loss : LossFunction, cost : 'b[] -> int -> V4s) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            x.AddCostFunction(residualCount, 4, blocks, read, loss, fun i t ->
                let v = cost a0 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
                t.[3] <- v.W
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, cost : 'b[] -> int -> V4s) =
            let a0 = Array.zeroCreate p0.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                |]
            let blocks =
                [|
                    p0 :> Block
                |]
            x.AddCostFunction(residualCount, 4, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
                t.[3] <- v.W
            )
        member x.AddCostFunctionScalar(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, loss : LossFunction, cost : 'b[] -> 'c[] -> scalar[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 t
            )
        member x.AddCostFunctionScalar(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, cost : 'b[] -> 'c[] -> scalar[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 t
            )
        member x.AddCostFunctionV2s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, loss : LossFunction, cost : 'b[] -> 'c[] -> V2s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(2 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunctionV2s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, cost : 'b[] -> 'c[] -> V2s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(2 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunctionV3s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, loss : LossFunction, cost : 'b[] -> 'c[] -> V3s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(3 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunctionV3s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, cost : 'b[] -> 'c[] -> V3s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(3 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunctionV4s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, loss : LossFunction, cost : 'b[] -> 'c[] -> V4s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(4 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunctionV4s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, cost : 'b[] -> 'c[] -> V4s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(4 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, loss : LossFunction, cost : 'b[] -> 'c[] -> scalar[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1
                for i in 0 .. residualCount - 1 do
                    t.[i] <- myRes.[i]
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, cost : 'b[] -> 'c[] -> scalar[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1
                for i in 0 .. residualCount - 1 do
                    t.[i] <- myRes.[i]
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, loss : LossFunction, cost : 'b[] -> 'c[] -> V2s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            x.AddCostFunction(2 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, cost : 'b[] -> 'c[] -> V2s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            x.AddCostFunction(2 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, loss : LossFunction, cost : 'b[] -> 'c[] -> V3s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            x.AddCostFunction(3 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, cost : 'b[] -> 'c[] -> V3s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            x.AddCostFunction(3 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, loss : LossFunction, cost : 'b[] -> 'c[] -> V4s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            x.AddCostFunction(4 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, cost : 'b[] -> 'c[] -> V4s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            x.AddCostFunction(4 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, loss : LossFunction, cost : 'b[] -> 'c[] -> int -> scalar) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            x.AddCostFunction(residualCount, 1, blocks, read, loss, fun i t ->
                let v = cost a0 a1 i
                t.[0] <- v
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, cost : 'b[] -> 'c[] -> int -> scalar) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            x.AddCostFunction(residualCount, 1, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 i
                t.[0] <- v
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, loss : LossFunction, cost : 'b[] -> 'c[] -> int -> V2s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            x.AddCostFunction(residualCount, 2, blocks, read, loss, fun i t ->
                let v = cost a0 a1 i
                t.[0] <- v.X
                t.[1] <- v.Y
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, cost : 'b[] -> 'c[] -> int -> V2s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            x.AddCostFunction(residualCount, 2, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 i
                t.[0] <- v.X
                t.[1] <- v.Y
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, loss : LossFunction, cost : 'b[] -> 'c[] -> int -> V3s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            x.AddCostFunction(residualCount, 3, blocks, read, loss, fun i t ->
                let v = cost a0 a1 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, cost : 'b[] -> 'c[] -> int -> V3s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            x.AddCostFunction(residualCount, 3, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, loss : LossFunction, cost : 'b[] -> 'c[] -> int -> V4s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            x.AddCostFunction(residualCount, 4, blocks, read, loss, fun i t ->
                let v = cost a0 a1 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
                t.[3] <- v.W
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, cost : 'b[] -> 'c[] -> int -> V4s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                |]
            x.AddCostFunction(residualCount, 4, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
                t.[3] <- v.W
            )
        member x.AddCostFunctionScalar(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> scalar[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 a2 t
            )
        member x.AddCostFunctionScalar(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, cost : 'b[] -> 'c[] -> 'd[] -> scalar[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 a2 t
            )
        member x.AddCostFunctionV2s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> V2s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(2 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 a2 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunctionV2s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, cost : 'b[] -> 'c[] -> 'd[] -> V2s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(2 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 a2 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunctionV3s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> V3s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(3 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 a2 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunctionV3s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, cost : 'b[] -> 'c[] -> 'd[] -> V3s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(3 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 a2 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunctionV4s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> V4s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(4 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 a2 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunctionV4s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, cost : 'b[] -> 'c[] -> 'd[] -> V4s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(4 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 a2 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> scalar[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1 a2
                for i in 0 .. residualCount - 1 do
                    t.[i] <- myRes.[i]
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, cost : 'b[] -> 'c[] -> 'd[] -> scalar[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1 a2
                for i in 0 .. residualCount - 1 do
                    t.[i] <- myRes.[i]
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> V2s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            x.AddCostFunction(2 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1 a2
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, cost : 'b[] -> 'c[] -> 'd[] -> V2s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            x.AddCostFunction(2 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1 a2
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> V3s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            x.AddCostFunction(3 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1 a2
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, cost : 'b[] -> 'c[] -> 'd[] -> V3s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            x.AddCostFunction(3 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1 a2
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> V4s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            x.AddCostFunction(4 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1 a2
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, cost : 'b[] -> 'c[] -> 'd[] -> V4s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            x.AddCostFunction(4 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1 a2
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> int -> scalar) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            x.AddCostFunction(residualCount, 1, blocks, read, loss, fun i t ->
                let v = cost a0 a1 a2 i
                t.[0] <- v
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, cost : 'b[] -> 'c[] -> 'd[] -> int -> scalar) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            x.AddCostFunction(residualCount, 1, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 a2 i
                t.[0] <- v
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> int -> V2s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            x.AddCostFunction(residualCount, 2, blocks, read, loss, fun i t ->
                let v = cost a0 a1 a2 i
                t.[0] <- v.X
                t.[1] <- v.Y
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, cost : 'b[] -> 'c[] -> 'd[] -> int -> V2s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            x.AddCostFunction(residualCount, 2, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 a2 i
                t.[0] <- v.X
                t.[1] <- v.Y
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> int -> V3s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            x.AddCostFunction(residualCount, 3, blocks, read, loss, fun i t ->
                let v = cost a0 a1 a2 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, cost : 'b[] -> 'c[] -> 'd[] -> int -> V3s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            x.AddCostFunction(residualCount, 3, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 a2 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> int -> V4s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            x.AddCostFunction(residualCount, 4, blocks, read, loss, fun i t ->
                let v = cost a0 a1 a2 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
                t.[3] <- v.W
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, cost : 'b[] -> 'c[] -> 'd[] -> int -> V4s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                |]
            x.AddCostFunction(residualCount, 4, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 a2 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
                t.[3] <- v.W
            )
        member x.AddCostFunctionScalar(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> scalar[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 a2 a3 t
            )
        member x.AddCostFunctionScalar(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> scalar[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 a2 a3 t
            )
        member x.AddCostFunctionV2s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> V2s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(2 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 a2 a3 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunctionV2s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> V2s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(2 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 a2 a3 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunctionV3s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> V3s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(3 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 a2 a3 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunctionV3s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> V3s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(3 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 a2 a3 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunctionV4s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> V4s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(4 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 a2 a3 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunctionV4s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> V4s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(4 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 a2 a3 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> scalar[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1 a2 a3
                for i in 0 .. residualCount - 1 do
                    t.[i] <- myRes.[i]
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> scalar[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1 a2 a3
                for i in 0 .. residualCount - 1 do
                    t.[i] <- myRes.[i]
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> V2s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            x.AddCostFunction(2 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1 a2 a3
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> V2s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            x.AddCostFunction(2 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1 a2 a3
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> V3s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            x.AddCostFunction(3 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1 a2 a3
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> V3s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            x.AddCostFunction(3 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1 a2 a3
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> V4s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            x.AddCostFunction(4 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1 a2 a3
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> V4s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            x.AddCostFunction(4 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1 a2 a3
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> int -> scalar) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            x.AddCostFunction(residualCount, 1, blocks, read, loss, fun i t ->
                let v = cost a0 a1 a2 a3 i
                t.[0] <- v
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> int -> scalar) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            x.AddCostFunction(residualCount, 1, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 a2 a3 i
                t.[0] <- v
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> int -> V2s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            x.AddCostFunction(residualCount, 2, blocks, read, loss, fun i t ->
                let v = cost a0 a1 a2 a3 i
                t.[0] <- v.X
                t.[1] <- v.Y
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> int -> V2s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            x.AddCostFunction(residualCount, 2, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 a2 a3 i
                t.[0] <- v.X
                t.[1] <- v.Y
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> int -> V3s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            x.AddCostFunction(residualCount, 3, blocks, read, loss, fun i t ->
                let v = cost a0 a1 a2 a3 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> int -> V3s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            x.AddCostFunction(residualCount, 3, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 a2 a3 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> int -> V4s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            x.AddCostFunction(residualCount, 4, blocks, read, loss, fun i t ->
                let v = cost a0 a1 a2 a3 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
                t.[3] <- v.W
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> int -> V4s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                |]
            x.AddCostFunction(residualCount, 4, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 a2 a3 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
                t.[3] <- v.W
            )
        member x.AddCostFunctionScalar(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> scalar[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 a2 a3 a4 t
            )
        member x.AddCostFunctionScalar(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> scalar[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 a2 a3 a4 t
            )
        member x.AddCostFunctionV2s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> V2s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(2 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 a2 a3 a4 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunctionV2s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> V2s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(2 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 a2 a3 a4 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunctionV3s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> V3s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(3 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 a2 a3 a4 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunctionV3s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> V3s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(3 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 a2 a3 a4 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunctionV4s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> V4s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(4 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 a2 a3 a4 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunctionV4s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> V4s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(4 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 a2 a3 a4 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> scalar[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4
                for i in 0 .. residualCount - 1 do
                    t.[i] <- myRes.[i]
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> scalar[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4
                for i in 0 .. residualCount - 1 do
                    t.[i] <- myRes.[i]
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> V2s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            x.AddCostFunction(2 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> V2s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            x.AddCostFunction(2 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> V3s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            x.AddCostFunction(3 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> V3s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            x.AddCostFunction(3 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> V4s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            x.AddCostFunction(4 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> V4s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            x.AddCostFunction(4 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> int -> scalar) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            x.AddCostFunction(residualCount, 1, blocks, read, loss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 i
                t.[0] <- v
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> int -> scalar) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            x.AddCostFunction(residualCount, 1, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 i
                t.[0] <- v
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> int -> V2s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            x.AddCostFunction(residualCount, 2, blocks, read, loss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 i
                t.[0] <- v.X
                t.[1] <- v.Y
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> int -> V2s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            x.AddCostFunction(residualCount, 2, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 i
                t.[0] <- v.X
                t.[1] <- v.Y
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> int -> V3s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            x.AddCostFunction(residualCount, 3, blocks, read, loss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> int -> V3s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            x.AddCostFunction(residualCount, 3, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> int -> V4s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            x.AddCostFunction(residualCount, 4, blocks, read, loss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
                t.[3] <- v.W
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> int -> V4s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                |]
            x.AddCostFunction(residualCount, 4, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
                t.[3] <- v.W
            )
        member x.AddCostFunctionScalar(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> scalar[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 a2 a3 a4 a5 t
            )
        member x.AddCostFunctionScalar(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> scalar[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 a2 a3 a4 a5 t
            )
        member x.AddCostFunctionV2s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> V2s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(2 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 a2 a3 a4 a5 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunctionV2s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> V2s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(2 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 a2 a3 a4 a5 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunctionV3s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> V3s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(3 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 a2 a3 a4 a5 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunctionV3s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> V3s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(3 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 a2 a3 a4 a5 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunctionV4s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> V4s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(4 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 a2 a3 a4 a5 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunctionV4s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> V4s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(4 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 a2 a3 a4 a5 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> scalar[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4 a5
                for i in 0 .. residualCount - 1 do
                    t.[i] <- myRes.[i]
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> scalar[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4 a5
                for i in 0 .. residualCount - 1 do
                    t.[i] <- myRes.[i]
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> V2s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            x.AddCostFunction(2 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4 a5
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> V2s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            x.AddCostFunction(2 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4 a5
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> V3s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            x.AddCostFunction(3 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4 a5
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> V3s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            x.AddCostFunction(3 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4 a5
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> V4s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            x.AddCostFunction(4 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4 a5
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> V4s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            x.AddCostFunction(4 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4 a5
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> int -> scalar) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            x.AddCostFunction(residualCount, 1, blocks, read, loss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 a5 i
                t.[0] <- v
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> int -> scalar) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            x.AddCostFunction(residualCount, 1, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 a5 i
                t.[0] <- v
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> int -> V2s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            x.AddCostFunction(residualCount, 2, blocks, read, loss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 a5 i
                t.[0] <- v.X
                t.[1] <- v.Y
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> int -> V2s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            x.AddCostFunction(residualCount, 2, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 a5 i
                t.[0] <- v.X
                t.[1] <- v.Y
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> int -> V3s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            x.AddCostFunction(residualCount, 3, blocks, read, loss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 a5 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> int -> V3s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            x.AddCostFunction(residualCount, 3, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 a5 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> int -> V4s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            x.AddCostFunction(residualCount, 4, blocks, read, loss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 a5 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
                t.[3] <- v.W
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> int -> V4s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                |]
            x.AddCostFunction(residualCount, 4, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 a5 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
                t.[3] <- v.W
            )
        member x.AddCostFunctionScalar(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> scalar[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 a2 a3 a4 a5 a6 t
            )
        member x.AddCostFunctionScalar(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> scalar[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 a2 a3 a4 a5 a6 t
            )
        member x.AddCostFunctionV2s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> V2s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(2 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 a2 a3 a4 a5 a6 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunctionV2s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> V2s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(2 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 a2 a3 a4 a5 a6 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunctionV3s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> V3s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(3 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 a2 a3 a4 a5 a6 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunctionV3s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> V3s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(3 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 a2 a3 a4 a5 a6 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunctionV4s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> V4s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(4 * residualCount, blocks, read, loss, fun t ->
                cost a0 a1 a2 a3 a4 a5 a6 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunctionV4s(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> V4s[] -> unit) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            let myRes = Array.zeroCreate residualCount
            x.AddCostFunction(4 * residualCount, blocks, read, TrivialLoss, fun t ->
                cost a0 a1 a2 a3 a4 a5 a6 myRes
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> scalar[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4 a5 a6
                for i in 0 .. residualCount - 1 do
                    t.[i] <- myRes.[i]
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> scalar[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            x.AddCostFunction(1 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4 a5 a6
                for i in 0 .. residualCount - 1 do
                    t.[i] <- myRes.[i]
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> V2s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            x.AddCostFunction(2 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4 a5 a6
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> V2s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            x.AddCostFunction(2 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4 a5 a6
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    ri <- ri + 2
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> V3s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            x.AddCostFunction(3 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4 a5 a6
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> V3s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            x.AddCostFunction(3 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4 a5 a6
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    ri <- ri + 3
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> V4s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            x.AddCostFunction(4 * residualCount, blocks, read, loss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4 a5 a6
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> V4s[]) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            x.AddCostFunction(4 * residualCount, blocks, read, TrivialLoss, fun t ->
                let myRes = cost a0 a1 a2 a3 a4 a5 a6
                let mutable ri = 0
                for i in 0 .. residualCount - 1 do
                    t.[ri + 0] <- myRes.[i].X
                    t.[ri + 1] <- myRes.[i].Y
                    t.[ri + 2] <- myRes.[i].Z
                    t.[ri + 3] <- myRes.[i].W
                    ri <- ri + 4
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> int -> scalar) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            x.AddCostFunction(residualCount, 1, blocks, read, loss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 a5 a6 i
                t.[0] <- v
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> int -> scalar) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            x.AddCostFunction(residualCount, 1, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 a5 a6 i
                t.[0] <- v
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> int -> V2s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            x.AddCostFunction(residualCount, 2, blocks, read, loss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 a5 a6 i
                t.[0] <- v.X
                t.[1] <- v.Y
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> int -> V2s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            x.AddCostFunction(residualCount, 2, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 a5 a6 i
                t.[0] <- v.X
                t.[1] <- v.Y
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> int -> V3s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            x.AddCostFunction(residualCount, 3, blocks, read, loss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 a5 a6 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> int -> V3s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            x.AddCostFunction(residualCount, 3, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 a5 a6 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, loss : LossFunction, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> int -> V4s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            x.AddCostFunction(residualCount, 4, blocks, read, loss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 a5 a6 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
                t.[3] <- v.W
            )
        member x.AddCostFunction(residualCount : int, p0 : Block<'b>, p1 : Block<'c>, p2 : Block<'d>, p3 : Block<'e>, p4 : Block<'f>, p5 : Block<'g>, p6 : Block<'h>, cost : 'b[] -> 'c[] -> 'd[] -> 'e[] -> 'f[] -> 'g[] -> 'h[] -> int -> V4s) =
            let a0 = Array.zeroCreate p0.Count
            let a1 = Array.zeroCreate p1.Count
            let a2 = Array.zeroCreate p2.Count
            let a3 = Array.zeroCreate p3.Count
            let a4 = Array.zeroCreate p4.Count
            let a5 = Array.zeroCreate p5.Count
            let a6 = Array.zeroCreate p6.Count
            let read =
                [|
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p0.Read(o, ptr, d, a0)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p1.Read(o, ptr, d, a1)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p2.Read(o, ptr, d, a2)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p3.Read(o, ptr, d, a3)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p4.Read(o, ptr, d, a4)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p5.Read(o, ptr, d, a5)
                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p6.Read(o, ptr, d, a6)
                |]
            let blocks =
                [|
                    p0 :> Block
                    p1 :> Block
                    p2 :> Block
                    p3 :> Block
                    p4 :> Block
                    p5 :> Block
                    p6 :> Block
                |]
            x.AddCostFunction(residualCount, 4, blocks, read, TrivialLoss, fun i t ->
                let v = cost a0 a1 a2 a3 a4 a5 a6 i
                t.[0] <- v.X
                t.[1] <- v.Y
                t.[2] <- v.Z
                t.[3] <- v.W
            )
