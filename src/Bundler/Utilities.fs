namespace Aardvark.Ceres
open System
open Aardvark.Base
open CeresSharp

module Array =
    let filteri (f : int -> 'a -> bool) (a : 'a[]) =
        let res = System.Collections.Generic.List()
        for i in 0 .. a.Length - 1 do
            if f i a.[i] then
                res.Add(a.[i])
        res.ToArray()

    let choosei (f : int -> 'a -> Option<'b>) (a : 'a[]) =
        let res = System.Collections.Generic.List()
        for i in 0 .. a.Length - 1 do
            match f i a.[i] with
                | Some b -> res.Add b
                | _ -> ()
        res.ToArray()


module Map =
    let intersect (l : Map<'k, 'a>) (r : Map<'k, 'b>) =
        let mutable res = Map.empty
        for (k,l) in Map.toSeq l do
            match Map.tryFind k r with
                | Some r ->
                    res <- Map.add k (l,r) res
                | None ->
                    ()
        res 


module PointCloud =

    let trafo (source : V3d[]) (target : V3d[]) =
        use p = new Problem()

        let b = p.AddParameterBlock([| V4d.IOOO; V4d.OIOO; V4d.OOIO; V4d.OOOI |])

        let res = Array.zeroCreate (3 * source.Length)
        p.AddCostFunction(res.Length, b, fun rows ->
            let mutable oi = 0

            for i in 0 .. source.Length - 1 do  
                let src = source.[i]
                let dst = target.[i]


                let src4 = V4s(src.X, src.Y, src.Z, 1.0)
                let test = V4s(V4s.Dot(rows.[0], src4), V4s.Dot(rows.[1], src4), V4s.Dot(rows.[2], src4), V4s.Dot(rows.[3], src4))
                let r = test.XYZ / test.W - dst

                res.[oi + 0] <- r.X
                res.[oi + 1] <- r.Y
                res.[oi + 2] <- r.Z
                oi <- oi + 3

            res
        )

        
        let cost = p.Solve(CeresOptions(4000, CeresSolverType.SparseSchur, false, 1.0E-15, 1.0E-15, 1.0E-15))
        if cost > 1.0E-4 then Log.warn "bad registration: %.5f" cost
        let res = b.Result
        let mat = M44d.FromRows(res.[0], res.[1], res.[2], res.[3])

        Trafo3d(mat, mat.Inverse)
  

