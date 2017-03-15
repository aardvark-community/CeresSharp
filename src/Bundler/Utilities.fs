namespace Aardvark.Ceres
open System
open Aardvark.Base
open CeresSharp

module Array =
    let private parOpt = System.Threading.Tasks.ParallelOptions(MaxDegreeOfParallelism = Environment.ProcessorCount)

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

    let mapParallel (f : 'a -> 'b) (input : 'a[]) =
        parOpt.MaxDegreeOfParallelism <- Environment.ProcessorCount
        let res = Array.zeroCreate input.Length
        System.Threading.Tasks.Parallel.For(0, input.Length, parOpt, fun i -> 
            res.[i] <- f input.[i]
        ) |> ignore

        res

    let chooseParallel (f : 'a -> Option<'b>) (input : 'a[]) =
        parOpt.MaxDegreeOfParallelism <- Environment.ProcessorCount
        let res = Array.zeroCreate input.Length
        System.Threading.Tasks.Parallel.For(0, input.Length, parOpt, fun i -> 
            res.[i] <- f input.[i]
        ) |> ignore

        res |> Array.choose id

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
  
    let trafo2 (source : Map<int, V3d>) (target : Map<int, V3d>) =
        let s, t = Map.intersect source target |> Map.toSeq |> Seq.map snd |> Seq.toArray |> Array.unzip
        trafo s t



type Edge<'w> = { i0 : int; i1 : int; weight : 'w }
    
type Graph<'w> = { edges : list<Edge<'w>> }

module Graph =
    open System.Collections.Generic

    type UnionNode(id : int) as this =
        let mutable parent = this
        let mutable rank = 1

        override x.GetHashCode() = id
        override x.Equals o =
            match o with
                | :? UnionNode as o -> id = o.Id
                | _ -> false

        member x.Id = id

        member x.Parent 
            with get() = parent
            and set v = parent <- v

        member x.Rank 
            with get() = rank
            and set v = rank <- v

    type UnionFind() =
        let nodes = Dict<int, UnionNode>()

        let node (i : int) =
            nodes.GetOrCreate(i, fun i -> UnionNode(i)) 

        let rec find (n : UnionNode) =
            if n.Parent <> n then
                let res = find n.Parent
                n.Parent <- res
                res
            else
                n

        let rec union (l : UnionNode) (r : UnionNode) =
            let l = find l
            let r = find r

            if l = r then 
                false
            else
                if l.Rank < r.Rank then
                    l.Parent <- r
                    true
                elif r.Rank < l.Rank then
                    r.Parent <- l
                    true
                else
                    r.Parent <- l
                    l.Rank <- l.Rank + 1
                    true
                  
        member x.Add(l : int, r : int) =
            let l = node l
            let r = node r
            union l r      

    let ofEdges (edges : list<Edge<'w>>) =
        { edges = edges }

    let minimumSpanningTree (cmp : 'w -> 'w -> int) (g : Graph<'w>) =
        let edges = List.toArray g.edges
        edges.QuickSort(fun e0 e1 -> cmp e0.weight e1.weight )

        let uf = UnionFind()

        let result = List<Edge<'w>>()
        for e in edges do
            if uf.Add(e.i0, e.i1) then
                result.Add e

        CSharpList.toArray result


