namespace Aardvark.Reconstruction

open Aardvark.Base

    
type Edge<'w> = { i0 : int; i1 : int; weight : 'w }

module Edge =
    let map (f : 'w -> 'v) (e : Edge<'w>) : Edge<'v> = { i0 = e.i0; i1 = e.i1; weight = f e.weight }
    
type Graph<'w> = { edges : list<Edge<'w>> }

type RoseTree<'a> =
    | Empty
    | Leaf of 'a
    | Node of 'a * list<RoseTree<'a>>


module RoseTree =
    let rec map (f : 'a -> 'b) (t : RoseTree<'a>) =
        match t with
            | Empty -> Empty
            | Leaf a -> Leaf (f a)
            | Node(a,c) -> Node(f a, List.map (map f) c)
    
    let rec toList (t : RoseTree<'a>) =
        match t with
            | Empty ->      []
            | Leaf a ->     [a]
            | Node(a,c) ->  a :: (c |> List.collect toList)

module Graph =
    open System.Collections.Generic
    open Aardvark.Base.GridCell

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
            and set (v : UnionNode) = 
                parent <- v

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
                
        let edges = CSharpList.toArray result
        
        let neighbours = Dict<int,HashSet<int>>()

        edges |> Array.iter ( fun e ->
                    let lns = neighbours.GetOrCreate(e.i0, (fun _ -> HashSet())) 
                    lns.Add e.i1 |> ignore

                    let rns = neighbours.GetOrCreate(e.i1, (fun _ -> HashSet())) 
                    rns.Add e.i0 |> ignore
                    )

        if neighbours.Count = 0 then
            Empty, edges
        else
            let root = neighbours.Keys |> Seq.head

            let rec traverse (edgeCount : ref<int>) (visited : HashSet<int>) (n : int) =
                if visited.Add n then
                    let neighbours = neighbours.[n]

                    let children = 
                        neighbours 
                            |> Seq.toList 
                            |> List.choose (fun ri -> 
                                match traverse edgeCount visited ri with
                                    | Some r -> 
                                        edgeCount := !edgeCount + 1
                                        Some r
                                    | None -> 
                                        None
                            )

                    match children with
                    | [] -> Leaf n |> Some
                    | _ -> Node(n, children) |> Some
                else
                    None

            match traverse (ref 0) (HashSet()) root with
            | Some t -> t, edges
            | None -> Empty, edges
        
module Ray3d =
    let avg (rays : list<Ray3d>) =
        match rays with
        | [] | [_] -> None
        | _ ->
            let (M, r) = 
                rays    |> List.collect
                            ( fun r -> 
                                let nx = r.Direction.AxisAlignedNormal()
                                let ny = r.Direction.Cross nx |> Vec.normalize
                                let o = r.Origin
                                [nx, nx.Dot o; ny, ny.Dot o]
                            )
                        |> List.map
                            ( fun (n,d) ->
                                n.OuterProduct n, d * n
                            )
                        |> List.fold ( fun (ms,vs) (m,v) -> (ms+m),(vs+v) ) (M33d.Zero,V3d.Zero)
                
            let p = (M.Inverse * r)
            let ds = rays |> List.map ( fun r -> r.GetMinimalDistanceTo p )
            Some p