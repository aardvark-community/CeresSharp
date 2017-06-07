namespace Aardvark.Reconstruction

open System
open Aardvark.Base

open System.Collections.Generic
module Match =
    open Aardvark.Base.Tree

    module internal Stateful =

        //unsafe because FeatureNodes have to have the correct state.
        //todo: make FeatureNode stateless
        let mkTracksFromCorrespondences (cameras : MapExt<CameraId, list<FeatureNode>>) =
            
            let used = Dict<CameraId*FeatureNode, bool>()
            let isUsed ci (f : FeatureNode) = used.[ci,f]
            let setUsed ci (f : FeatureNode) = used |> Dict.set (ci,f) true
            let neighborsOf (f : FeatureNode) = (f.corresponding |> Dict.toList)

            let rec checkNeighbors (path : list<CameraId * FeatureNode>) (neighbors : list<CameraId*HashSet<FeatureNode>>) =

                match neighbors with                                    //check corresponding neighbouring images
                | [] -> path                                            //no more neighbours, we're finished with our path
                | (corImg, cor)::remainingNeighbours ->                 //has neighbours, take the first one
            
                    let rec findUnusedMatch (cor : list<FeatureNode>) =
                        match cor with
                        | [] -> None
                        | corFtr::remainingFtrs ->
                            if isUsed corImg corFtr then
                                findUnusedMatch remainingFtrs
                            else
                                setUsed corImg corFtr
                                Some corFtr

                    match findUnusedMatch (cor |> HashSet.toList) with  //check if this neighbour has an unused feature match
                    | None ->                                           //doesn't have unused feature, try the next neighbour
                        checkNeighbors path remainingNeighbours
                    | Some m ->                                                     //has an unused feature. this is part of the result.
                        let deepPath = checkNeighbors [] (neighborsOf m)            //first, collect all subresults from the neighbours
                        checkNeighbors ([[corImg,m]; deepPath; path] |> List.concat) remainingNeighbours   //then, append them to the current and continue with next neighbour

            for (cid, ftrs) in cameras |> MapExt.toList do for f in ftrs do used |> Dict.add (cid,f) false
            
            let result = List<list<CameraId * FeatureNode>>()

            for (cid, ftrs) in cameras |> MapExt.toList do
                for f in ftrs do
                    if not (isUsed cid f) then
                        setUsed cid f
                        let path = checkNeighbors [cid,f] (neighborsOf f)
                        result.Add path
            
            result |> Seq.toList
            
        //unsafe because FeatureNodes write their state
        let fillInCorrespondences (edges : list<Edge<list<FeatureNode * FeatureNode>>>) =

            let (tree,maximumEdges) = 
                Graph.ofEdges edges
                    |> Graph.minimumSpanningTree ( fun lWeight rWeight -> - compare lWeight.Length rWeight.Length )
            
            //printfn "[FillIn] bestEdges# = %A" (bestEdges |> Seq.length)

            //fill in correspondences
            for e in maximumEdges do
                for (lNode, rNode) in e.weight do
                    lNode.Add(CameraId(e.i1), rNode)
                    rNode.Add(CameraId(e.i0), lNode)

            (tree,maximumEdges)

    module GetMatches =
        
        let probabilityOnly bruteforceThresh lambda sigma probability l r =
            MatchProcs.bruteforce bruteforceThresh l r
            |> MatchProcs.probability lambda sigma probability
            |> List.map MatchProcs.FeatureMatch.toFeatureNodePair

        let probabilityAndAffine bruteforceThresh lambda sigma probability affineLambda affineSigma ndcMaxDiff l r =
            MatchProcs.bruteforce bruteforceThresh l r
            |> MatchProcs.probability lambda sigma probability
            |> MatchProcs.affine affineLambda affineSigma ndcMaxDiff
            |> List.map MatchProcs.FeatureMatch.toFeatureNodePair

    let mkTracks (cameras : MapExt<CameraId, list<FeatureNode>>) 
                 (getMatches : list<FeatureNode> -> list<FeatureNode> -> list<FeatureNode * FeatureNode>)
                 : MapExt<TrackId, MapExt<CameraId, V2d>> * Edges =

        let cids = cameras |> MapExt.toSeq |> Seq.map fst
        
        let allPairs =
            [
                for lcid in cids do
                    for rcid in cids do
                        if rcid.Id > lcid.Id then
                            yield (lcid, rcid)
            ]
        
        let allEdges =
            allPairs |> List.mapi ( fun nr (lcid, rcid) ->
                let matches = getMatches cameras.[lcid] cameras.[rcid]
                printfn "[MkTracks] matching pair %A of %A: (%A-%A) #%A Matches" nr (allPairs.Length - 1) lcid.Id rcid.Id matches.Length
                { i0 = lcid.Id; i1 = rcid.Id; weight = matches }
            )
        
        //first this
        let (tree,es) = Stateful.fillInCorrespondences allEdges

        //then this
        let paths = Stateful.mkTracksFromCorrespondences cameras
        
        printfn "[MkTracks] paths# = %A;; longerThanOne# = %A" paths.Length (paths |> List.filter ( fun p -> p.Length > 1 ) |> List.length)
        
        let cts = paths |> List.groupBy List.length
                        |> MapExt.ofList
                        |> MapExt.map ( fun _ v -> List.length v ) 

        printfn "[MkTracks] distribution (length,count) \n %A" cts

        //invent TrackIds
        let paths = paths |> List.mapi ( fun idx path -> TrackId(idx),path )
        
        let tids : Dict<FeatureNode * FeatureNode, HashSet<TrackId>> = Dict()

        Log.line "[MkTracks] Collecting edges ..."
        for (tid,path) in paths do
            for (lc,l) in path do
                for (rc,r) in path do
                    let k1 = (l,r)
                    let f = tids.GetOrCreate(k1,(fun _ -> HashSet<TrackId>()))
                    if f.Add(tid) && f.Count > 1 then Log.warn "[MkTracks] FeatureNodePair appears in different TrackIds! candidates=%A: %A" f.Count (f |> Seq.toList)
                    
                    let k2 = (r,l)
                    let f = tids.GetOrCreate(k2,(fun _ -> HashSet<TrackId>()))
                    if f.Add(tid) && f.Count > 1 then Log.warn "[MkTracks] FeatureNodePair appears in different TrackIds! candidates=%A: %A" f.Count (f |> Seq.toList)

        Log.line "[MkTracks] Collected edges, found# %A" tids.Count

        let p2 = paths |> List.toArray
        Log.line "[MkTracks] Overall Paths FeatureNodeCt# %A" (paths |> Seq.sumBy ( fun (_,path) -> path |> List.length))

        //for (tid,path) in paths do
        //    Log.line "Path %A - Count %A" tid path.Length
        
        Log.line "[MkTracks] Overall Edges Pairs count# %A" (es |> Array.sumBy (fun e -> e.weight.Length) )

        for e in es do
            Log.line "[MkTracks] Edge %A:%A - Pairs %A" e.i0 e.i1 e.weight.Length
        

        let edges = 
            es |> Array.toList
               |> List.map ( fun e -> 
                let weights = e.weight |> List.choose ( fun (l,r) ->
                                 tids |> Dict.tryFind (l,r) 
                                      |> Option.map ( fun tid ->
                                        tid |> Seq.head,(l.feature.ndc,r.feature.ndc)
                                      )
                                )        |> MapExt.ofList 
                { 
                   i0 = e.i0
                   i1 = e.i1
                   weight = weights
                }
               )
               
        Log.line "[MkTracks] Overall Edges with TrackId associated# %A" (edges |> List.sumBy (fun e -> e.weight.Count) )

        for e in edges do
            Log.line "[MkTracks] TrackIdEdge %A:%A - Pairs %A" e.i0 e.i1 e.weight.Count

        let tracks = 
            paths |> List.map ( fun (tid, path) -> 
                                 tid, path 
                                       |> List.map ( fun (cid,fn) -> cid,fn.feature.ndc )
                                       |> MapExt.ofList )
                  |> MapExt.ofList
         
        tracks, (tree,edges)
        