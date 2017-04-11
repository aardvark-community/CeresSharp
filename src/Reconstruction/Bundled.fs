namespace Aardvark.Reconstruction

open System.Collections.Generic

open Aardvark.Base
open Aardvark.Base.Monads.State

module Bundled =
    open CeresSharp

    let minTrackLength = 2
    let minObsCount = 5
    
    let unstableCameras (prob : BundlerProblem) (cams : list<CameraId>) : list<CameraId> =
        prob.tracks 
            |> Tracks.toMeasurements
            |> MapExt.toList 
            |> List.filter ( fun (ci,ms) -> ms.Count < minObsCount ) 
            |> List.map fst

    let unstablePoints (prob : BundlerProblem) (pts : list<TrackId>) : list<TrackId> =
        prob.tracks
            |> MapExt.toList
            |> List.filter ( fun (ti, t) -> t.Count < minTrackLength )
            |> List.map fst

    let assertInvariants (prob : Bundled<BundlerProblem>) : Bundled<BundlerProblem> =
        state {
            let! p = prob

            let rec fix (prob : BundlerProblem) =
                state {
                    let p = prob
                    let! s = State.get
                    match unstableCameras p (s.cameras |> MapExt.toList |> List.map fst) with
                    | [] ->
                        match unstablePoints p (s.points |> MapExt.toList |> List.map fst) with
                        | [] -> return p
                        | badTracks ->
                            let mutable res = p
                            for tid in badTracks do 
                                res <- res |> BundlerProblem.removeTrack tid
                                do! BundlerState.unsetPoint tid
                            return! fix res
                    | badCams ->
                        let mutable res = p
                        for cid in badCams do
                            res <- res |> BundlerProblem.removeCamera cid
                            do! BundlerState.unsetCamera cid
                        return! fix res
                }

            let! fixedProb = fix p

            return fixedProb
        }
        
    let map (pf : TrackId*V3d -> V3d) (cf : CameraId*Camera3d -> Camera3d) (prob : Bundled<BundlerProblem>) : Bundled<BundlerProblem> =
        state {
            let! p = prob
            let! s = State.get

            do! State.put { s with points =     s.points    |> MapExt.map ( fun i v -> pf(i,v) ); 
                                   cameras =    s.cameras   |> MapExt.map ( fun i v -> cf(i,v) )}

            return p
        } |> assertInvariants

    let mapPoints pf prob = map pf snd>>id prob
    let mapCams   cf prob = map snd>>id cf prob
    
    let filter (pf : TrackId*V3d -> bool) (cf : CameraId*Camera3d -> bool) (prob : Bundled<BundlerProblem>) : Bundled<BundlerProblem> =
        state {
            let! p = prob
            let! s = State.get

            do! State.put { s with points =     s.points    |> MapExt.filter ( fun i v -> pf(i,v) ); 
                                   cameras =    s.cameras   |> MapExt.filter ( fun i v -> cf(i,v) )}

            return p
        } |> assertInvariants

    let filterPoints pf prob = filter pf (fun _ -> true) prob
    let filterCams   cf prob = filter (fun _ -> true) cf prob
    
    let emptyState (p : BundlerProblem) : Bundled<BundlerProblem> =
        state {
            let edges = Tracks.toEdges p.tracks
                                              
            let (mst, minimumEdges) =
                edges |> Graph.ofEdges
                        |> Graph.minimumSpanningTree ( fun e1 e2 -> compare e1.Count e2.Count ) 

            let minimumEdges = minimumEdges |> Array.toList |> List.map (Edge.map Seq.toList)         
             
            let minimumTracks = Edges.toTracks minimumEdges

            let minimumMeasurements = Tracks.toMeasurements minimumTracks
                   
            for KeyValue(ci, _) in minimumMeasurements do
                do! BundlerState.setCamera ci (Camera3d())

            for KeyValue(pi, _) in minimumTracks do
                do! BundlerState.setPoint pi V3d.Zero
            
            return { tracks = minimumTracks }
        } |> assertInvariants

    let estimateCams (prob : Bundled<BundlerProblem>) : Bundled<BundlerProblem> =
        let initialCameras (mst : RoseTree<_>) (minimumEdges : list<Edge<_>>) = 
            
            let getMatches l r =
                let i = minimumEdges
                            |> List.tryFind ( fun e -> e.i0 = l && e.i1 = r )
                let o1 = i  |> Option.map   ( fun i -> i.weight    |> List.toArray )

                let i = minimumEdges
                            |> List.tryFind ( fun e -> e.i0 = r && e.i1 = l )

                let o2 = i |> Option.map ( fun i -> i.weight |> List.map ( fun (s,p) -> p,s ) |> List.toArray )

                [|
                    match o1 with
                    | None -> ()
                    | Some (ps) -> yield ps

                    match o2 with
                    | None -> ()
                    | Some (ps) -> yield ps
                |] |> Seq.head

            Estimate.camsFromMatches mst getMatches
                    |> List.map ( fun (ci, t) -> CameraId(ci), t )

        state {
            let! p = prob

            let edges = Tracks.toEdges p.tracks
                                              
            let (mst, minimumEdges) =
                edges |> Graph.ofEdges
                        |> Graph.minimumSpanningTree ( fun e1 e2 -> compare e1.Count e2.Count ) 

            let minimumEdges = minimumEdges |> Array.toList |> List.map (Edge.map Seq.toList)     

            let cams = initialCameras mst minimumEdges
            
            for (ci, c3d) in cams do
                do! BundlerState.setCamera ci c3d

            return p
        }
        
    let estimatePoints (prob : Bundled<BundlerProblem>) : Bundled<BundlerProblem> =
        state {
            let! p = prob
            let! s = State.get

            let points = Estimate.pointsFromCams p.tracks ( fun cid obs -> s.cameras.[cid].GetRay obs )

            let mutable res = p
            for KeyValue(tid, point) in points do
                match point with
                | None -> 
                    res <- res |> BundlerProblem.removeTrack tid
                    do! BundlerState.unsetPoint tid
                | Some pt ->
                    do! BundlerState.setPoint tid pt

            return res
        }
        
    let removeOffscreenPoints (prob : Bundled<BundlerProblem>) : Bundled<BundlerProblem> =
        state {
            let! s = State.get
            let! np = prob |> filterPoints 
                              ( fun (_,p) -> 
                                s.cameras 
                                  |> MapExt.toList
                                  |> List.map snd
                                  |> List.exists ( fun cam ->
                                         let (_,d) = cam.ProjectWithDepth p 
                                         d < 0.0
                                     )
                                  |> not
                              )
            return np
        }

    let maxRayDist = 100.0
    let removeRayOutliers (prob : Bundled<BundlerProblem>) : Bundled<BundlerProblem> =
        state {
            let! s = State.get
            let! op = prob
            let! np = prob |> filterPoints 
                              ( fun (ti,p) -> 
                                s.cameras 
                                  |> MapExt.toList
                                  |> List.exists ( fun (ci,cam) ->
                                         let ray = cam.GetRay op.tracks.[ti].[ci]
                                         ray.GetMinimalDistanceTo p > maxRayDist
                                     )
                                  |> not
                              )

            return np
        }

    let bundleAdjust (options : CeresOptions) (prob : Bundled<BundlerProblem>) : Bundled<BundlerProblem> =
        undefined
 
module Bundler =
    
    open Bundled

    let ofMeasurements (measurements : MapExt<CameraId, MapExt<TrackId, V2d>>) : BundlerProblem =
        
        // get all the tracks
        let mutable tracks : MapExt<TrackId, MapExt<CameraId, V2d>> = MapExt.empty

        for (ci, measurements) in MapExt.toSeq measurements do
            for (id, m) in MapExt.toSeq measurements do
                tracks <- 
                    tracks |> MapExt.alter id (fun old ->
                        let old = Option.defaultValue MapExt.empty old
                        Some (old |> MapExt.add ci m)
                    )
                    
        { tracks = tracks }
        
    let miniCV (p : BundlerProblem) : Bundled<BundlerProblem> =
        let ceresOptions = undefined
        
        let state = 
            emptyState p
                |> assertInvariants
                |> estimateCams
                |> estimatePoints
                |> assertInvariants
                |> removeOffscreenPoints
                |> removeRayOutliers
                |> bundleAdjust ceresOptions
                |> assertInvariants
                |> bundleAdjust ceresOptions
                //...
                
        state