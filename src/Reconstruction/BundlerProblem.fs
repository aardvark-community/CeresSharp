namespace Aardvark.Reconstruction

open Aardvark.Base
open Aardvark.Base.Monads.State

open System.Collections.Generic
open Aardvark.Base.Monads.State


        
module Edges =

    let toTracks (edges : list<Edge<MapExt<TrackId,V2d*V2d>>>) : MapExt<TrackId, MapExt<CameraId, V2d>> =
        
        let mutable res = MapExt.empty

        for e in edges do
            let l = CameraId(e.i0)
            let r = CameraId(e.i1)
            
            for KeyValue(tid, (lo, ro)) in e.weight do

                let addBoth (m : MapExt<_,_>) =
                    let mutable obs = m
                    obs <- obs.Add (l,lo)
                    obs <- obs.Add (r,ro)
                    obs

                res <- res |> MapExt.alter tid ( fun m ->
                            match m with
                            | None -> addBoth MapExt.empty |> Some
                            | Some obs -> addBoth obs |> Some
                       )
        res

module Tracks =

    let toMeasurements (tracks : MapExt<TrackId, MapExt<CameraId, V2d>>) : MapExt<CameraId, MapExt<TrackId, V2d>> =
        
        let mutable measures : MapExt<CameraId, MapExt<TrackId, V2d>> = MapExt.empty

        for (id, m) in MapExt.toSeq tracks do
            for (ci, measure) in MapExt.toSeq m do
                measures <- measures |> MapExt.alter ci ( fun old ->
                                let old = Option.defaultValue MapExt.empty old
                                Some (old |> MapExt.add id measure)
                            )
        measures
            
    let toEdges (tracks : MapExt<TrackId, MapExt<CameraId, V2d>>) : list<Edge<MapExt<TrackId,V2d*V2d>>> =
        let edges = Dict<CameraId * CameraId, ref<MapExt<TrackId,V2d * V2d>>>()
            
        for KeyValue(tid, track) in tracks do
            for KeyValue(ci,pi) in track do
                for KeyValue(cj,pj) in track do
                    if ci < cj then
                        let m = edges.GetOrCreate((ci,cj), (fun _ -> ref MapExt.empty))
                        m := !m |> MapExt.add tid (pi,pj)
            
        edges   |> Dict.toList 
                |> List.choose ( fun ((ci, cj),m) -> 
                    Some { i0 = ci.Id; i1 = cj.Id; weight = !m } 
                )
                    

    
type BundlerState =
    {
        cameras     : MapExt<CameraId, Camera3d>
        points      : MapExt<TrackId,  V3d>
    }


module BundlerState =

    let empty : BundlerState = { cameras = MapExt.empty; points = MapExt.empty }

    let map (fc : CameraId -> Camera3d -> Camera3d) (fp : TrackId -> V3d -> V3d) (state : BundlerState) : BundlerState =
        {   
            cameras = state.cameras |> MapExt.map (fun i v -> fc i v) 
            points = state.points   |> MapExt.map (fun i v -> fp i v) 
        }

    let choose (fc : CameraId -> Camera3d -> Option<Camera3d>) (fp : TrackId -> V3d -> Option<V3d>) (state : BundlerState) : BundlerState =
        {   
            cameras = state.cameras |> MapExt.choose (fun i v -> fc i v) 
            points = state.points   |> MapExt.choose (fun i v -> fp i v) 
        }
        
    let filter (fc : CameraId -> Camera3d  -> bool) (fp : TrackId -> V3d  -> bool) (state : BundlerState) : BundlerState =
        state |> choose ( fun id v -> if fc id v then Some v else None) ( fun id v -> if fp id v then Some v else None)
        

    let setPoint (t : TrackId) (pt : V3d) (s : BundlerState) =
        { s with points = MapExt.add t pt s.points }

    let unsetPoint (t : TrackId) (s : BundlerState) =
        { s with points = MapExt.remove t s.points }
        
    let setCamera (c : CameraId) (cam : Camera3d) (s : BundlerState) =
        { s with cameras = s.cameras |> MapExt.add c cam }

    let unsetCamera (c : CameraId) (s : BundlerState) =
        { s with cameras = s.cameras |> MapExt.remove c }


        
type BundlerProblem =
    {
        tracks : MapExt<TrackId, MapExt<CameraId, V2d>>
    }

module BundlerProblem =
    
    let empty : BundlerProblem = { tracks = MapExt.empty }
    
    let map (f : TrackId -> CameraId -> V2d -> V2d) (prob : BundlerProblem) : BundlerProblem =
        { 
            tracks = 
                prob.tracks |> MapExt.map ( fun tid track ->
                                track |> MapExt.map ( fun cid obs -> f tid cid obs )
                               )
        }

    let choose (f : TrackId -> CameraId -> V2d -> Option<V2d>) (prob : BundlerProblem) : BundlerProblem =
        { 
            tracks = 
                prob.tracks |> MapExt.choose ( fun tid track ->
                                let newTrack = track |> MapExt.choose ( fun cid obs -> f tid cid obs )
                                if newTrack.Count > 0 then Some newTrack else None
                               )
        }
        
    let filter (f : TrackId -> CameraId -> V2d -> bool) (prob : BundlerProblem) : BundlerProblem =
        prob |> choose ( fun tid cid o -> if f tid cid o then Some o else None )
        
        

    let removeCamera (id : CameraId) (sol : BundlerProblem) =
        { 
            tracks =
                sol.tracks |> MapExt.choose (fun k pt ->
                    let r = MapExt.remove id pt
                    if MapExt.count r > 1 then
                        Some r
                    else
                        None
                )
        }

    let removeTrack (id : TrackId) (sol : BundlerProblem) =
        {   tracks = MapExt.remove id sol.tracks    }

    let addMeasurement (tid : TrackId) (cam : CameraId) (position : V2d) (sol : BundlerProblem) =
        { 
            tracks =
                sol.tracks
                    |> MapExt.alter tid (fun pt ->
                        match pt with
                            | Some pt -> 
                                Some ( MapExt.add cam position pt )
                            | None ->
                                Some ( MapExt.ofList [cam, position] )
                    )
        }

    let removeMeasurement (tid : TrackId) (cid : CameraId) (sol : BundlerProblem) =
        {
            tracks =
                sol.tracks
                    |> MapExt.alter tid ( fun pt ->
                        match pt with
                            | None -> None
                            | Some pt -> Some ( MapExt.remove cid pt )
                       )
                    |> MapExt.filter ( fun _ track ->
                        not (track |> MapExt.isEmpty)
                       )
        }

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
    
type Bundled = BundlerProblem * BundlerState

module Bundled =
    
    let map (ft : TrackId -> CameraId -> V2d -> V2d) (fc : CameraId -> Camera3d -> Camera3d) (fp : TrackId -> V3d -> V3d) (b : Bundled) : Bundled =
        
        let (prob,state) = b
        (prob |> BundlerProblem.map ft, state |> BundlerState.map fc fp)

    let choose (ft : TrackId -> CameraId -> V2d -> Option<V2d>) (fc : CameraId -> Camera3d -> Option<Camera3d>) (fp : TrackId -> V3d -> Option<V3d>) (b : Bundled) : Bundled =
        
        let (prob,state) = b
        (prob |> BundlerProblem.choose ft, state |> BundlerState.choose fc fp)

    let filter (ft : TrackId -> CameraId -> V2d -> bool) (fc : CameraId -> Camera3d -> bool) (fp : TrackId -> V3d -> bool) (b : Bundled) : Bundled =
        
        let (prob,state) = b
        (prob |> BundlerProblem.filter ft, state |> BundlerState.filter fc fp)

    let filterPointsAndObservations (f : TrackId -> CameraId -> V2d -> V3d -> bool) (b : Bundled) : Bundled =
        let (n,p) = b
        let ft tid cid o = f tid cid o p.points.[tid]
        let fp tid p =
            [
                for KeyValue(cid, o) in n.tracks.[tid] do
                    yield f tid cid o p
            ] |> List.fold (&&) true

        filter ft (fun _ _ -> true) fp b
     

    let removeCamera (id : CameraId) (prob : Bundled) : Bundled =

        let (p,s) = prob
        let np = p |> BundlerProblem.removeCamera id
        let ns = s |> BundlerState.unsetCamera id

        (np,ns)

    let removeTrack (id : TrackId) (prob : Bundled) : Bundled =
        
        let (p,s) = prob
        let np = p |> BundlerProblem.removeTrack id
        let ns = s |> BundlerState.unsetPoint id

        (np,ns)
    
    let initial (p : BundlerProblem) : Bundled =

        let mutable initial = BundlerState.empty
        
        let edges = Tracks.toEdges p.tracks

        let (mst, minimumEdges) =
            edges   |> Graph.ofEdges
                    |> Graph.minimumSpanningTree ( fun e1 e2 -> compare e1.Count e2.Count ) 

        let minimumEdges = minimumEdges |> Array.toList 
             
        let minimumTracks = Edges.toTracks minimumEdges

        let minimumMeasurements = Tracks.toMeasurements minimumTracks
                   
        for KeyValue(ci, _) in minimumMeasurements do
            initial <- initial |> BundlerState.setCamera ci (Camera3d())

        for KeyValue(pi, _) in minimumTracks do
            initial <- initial |> BundlerState.setPoint pi V3d.Zero

        (p,initial)

        