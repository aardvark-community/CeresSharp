namespace Aardvark.Reconstruction

open Aardvark.Base
open Aardvark.Base.Monads.State

open System.Collections.Generic

type BundlerProblem =
    {
        tracks : MapExt<TrackId, MapExt<CameraId, V2d>>
    }
    
type BundlerState =
    {
        cameras     : MapExt<CameraId, Camera3d>
        points      : MapExt<TrackId, V3d>
    }

type Bundled<'a> = State<BundlerState, 'a>

module BundlerState =

    let empty = { cameras = MapExt.empty; points = MapExt.empty }

    let setPoint (t : TrackId) (pt : V3d) =
        State.modify (fun s -> { s with points = MapExt.add t pt s.points })

    let unsetPoint (t : TrackId) =
        State.modify (fun s -> { s with points = MapExt.remove t s.points })
        
    let setCamera (c : CameraId) (cam : Camera3d) =
        State.modify ( fun s -> { s with cameras = s.cameras |> MapExt.add c cam } )

    let unsetCamera (c : CameraId) =
        State.modify (fun s -> { s with cameras = s.cameras |> MapExt.remove c })
        
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
                    
module BundlerProblem =
    open Aardvark.Base.MultimethodTest
    
    let empty = { tracks = MapExt.empty }

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
    
module Bundled =
    let removeCamera (id : CameraId) (prob : Bundled<BundlerProblem>) : Bundled<BundlerProblem>=
        state {
            let! p = prob
            let np = p |> BundlerProblem.removeCamera id

            let! s = State.get
            do! BundlerState.unsetCamera id

            return np
        }

    let removeTrack (id : TrackId) (prob : Bundled<BundlerProblem>) : Bundled<BundlerProblem>=
        state {
            let! p = prob
            let np = p |> BundlerProblem.removeTrack id
            
            let! s = State.get
            do! BundlerState.unsetPoint id

            return np
        }   
        