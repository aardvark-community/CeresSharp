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
    let setPoint (t : TrackId) (pt : V3d) =
        State.modify (fun s -> { s with points = MapExt.add t pt s.points })

    let unsetPoint (t : TrackId) =
        State.modify (fun s -> { s with points = MapExt.remove t s.points })
        
    let setCamera (c : CameraId) (cam : Camera3d) =
        State.modify ( fun s -> { s with cameras = s.cameras |> MapExt.add c cam } )

    let unsetCamera (c : CameraId) =
        State.modify (fun s -> { s with cameras = s.cameras |> MapExt.remove c })
        
module Edges =

    let toTracks (edges : list<Edge<list<V2d*V2d>>>) : MapExt<TrackId, MapExt<CameraId, V2d>> =
            
        let used = 
            let d = Dict<Edge<_>, bool>()
            for e in edges do d.[e] <- false
            d
            
        failwith "IMPLEMENT ME PLS"

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
            
    let toEdges (tracks : MapExt<TrackId, MapExt<CameraId, V2d>>) : list<Edge<List<V2d*V2d>>> =
        let edges = Dict<CameraId * CameraId, List<V2d * V2d>>()
            
        for KeyValue(_, track) in tracks do
            for KeyValue(ci,pi) in track do
                for KeyValue(cj,pj) in track do
                    if ci < cj then
                        let list = edges.GetOrCreate((ci,cj), (fun _ -> List()))
                        list.Add (pi,pj)
            
        edges |> Dict.toList 
                |> List.choose ( fun ((ci, cj),l) -> 
                    Some { i0 = ci.Id; i1 = cj.Id; weight = l } 
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
        { tracks = MapExt.remove id sol.tracks }

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
    