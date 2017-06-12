﻿namespace Aardvark.Reconstruction

open Aardvark.Base
open Aardvark.Base.Monads.State

open System.Collections.Generic
open Aardvark.Base.Monads.State



            
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
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


    let withPoints (tracks : MapExt<TrackId, MapExt<CameraId,V2d>>) (getPoint : TrackId -> V3d) (s : BundlerState) =
        {
            s with points = tracks |> MapExt.map ( fun tid _ -> getPoint tid ) 
        }

    let withCameras (measures : MapExt<CameraId, MapExt<TrackId,V2d>>) (getCam : CameraId -> Camera3d) (s : BundlerState) =
        {
            s with cameras = measures |> MapExt.map ( fun cid _ -> getCam cid ) 
        }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
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
                prob.tracks |> MapExt.map ( fun tid track ->
                                track |> MapExt.choose ( fun cid obs -> f tid cid obs )
                               )
        }
        
    let filter (f : TrackId -> CameraId -> V2d -> bool) (prob : BundlerProblem) : BundlerProblem =
        prob |> choose ( fun tid cid o -> if f tid cid o then Some o else None )
        
    let removeCamera (id : CameraId) (sol : BundlerProblem) =
        { 
            tracks =
                sol.tracks |> MapExt.map (fun k pt ->
                    MapExt.remove id pt
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
        }

    let ofMeasurements (measurements : MapExt<CameraId, MapExt<TrackId, V2d>>) : BundlerProblem =

        { tracks = Measurements.toTracks measurements }
    
type ReprojectionError =
    {
        cost        : float
        max         : float
        min         : float
        average     : float
        stdev       : float
    }

module ReprojectionError = 
    let nothing =
        {
            cost        = 0.0
            max         = 0.0
            min         = 0.0
            average     = 0.0
            stdev       = 0.0 
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Bundled =

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
    
    let minTrackLength = 3
    let minObsCount = 5
    
    let unstableCameras ((prob,state) : Bundled) : list<CameraId> =
        let ms = prob.tracks |> Tracks.toMeasurements

        state.cameras
            |> MapExt.toList
            |> List.choose ( fun (cid,_) ->
                ms  |> MapExt.tryFind cid 
                    |> Option.map (fun obs -> if obs.Count < minObsCount then Some cid else None) 
                    |> Option.defaultValue (Some cid)
               )

    let unstablePoints ((prob,state) : Bundled) : list<TrackId> =
        state.points
            |> MapExt.toList
            |> List.map fst
            |> List.append (prob.tracks |> MapExt.toList |> List.map fst)
            |> List.distinct
            |> List.choose ( fun tid ->
                prob.tracks  
                    |> MapExt.tryFind tid 
                    |> Option.map (fun obs -> if obs.Count < minTrackLength then Some tid else None) 
                    |> Option.defaultValue (Some tid)
               )

    let swap f a b = f b a

    let assertInvariants (prob : Bundled) : Bundled =
        let rec fix (prob : Bundled) =
            match unstableCameras prob with
            | [] ->
                match unstablePoints prob with
                | [] -> prob
                | badTracks ->
                    badTracks |> List.fold (swap removeTrack) prob
                              |> fix
            | badCams ->
                badCams |> List.fold (swap removeCamera) prob
                        |> fix
        fix prob
    
    let map (ft : TrackId -> CameraId -> V2d -> V2d) (fc : CameraId -> Camera3d -> Camera3d) (fp : TrackId -> V3d -> V3d) (b : Bundled) : Bundled =
        
        let (prob,state) = b
        (prob |> BundlerProblem.map ft, state |> BundlerState.map fc fp)

    let choose (ft : TrackId -> CameraId -> V2d -> Option<V2d>) (fc : CameraId -> Camera3d -> Option<Camera3d>) (fp : TrackId -> V3d -> Option<V3d>) (b : Bundled) : Bundled =
        
        let (prob,state) = b
        (prob |> BundlerProblem.choose ft, state |> BundlerState.choose fc fp)

    let private filter (ft : TrackId -> CameraId -> V2d -> bool) (fc : CameraId -> Camera3d -> bool) (fp : TrackId -> V3d -> bool) (b : Bundled) : Bundled =
        
        let (prob,state) = b
        (prob |> BundlerProblem.filter ft, state |> BundlerState.filter fc fp)

    let filterObservations (ft : TrackId -> CameraId -> V2d -> bool) (b : Bundled) : Bundled =
        filter 
            ft
            (constF (constF true))
            (constF (constF true))
            b
        |> assertInvariants
       
    let filterCameras (fc : CameraId -> Camera3d -> bool) (b : Bundled) : Bundled =
        filter 
            (constF (constF (constF true)))
            fc
            (constF (constF true))
            b
        |> assertInvariants
    
    let filterPoints (fp : TrackId -> V3d -> bool) (b : Bundled) : Bundled =
        filter 
            (constF (constF (constF true)))
            (constF (constF true))
            fp
            b
        |> assertInvariants

    let filterPointsAndObservationsAggressive (f : TrackId -> CameraId -> V2d -> V3d -> bool) (b : Bundled) : Bundled =
        let (n,p) = b
        let evil = HashSet<_>()

        let fp tid p =
            [
                for KeyValue(cid, o) in n.tracks.[tid] do
                    yield f tid cid o p, (tid)
            ] |> List.fold ( fun anyrem (rem, (tid)) ->
                             let passt = anyrem && rem
                             if not passt then evil.Add(tid) |> ignore
                             passt
                           ) true
        
        let ft tid _ _ = evil.Contains(tid) |> not
        
        let np = BundlerState.filter (constF (constF true)) fp p
        let nn = BundlerProblem.filter ft n

        let (nn,np) = assertInvariants (nn,np)

        (nn, np)

    
    let initial (p : BundlerProblem) (es : Option<Edges>) : Bundled * Edges =

        let mutable initial = BundlerState.empty
        let (mst, maximumEdges) =
            match es with 
            | None -> 
                let (mst, maximumEdges) =
                    p.tracks
                            |> Tracks.toEdgesAllWithAll
                            |> Graph.ofEdges
                            |> Graph.minimumSpanningTree ( fun e1 e2 -> - compare e1.Count e2.Count ) 
        
                let maximumEdges = maximumEdges |> Array.toList 
                
                (mst, maximumEdges)

            | Some v -> v
             
        let maximumTracks = Edges.toTracks maximumEdges
        
        let maximumMeasurements = Tracks.toMeasurements maximumTracks
                   
        for KeyValue(ci, _) in maximumMeasurements do
            initial <- initial |> BundlerState.setCamera ci (Camera3d())

        for KeyValue(pi, _) in maximumTracks do
            initial <- initial |> BundlerState.setPoint pi V3d.Zero
            
        ( { p with tracks = maximumTracks } ,initial), (mst, maximumEdges)

    open System

    let reprojectionError ( (prob,state) : Bundled ) : ReprojectionError =
        let errors =
            [|
                for KeyValue(tid, t) in prob.tracks do
                    for KeyValue(cid, obs) in t do
                        let diff = (state.cameras.[cid].Project state.points.[tid]) - obs
                        yield diff, (Vec.length (0.5*diff))
            |] 

        let mutable sumSq = 0.0
        let mutable sum = 0.0
        let mutable emin = Double.PositiveInfinity
        let mutable emax = Double.NegativeInfinity
        for (v,l) in errors do
            sumSq <- sumSq + v.X * v.X + v.Y * v.Y
            sum <- sum + l
            emin <- min emin l
            emax <- max emax l
            
        let average = sum / float errors.Length
        let variance = Array.sumBy (fun (_,e) -> (e - average) * (e - average)) errors / float (errors.Length - 1)
        
        {
            cost        = 0.5 * sumSq
            max         = emax
            min         = emin
            average     = average
            stdev       = sqrt variance
        }
        