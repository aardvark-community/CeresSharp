namespace Aardvark.Reconstruction

open System.Collections.Generic

open Aardvark.Base
open Aardvark.Base.Monads.State

type CameraPoseConfig =
    {
        Config : RecoverPoseConfig
        IsInlierAdorner : Bundled -> MapExt<TrackId, MapExt<CameraId, bool>> -> Bundled
    }

module Adorner =
    
    let res b _ = b

    let getOutliers (out : ref<MapExt<TrackId, MapExt<CameraId, bool>>>) (b : Bundled) (o : MapExt<TrackId, MapExt<CameraId, bool>>) =
        out := o
        res b o

    let removeOutliers (b : Bundled) (o : MapExt<TrackId, MapExt<CameraId, bool>>) =
        b |> Bundled.filterObservations (fun tid cid _ -> o.[tid].[cid])
        
module CameraPoseConfig =
    let ok =
        {
            Config = RecoverPoseConfig(1.0, V2d.Zero, 0.999, 0.001) 
            IsInlierAdorner = Adorner.res
        }

    let ofPrecision p t =
        {
            ok with
                Config = RecoverPoseConfig(1.0, V2d.Zero, p, t) 
        }
    
module Bundler =
    open CeresSharp
    open Aardvark.Base.MultimethodTest
    open Aardvark.Base.ASM
    
    let minTrackLength = 2
    let minObsCount = 5
    
    let unstableCameras (state : BundlerProblem) : list<CameraId> =
        state.tracks
            |> Tracks.toMeasurements
            |> MapExt.toList 
            |> List.filter ( fun (ci,ms) -> ms.Count < minObsCount ) 
            |> List.map fst

    let unstablePoints (prob : BundlerProblem) : list<TrackId> =
        prob.tracks
            |> MapExt.toList
            |> List.filter ( fun (ti, t) -> t.Count < minTrackLength )
            |> List.map fst

    let assertInvariants (prob : Bundled) : Bundled =
        let rec fix (prob : Bundled) =
            let (p,s) = prob
            match unstableCameras p with
            | [] ->
                match unstablePoints p with
                | [] -> (p,s)
                | badTracks ->
                    let mutable res = (p,s)
                    for tid in badTracks do 
                        res <- res |> Bundled.removeTrack tid
                    fix res
            | badCams ->
                let mutable res = (p,s)
                for cid in badCams do
                    res <- res |> Bundled.removeCamera cid
                fix res
        fix prob

    let initial (prob : BundlerProblem) : Bundled =
        Bundled.initial prob |> assertInvariants

    let estimateCams (cfg : CameraPoseConfig) (prob : Bundled) : Bundled =
        let initialCameras (mst : RoseTree<_>) (minimumEdges : list<Edge<_>>) = 
            let getMatches l r =
                let i = minimumEdges
                            |> List.tryFind ( fun e -> e.i0 = l && e.i1 = r )
                let o1 = i  |> Option.map   ( fun i -> i.weight |> MapExt.toArray |> Array.map snd )

                let i = minimumEdges
                            |> List.tryFind ( fun e -> e.i0 = r && e.i1 = l )

                let o2 = i |> Option.map ( fun i -> i.weight |> MapExt.toArray |> Array.map snd |> Array.map tupleSwap )

                [|
                    match o1 with
                    | None -> ()
                    | Some (ps) -> yield ps

                    match o2 with
                    | None -> ()
                    | Some (ps) -> yield ps
                |] |> Seq.head

            Estimate.camsFromMatches cfg.Config mst getMatches
                    |> List.map ( fun (ci, t) -> CameraId(ci), t )

        let (p,s) = prob

        let edges = Tracks.toEdges p.tracks
                                              
        let (mst, minimumEdges) =
            edges |> Graph.ofEdges
                    |> Graph.minimumSpanningTree ( fun e1 e2 -> compare e1.Count e2.Count ) 

        let minimumEdges = minimumEdges |> Array.toList   

        let cams = initialCameras mst minimumEdges
         
        let mutable ns = s
        for (ci, c3d) in cams do
            ns <- ns |> BundlerState.setCamera ci c3d

        (p,ns)
        
    let estimatePoints (prob : Bundled) : Bundled =
        let (p,s) = prob

        let points = Estimate.pointsFromCams p.tracks ( fun cid obs -> s.cameras.[cid].GetRay obs )

        let mutable res = (p,s)
        for KeyValue(tid, point) in points do
            match point with
            | None -> 
                res <- res |> Bundled.removeTrack tid
            | Some pt ->
                let (p,s) = res
                res <- (p, s |> BundlerState.setPoint tid pt)

        res
    
    let estimateBoth = estimateCams >> estimatePoints
        
    let removeOffscreenPoints (prob : Bundled) : Bundled =
        let (n,s) = prob
        prob |> Bundled.filterPointsAndObservationsAggressive ( fun _ cid _ p -> 
                    let (_,d) = s.cameras.[cid].ProjectWithDepth p 
                    d > 0.0
                )
             |> assertInvariants

    let maxRayDist = 100.0
    let removeRayOutliers (prob : Bundled) : Bundled =
        let (n,s) = prob
        prob |> Bundled.filterPointsAndObservationsAggressive ( fun _ cid o p -> 
                    let ray = s.cameras.[cid].GetRay o
                    ray.GetMinimalDistanceTo p < maxRayDist
                )
             |> assertInvariants
             
    let removeRayOutliersObservationsOnly (prob : Bundled) : Bundled =
        let (n,s) = prob
        let nn = n |> BundlerProblem.filter ( fun tid cid o ->
                        let ray = s.cameras.[cid].GetRay o
                        ray.GetMinimalDistanceTo s.points.[tid] > maxRayDist
                      )

        (nn,s) |> assertInvariants
        
    let bundleAdjust (options : CeresOptions) (config : SolverConfig) (prob : Bundled) : Bundled =
        let (p,s) = prob

        let (cost, points, cams) = Solver.bundleAdjust options config s.points s.cameras p.tracks

        let ns =  { s with points = points; cameras = cams }

        (p,ns)
        
module CoolNameGoesHere =
    
    open Bundler
    open CeresSharp
    
    let miniCV (p : BundlerProblem) : Bundled =
        let ceresOptions = CeresOptions(2500, CeresSolverType.SparseSchur, true, 1.0E-16, 1.0E-16, 1.0E-16)
        let solverConfig = SolverConfig.allFree
        
        Bundler.initial p
            |> estimateCams 
            |> estimatePoints 
            |> assertInvariants 
            |> bundleAdjust ceresOptions solverConfig 
        
            //|> removeOffscreenPoints
            //|> removeRayOutliersObservationsOnly
            //|> assertInvariants
            //|> bundleAdjust ceresOptions solverConfig
        