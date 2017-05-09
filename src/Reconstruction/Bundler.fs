namespace Aardvark.Reconstruction

open System.Collections.Generic

open Aardvark.Base
open Aardvark.Base.Monads.State

type Inliers =
    | Pairwise of MapExt<TrackId, MapExt<CameraId * CameraId, bool>>
    | Combined of MapExt<TrackId, MapExt<CameraId, bool>>
    | Nothing

module Inliers =

    module Adorner =
    
        let noInliers b _ = b, Inliers.Nothing

        let getInliers (b : Bundled) (o : MapExt<TrackId, MapExt<CameraId * CameraId, bool>>) = b, Inliers.Pairwise o

        let getCombinedInlier (comb : bool -> bool -> bool) (b : Bundled) (o : MapExt<TrackId, MapExt<CameraId * CameraId, bool>>) =
            let blub =
                o |> MapExt.map ( fun _ inl ->
                      let l = inl |> MapExt.toList |> List.map ( fun ((c,_),v) -> c,v ) |> MapExt.ofList
                      let r = inl |> MapExt.toList |> List.map ( fun ((_,c),v) -> c,v ) |> MapExt.ofList
                      l |> MapExt.unionWith comb r
                     )

            b, Inliers.Combined blub
    
    type Adorner = Bundled -> MapExt<TrackId, MapExt<CameraId * CameraId, bool>> -> Bundled * Inliers

    let ignore (b : Bundled,_) = b

    let removeOutliers (b : Bundled, i : Inliers) =
        match i with
        | Combined o -> b |> Bundled.filterPointsAndObservationsAggressive (fun tid cid _ _ -> o.[tid].[cid])
        | _ -> b
        
type CameraPoseConfig =
    {
        Config : RecoverPoseConfig
    }
        
module CameraPoseConfig =
    let ok =
        {
            Config = RecoverPoseConfig(1.0, V2d.Zero, 0.99, 0.01) 
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

    open Bundled

    let initial (prob : BundlerProblem) : Bundled =
        Bundled.initial prob |> assertInvariants

    let estimateCams (cfg : CameraPoseConfig) (handleInliers : Inliers.Adorner) (prob : Bundled) : Bundled * Inliers =
        let initialCameras (mst : RoseTree<_>) (minimumEdges : list<Edge<_>>) = 
            let getMatches l r =
                let i = minimumEdges
                            |> List.tryFind ( fun e -> e.i0 = l && e.i1 = r )
                let o1 = i  |> Option.map   ( fun i -> i.weight )

                let i = minimumEdges
                            |> List.tryFind ( fun e -> e.i0 = r && e.i1 = l )

                let o2 = i |> Option.map ( fun i -> i.weight |> MapExt.map ( fun _ v -> tupleSwap v ) )

                [|
                    match o1 with
                    | None -> ()
                    | Some (ps) -> yield ps

                    match o2 with
                    | None -> ()
                    | Some (ps) -> yield ps
                |] |> Seq.head

            let (inliers, res) = Estimate.camsFromMatches cfg.Config mst getMatches
                    
            inliers,
            res |> List.map ( fun (ci, t) -> CameraId(ci), t )

        let (p,s) = prob

        let edges = Tracks.toEdges p.tracks
                                              
        let (mst, minimumEdges) =
            edges |> Graph.ofEdges
                  |> Graph.minimumSpanningTree ( fun e1 e2 -> compare e1.Count e2.Count ) 

        let minimumEdges = minimumEdges |> Array.toList   

        let (inliers, cams) = initialCameras mst minimumEdges
        
        let mutable ns = s
        for (ci, c3d) in cams do
            ns <- ns |> BundlerState.setCamera ci c3d

        let minimumTracks = Edges.toTracks minimumEdges
        handleInliers ({ p with tracks = minimumTracks },ns) inliers 
        
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
    
    let removeBehindPoints (prob : Bundled) : Bundled =
        let (n,s) = prob
        prob |> Bundled.filterPointsAndObservationsAggressive ( fun _ cid _ p -> 
                    let (_,d) = s.cameras.[cid].ProjectWithDepth p 
                    d > 0.0
                )
             |> assertInvariants
             
    let removeRayOutliers (maxRayDist : float) (prob : Bundled) : Bundled =
        let (n,s) = prob
        prob |> Bundled.filterPointsAndObservationsAggressive ( fun _ cid o p -> 
                    let ray = s.cameras.[cid].GetRay o
                    ray.GetMinimalDistanceTo p < maxRayDist
                )
             |> assertInvariants
             
    let removeRayOutliersObservationsOnly (maxRayDist : float) (prob : Bundled) : Bundled =
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
            |> estimateCams CameraPoseConfig.ok Inliers.Adorner.noInliers
            |> Inliers.ignore
            |> estimatePoints 
            |> Bundled.assertInvariants 
            |> bundleAdjust ceresOptions solverConfig 
        
            //|> removeOffscreenPoints
            //|> removeRayOutliersObservationsOnly
            //|> assertInvariants
            //|> bundleAdjust ceresOptions solverConfig
        