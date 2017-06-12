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

    let initial (es : Option<Edges>) (prob : BundlerProblem): Bundled * Edges =
        Bundled.initial prob es

    let estimateCams (cameraTree : Edges) (cfg : CameraPoseConfig) (handleInliers : Inliers.Adorner) (prob : Bundled) : Bundled * Inliers =
        let initialCameras (mst : RoseTree<_>) (minimumEdges : list<Edge<_>>) measures = 
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

            let (inliers, res) = Estimate.camsFromMatches cfg.Config mst getMatches measures
                    
            inliers,
            res |> List.map ( fun (ci, t) -> CameraId(ci), t )

        let (p,s) = prob

        let (mst,minimumEdges) = cameraTree
        
        let minimumMeasures = minimumEdges |> Edges.toTracks |> Tracks.toMeasurements

        let (inliers, cams) = initialCameras mst minimumEdges minimumMeasures

        let cams = cams |> MapExt.ofList

        let ns = s |> BundlerState.withCameras minimumMeasures (fun cid -> cams.[cid])

        let (res,i) = handleInliers (p,ns) inliers 

        (res,i)
        
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
                    d < 0.0
                )
             |> assertInvariants
    
    let removeTooClosePoints (minDepth : float) (prob : Bundled) : Bundled =
        let (n,s) = prob
        prob |> Bundled.filterPointsAndObservationsAggressive ( fun _ cid _ p -> 
                    let (p,d) = s.cameras.[cid].ProjectWithDepth p 
                    abs d > minDepth
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
    
    //let miniCV (p : BundlerProblem) : Bundled =
    //    let ceresOptions = CeresOptions(2500, CeresSolverType.SparseSchur, true, 1.0E-16, 1.0E-16, 1.0E-16)
    //    let solverConfig = SolverConfig.allFree
        
    //    let e = ref Unchecked.defaultof<_>


    //    Bundler.initial p
    //        |> Edges.get e
    //        |> estimateCams !e CameraPoseConfig.ok Inliers.Adorner.noInliers
    //        |> Inliers.removeOutliers
    //        |> estimatePoints 
    //        |> Bundled.assertInvariants 
    //        |> bundleAdjust ceresOptions solverConfig 
        
            //|> removeOffscreenPoints
            //|> removeRayOutliersObservationsOnly
            //|> assertInvariants
            //|> bundleAdjust ceresOptions solverConfig
        
    let miniCV (p : BundlerProblem) : Bundled =
        let ceresOptions = CeresOptions(2500, CeresSolverType.SparseSchur, true, 1.0E-16, 1.0E-16, 1.0E-16)
        let solverConfig = SolverConfig.allFree
        
        let e = ref Unchecked.defaultof<_>
        
        let p1 = Bundler.initial None p
        let r1 = Edges.get e p1
        let r2 = estimateCams !e CameraPoseConfig.ok Inliers.Adorner.noInliers r1
        let r3 = Inliers.removeOutliers r2
        let r4 = estimatePoints r3
        let r5 = Bundled.assertInvariants r4
        let r6 = bundleAdjust ceresOptions solverConfig r5

        r6