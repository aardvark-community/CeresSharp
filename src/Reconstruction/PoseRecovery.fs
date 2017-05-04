namespace Aardvark.Reconstruction

open System
open Aardvark.Base
open Aardvark.Base.MiniCV

module Estimate = 
    let LtoRTrafo (cfg : RecoverPoseConfig) (l : V2d[]) (r : V2d[]) =
            
        let (i,R,t) = MiniCV.recoverPose cfg l r

        Trafo3d.FromBasis(R.C0, R.C1, R.C2, t)
   
    let camsFromMatches (cfg : RecoverPoseConfig) (cameraTree : RoseTree<int>) (getMatches : int -> int -> (V2d * V2d)[] ) =
        
        let register ci parent =
            let (a, b) = getMatches parent ci |> Array.unzip
            LtoRTrafo cfg a b

        let rec traverse (cur : Trafo3d) (parent : int) (remaining : RoseTree<_>) =
            match remaining with
            | Empty -> Empty
            | Leaf ci ->
                let trafo = cur * register ci parent
                Leaf(ci, trafo)
            | Node (ci, children) ->
                let trafo = cur * register ci parent
                Node((ci, trafo), children |> List.map (traverse trafo ci) )

        let trafoTree = 
            match cameraTree with
            | Empty -> Empty
            | Leaf i ->
                Leaf(i, Trafo3d.Identity)
            | Node(i, children) ->
                let trafo = Trafo3d.Identity
                Node((i, trafo), children |> List.map (traverse trafo i))
            
        trafoTree 
            |> RoseTree.toList
            |> List.map ( fun (ci,trafo) -> 
                let oc = Camera3d.LookAt(V3d.IOO * 10.0, V3d.Zero, 1.0, V3d.OOI)
                let forward = AngleAxis.RotatePoint(-oc.AngleAxis, -V3d.OOI)
                let trafo = (Trafo3d.Rotation(forward, -Math.PI/2.0)) * trafo
                ci, oc.Transformed trafo
                ) 
    
    let pointsFromCams (tracks : MapExt<TrackId, MapExt<CameraId, V2d>>) ( getRay : CameraId -> V2d -> Ray3d ) =
        tracks 
            |> MapExt.map
                ( fun tid track ->
                    track   |> MapExt.toList
                            |> List.map ( fun (ci,obs) -> getRay ci obs)
                            |> Ray3d.avg
                )
