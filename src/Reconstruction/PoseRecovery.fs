namespace Aardvark.Reconstruction

open System
open Aardvark.Base
open Aardvark.Base.MiniCV

module Estimate = 
    open Aardvark.Base.MultimethodTest

    let LtoRTrafo (cfg : RecoverPoseConfig) (l : V2d[]) (r : V2d[]) =
        
        Log.line "match count: %A" l.Length

        if l.Length < 20 then Log.warn "Low match count: %A %A" l.Length r.Length

        // v- this is a hack, since cameraTree edges with so few matches should actually be removed.
        // cameraTree would fall apart into multiple trees. TODO: Think about this.
        if l.Length > 5 && r.Length > 5 then
            let (i,R,t,mask) = MiniCV.recoverPose cfg l r

            Log.line "inliers: %A (of %A)" (mask |> Array.sumBy int) (l |> Array.length)

            //printfn "t = %A" t
            //printfn "rot: "
            //printfn "%A" R.R0
            //printfn "%A" R.R1
            //printfn "%A" R.R2

            mask, Trafo3d.FromBasis(R.C0, R.C1, R.C2, t)
             
        else
            l |> Array.map ( fun _ -> 0uy ), Trafo3d.Identity
   
    let pointsFromCams (tracks : MapExt<TrackId, MapExt<CameraId, V2d>>) ( getRay : CameraId -> V2d -> Ray3d ) =
        tracks 
            |> MapExt.map
                ( fun tid track ->
                    track   |> MapExt.toList
                            |> List.map ( fun (ci,obs) -> getRay ci obs)
                            |> Ray3d.avg
                )

    let private (!) = CameraId

    let camsFromMatches (cfg : RecoverPoseConfig) (cameraTree : RoseTree<int>)
                        (getMatches : int -> int -> MapExt<TrackId, (V2d * V2d)> )
                        (measurements : MapExt<CameraId, MapExt<TrackId, V2d>>) =
        
        let register ci parent =
            let ms =  getMatches parent ci
            let (a, b) = ms |> MapExt.toArray |> Array.map snd |> Array.unzip
            let idx = ms |> MapExt.toArray |> Array.map fst
            let (mask, trafo) = LtoRTrafo cfg a b
            let inliers = 
                [
                    for i in 0..mask.Length-1 do
                        yield idx.[i], (CameraId(ci),CameraId(parent),(mask.[i] > 0uy))
                ] |> MapExt.ofList

            inliers,trafo
            
        let solved = Dictionary.empty
        let rootCam = Camera3d(V3d.Zero, V3d.Zero)
        let rec traverse (cur : Trafo3d) (inliers : MapExt<TrackId, MapExt<CameraId * CameraId, bool>>) (parent : int) (remaining : RoseTree<_>) =
            
            let mkInliers inl =
                let mutable super = inliers
                for KeyValue(tid, (cidSelf,cidParent,isInlier)) in inl do
                    let newMap = [ (cidSelf,cidParent),isInlier ] |> MapExt.ofList
                    super <- super |> MapExt.alter tid ( fun map ->
                                        match map with
                                        | None -> Some newMap
                                        | Some m -> Some (newMap |> MapExt.union m)
                                        )
                super
                

            let scaleTrafo (src : CameraId) (dst : CameraId) (deltaTrafo : Trafo3d) (inliers : MapExt<TrackId,(CameraId*CameraId*bool)>) : Trafo3d = 
                
                if solved.Count = 0 then
                    solved.[src] <- rootCam
                    solved.[dst] <- rootCam.Transformed deltaTrafo
                    deltaTrafo
                else
                    assert(solved.Count >= 2)

                    let srcCam = solved.[src]

                    let currentTracks =
                        measurements |> MapExt.filter (fun cid _ -> solved.ContainsKey cid) |> Measurements.toTracks

                    let estimatePoints =
                        pointsFromCams currentTracks (fun cid -> solved.[cid].GetRay)

                    let visiblePoints =
                        measurements.[dst] 
                            |> MapExt.choose (fun k pp -> 
                                MapExt.tryFind k estimatePoints 
                                    |> Option.defaultValue None 
                                    |> Option.map (fun p3 -> pp, p3)
                            )
                            |> MapExt.filter ( fun tid _ ->
                                let (_,_,inl) = inliers.[tid]
                                inl
                            )

                    // fw = V3d.OOI
                    

                    // srcRot * (p - srcPos)

                    // project(p) = 
                    //   let a = dstRot * srcRot * p - dstRot * srcRot * srcPos - scale * dstPos
                    //   a.XY / a.Z
                    
                    // srcCam.Transformed((R | s * t)).Project(p3) = pp

                    //let dstTrafo = srcTrafo * deltaTrafo

                    let dstCam = srcCam.Transformed deltaTrafo
                    let r = (dstCam.AngleAxis |> AngleAxis.Trafo).Forward
                    let t = dstCam.Position

                    let piscis = visiblePoints |> MapExt.toList |> List.map (snd)

                    let sX pi (ci : V2d) : float =
                        (ci.X * (r.TransformPos pi).Z - (r.TransformPos pi).X) / (t.X * ci.X * t.Z)
                        
                    let sY pi (ci : V2d) : float =
                        (ci.Y * (r.TransformPos pi).Z - (r.TransformPos pi).Y) / (t.Y * ci.Y * t.Z)

                    let scaleRaw = piscis |> List.map ( fun (ci,pi) -> (sX pi ci, sY pi ci) ) |> List.toArray
                                       
                    let scale = scaleRaw |> Array.toList |> List.collect ( fun (x,y) -> [x;y] ) |> List.average

                    let r = M44d.op_Explicit (deltaTrafo.Forward.UpperLeftM33())
                    let t = deltaTrafo.Forward.C3 * scale
                    
                    Log.line "[Trafo] Distance between Cam %A and %A: %A" src.Id dst.Id scale

                    let realDeltaTrafo = Trafo3d.FromBasis(r.C0.XYZ,r.C1.XYZ,r.C2.XYZ,t.XYZ)

                    let (srcTrafo,_) = solved.[src].ViewAndProjTrafo 123123123.0

                    let dstTrafo = srcTrafo * realDeltaTrafo

                    solved.[dst] <- rootCam.Transformed dstTrafo

                    realDeltaTrafo
                

            match remaining with
            | RoseTree.Empty -> 
                MapExt.empty,RoseTree.Empty
            | RoseTree.Leaf ci ->
                let (inl, trafo) = register ci parent
                
                let trafo = cur * (scaleTrafo !parent !ci trafo inl)

                let inliers = mkInliers inl
                inliers, RoseTree.Leaf(ci, trafo)
            | RoseTree.Node (ci, children) ->
                let (inl, trafo) = register ci parent
                let inliers = mkInliers inl
                
                let trafo = cur * (scaleTrafo !parent !ci trafo inl)
                
                let (inliers, children) = 
                    let oo = children |> List.map (traverse trafo inliers ci)
                    (oo |> List.map fst 
                        |> List.fold (MapExt.unionWith (fun l r -> l |> MapExt.unionWith (fun a b -> a && b) r)) MapExt.empty),
                     oo |> List.map snd
                inliers, RoseTree.Node((ci, trafo), children )

        let (inliers, trafoTree) = 
            match cameraTree with
            | RoseTree.Empty -> MapExt.empty, RoseTree.Empty
            | RoseTree.Leaf i ->
                MapExt.empty, RoseTree.Leaf(i, Trafo3d.Identity)
            | RoseTree.Node(i, children) ->
                let trafo = Trafo3d.Identity
                let (inliers, children) = 
                    let oo = children |> List.map (traverse trafo MapExt.empty i)
                    (oo |> List.map fst 
                        |> List.fold (MapExt.unionWith (fun l r -> l |> MapExt.unionWith (&&) r)) MapExt.empty), 
                     oo |> List.map snd
                inliers, RoseTree.Node((i, trafo), children)
            
        inliers,
        trafoTree 
            |> RoseTree.toList
            |> List.map ( fun (ci,trafo) -> 
                 let oc = Camera3d.LookAt(V3d.OIO * 5.0, V3d.Zero, 1.0, V3d.OOI)
                 let forward = AngleAxis.RotatePoint(-oc.AngleAxis, -V3d.OOI)

                 let trafo2 = (Trafo3d.Rotation(forward, -Math.PI)) * trafo
                 
                 let nc = oc.Transformed trafo2
                 let testc = oc.Transformed trafo

                 Log.line "=============="
                 Log.line "[Trafo] Cid: %A" ci
                 Log.line "[Trafo] Original Camera: \n%A \n%A" testc.Position testc.AngleAxis
                 Log.line "[Trafo] Original Forward: %A" forward
                 Log.line "[Trafo] Trafo: \n%A" trafo
                 Log.line "[Trafo] Trafo2: \n%A" trafo2
                 Log.line "[Trafo] New Camera: \n%A \n%A" nc.Position nc.AngleAxis
                 Log.line "=============="

                 ci, nc
                )
         
