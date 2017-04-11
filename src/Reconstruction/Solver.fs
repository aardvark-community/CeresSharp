namespace Aardvark.Reconstruction

open System
open CeresSharp

open Aardvark.Base

type Fixations =
    {
        fixedPoints : MapExt<TrackId, bool>
        fixedCams   : MapExt<CameraId,bool>
    }

type Fixing =
    | EverythingFree
    | PointsFixedCamsFree
    | CamsFixedPointsFree
    | FixationMap of Fixations

type SolverConfig =
    {
        ParameterFixing : Fixing
    }

module Solver =

    let pointIsFixed (tid : TrackId) (fixations : Fixing) =
        match fixations with
        | EverythingFree | CamsFixedPointsFree -> false
        | FixationMap f -> f.fixedPoints.[tid]
        | _ -> true

    let cameraIsFixed (cid : CameraId) (fixations : Fixing) =
        match fixations with
        | EverythingFree | PointsFixedCamsFree -> false
        | FixationMap f -> f.fixedCams.[cid]
        | _ -> true

    let bundleAdjust 
              (options : CeresOptions) (config : SolverConfig) (points : MapExt<TrackId, V3d>) (cameras : MapExt<CameraId, Camera3d>) 
              (observations : MapExt<TrackId,MapExt<CameraId,V2d>>)
                : float * points : MapExt<TrackId, V3d> * MapExt<CameraId, Camera3d> =

        use problem = new Problem()

        let pointBlocks = 
            points |> MapExt.choose ( fun tid p ->
                        if not (pointIsFixed tid config.ParameterFixing) then Some (problem.AddParameterBlock [| p |]) else None
                      )

        let camBlocks   = 
             cameras |> MapExt.choose ( fun cid c ->
                        if not (cameraIsFixed cid config.ParameterFixing) then Some (problem.AddParameterBlock<Camera3d,Camera3s> [| c |]) else None
                      )
                      
        let costFunction (real : V2d) (obs : V2s, depth : scalar) =
            let diff = real - obs
                    
            let depth = depth * depth
                    
            [|
                diff.X 
                diff.Y 
            |]

        for i in 0 .. prob.input.tracks.Length-1 do
            
            if pointIdx |> Array.contains i then
                let pi = pointIdx.[i]
                let pb = pointBlocks.[pi]
                let pw = pointWeights.[pi]
                let track = prob.input.tracks.[i]

                for (ci,lpi) in track do
                    let real = measurements.[ci].[lpi]
                    
                    let costFunction = costFunction real

                    let numResiduals =
                        costFunction (V2s.OO, scalar 0.0) |> Array.length

                    if camIdx |> Array.contains ci then
                        let cb = camBlocks.[camIdx |> indexOf ci]
                    
                        p.AddCostFunction(numResiduals, pb, cb, fun point cam ->
                            let obs = cam.[0].ProjectWithDepth point.[0]
                            costFunction obs
                        )
                
                    elif camFixedIdx |> Array.contains ci then
                        let cf = camFixed.[camFixedIdx |> indexOf ci]
                    
                        p.AddCostFunction(numResiduals, pb, fun point ->
                                let obs = point.[0] |> V3s.getProjectedByWithDepth cf.[0]
                                costFunction obs
                            )
                    else    
                        Log.error "CAN NOT HAPPEN!!!!!!!!!"

            elif fixedpointIdx |> Array.contains i then
                let pi = fixedpointIdx.[i]
                let pb = fixedPointb.[pi]
                let pw = pointWeights.[pi]
                let track = prob.input.tracks.[i]

                for (ci,lpi) in track do
                    let real = measurements.[ci].[lpi]
                
                    let costFunction = costFunction real

                    let numResiduals =
                        costFunction (V2s.OO, scalar 0.0) |> Array.length

                    if camIdx |> Array.contains ci then
                        let cb = camBlocks.[camIdx |> indexOf ci]
                    
                        p.AddCostFunction(numResiduals, cb, fun cam ->
                            let obs = cam.[0].ProjectWithDepth (V3s pb.[0])
                            costFunction obs
                        )
                
                    elif camFixedIdx |> Array.contains ci then
                        //failwith "dont do this"
                        ()
                    else    
                        Log.error "CAN NOT HAPPEN!!!!!!!!!"
            else failwith "can't happen"

        let cost = p.Solve(options)

        let (points, cameras) = getCurrentPointsCams()

        cost, points, cameras