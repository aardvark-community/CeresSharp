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
        Adorn : MapExt<TrackId, V3d> -> MapExt<CameraId, Camera3d> -> unit
        ParameterFixing : Fixing
    }

module SolverConfig =
    
    let allFree = { Adorn = () |> constF |> constF; ParameterFixing = EverythingFree }

module Solver =
    open System.Runtime.InteropServices

    let bundleAdjust 
              (options : CeresOptions) (config : SolverConfig) (points : MapExt<TrackId, V3d>) (cameras : MapExt<CameraId, Camera3d>) 
              (tracks : MapExt<TrackId,MapExt<CameraId,V2d>>)
                : float * MapExt<TrackId, V3d> * MapExt<CameraId, Camera3d> =
        match points.Count < 5, cameras.Count < 3 with
        | true, false -> Log.error "Not enough Tracks! Only %A (need %A). No bundle adjustment possible." points.Count 4; Double.PositiveInfinity, points, cameras
        | false, true -> Log.error "Not enough Cameras! Only %A (need %A). No bundle adjustment possible." cameras.Count 3; Double.PositiveInfinity, points, cameras
        | true, true  -> Log.error "Not enough Cameras and Points! Only c=%A t=%A (need c=%A t=%A). No bundle adjustment possible." cameras.Count points.Count 3 4; Double.PositiveInfinity, points, cameras
        | false, false->
            let pointIsFixed (tid : TrackId) =
                match config.ParameterFixing with
                | EverythingFree | CamsFixedPointsFree -> false
                | FixationMap f -> f.fixedPoints.[tid]
                | _ -> true

            let cameraIsFixed (cid : CameraId) =
                match config.ParameterFixing with
                | EverythingFree | PointsFixedCamsFree -> false
                | FixationMap f -> f.fixedCams.[cid]
                | _ -> true

            
            let getResult intermediary (pointBlocks : MapExt<TrackId, IParameterBlock<V3d,V3s>>) (camBlocks : MapExt<CameraId, IParameterBlock<Camera3d,Camera3s>>) =

                let points = 
                    pointBlocks |> MapExt.map ( fun _ pb -> pb.GetResult(intermediary).[0] )
                                |> MapExt.union ( points |> MapExt.filter ( fun tid _ -> pointIsFixed tid ) )

                let cameras =
                    camBlocks |> MapExt.map ( fun _ cb -> cb.GetResult(intermediary).[0] )
                              |> MapExt.union ( cameras |> MapExt.filter ( fun cid _ -> cameraIsFixed cid ) ) 
                
                points, cameras

            use problem = new Problem()

            let pointBlocks = 
                points |> MapExt.choose ( fun tid p ->
                            if not (pointIsFixed tid) then Some (problem.AddParameterBlock [| p |]) else None
                          )

            let camBlocks   = 
                 cameras |> MapExt.choose ( fun cid c ->
                            if not (cameraIsFixed cid) then Some (problem.AddParameterBlock<Camera3d,Camera3s> [| c |]) else None
                          )
             
            let costFunction (real : V2d) (obs : V2s) =
                let diff = real - obs
                [|
                    diff.X 
                    diff.Y 
                |]

            let numResiduals = costFunction V2d.OO V2s.OO |> Array.length


            let mutable adornCounter = 0
            let paramCount = tracks |> MapExt.toSeq |> Seq.map snd |> Seq.sumBy (fun o -> o.Count)
            let readAndAdorn () =
                adornCounter <- adornCounter + 1
                if adornCounter > paramCount then
                    let (points,cameras) = getResult true pointBlocks camBlocks
                    config.Adorn points cameras
                    adornCounter <- 0

            let costFunctionCamPoint (real : V2d) (pb : IParameterBlock<V3d,V3s>) (cb : IParameterBlock<Camera3d,Camera3s>) =
                problem.AddCostFunction(numResiduals, pb, cb, fun point cam ->
                    let obs = cam.[0].Project point.[0]
                    let res = costFunction real obs
                    
                    readAndAdorn ()

                    res
                )

            let costFunctionCamOnly (real : V2d) (point : V3d) (cb : IParameterBlock<Camera3d,Camera3s>) =
                problem.AddCostFunction(numResiduals, cb, fun cam ->
                    let obs = cam.[0].Project (V3s point)
                    let res = costFunction real obs
                    
                    readAndAdorn ()

                    res
                )

            let costFunctionPointOnly (real : V2d) (pb : IParameterBlock<V3d,V3s>) (cam : Camera3d) =
                problem.AddCostFunction(numResiduals, pb, fun point ->
                    let obs = point.[0] |> V3s.getProjectedBy cam
                    let res = costFunction real obs
                    
                    readAndAdorn ()

                    res
                )

            for KeyValue(tid, track) in tracks do
            
                for KeyValue(cid, obs) in track do
                
                    match pointIsFixed tid, cameraIsFixed cid with
                    | false, false -> costFunctionCamPoint  obs pointBlocks.[tid] camBlocks.[cid]
                    | true,  false -> costFunctionCamOnly   obs points.[tid]      camBlocks.[cid]
                    | false, true  -> costFunctionPointOnly obs pointBlocks.[tid] cameras.[cid]
                    | true,  true  -> ()

            let cost = problem.Solve(options)
        
            let (points,cameras) = getResult false pointBlocks camBlocks

            cost, points, cameras