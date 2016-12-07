namespace Aardvark.Ceres

open System
open Aardvark.Base
open CeresSharp


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Bundler =
    open CeresSharp

    let private rand = RandomSystem()

    let improve (sol : BundlerSolution) =
        Log.startTimed "improving"
        let input = sol.problem
        use p = new Problem()
        let guessedPoints   : V3d[] = sol.points |> Array.copy
        let guessedCameras  : Camera3d[][] = sol.cameras |> Array.map (fun c -> [| c |])

        let worldPoints     = p.AddParameterBlock guessedPoints
        let camBlocks       = guessedCameras |> Array.map (fun c -> p.AddParameterBlock<Camera3d, Camera3s>(c))

        for ci in 0 .. guessedCameras.Length - 1 do
            let measurements = input.measurements.[ci] |> Seq.toArray
            let residuals = 2 * measurements.Length
            let res = Array.zeroCreate residuals

            p.AddCostFunction(residuals, worldPoints, camBlocks.[ci], fun world cam ->
                let cam = cam.[0]
                let mutable oi = 0
                for kvp in measurements do
                    let obs = cam.Project world.[kvp.Key]
                    let r = obs - kvp.Value
                    res.[oi + 0] <- r.X 
                    res.[oi + 1] <- r.Y 
                    oi <- oi + 2

                res
            )
        

        let maxCost = 1.0E-3 * float camBlocks.Length
        let cost = p.Solve(CeresOptions(400, CeresSolverType.SparseSchur, false, 1.0E-10, 1.0E-3, 1.0E-5))

        Log.line "new cost: %.4f" cost

        let points = worldPoints.Result
        let cameras = camBlocks |> Array.map (fun b -> b.Result.[0])

        Log.stop()
        { problem = input; cameras = cameras; points = points }

    let improveSubSolution (sol : BundlerSubSolution) =
        let p = { BundlerSubProblem.problem = sol.problem; BundlerSubProblem.subCameras = sol.subCameras |> Map.toSeq |> Seq.map fst |> Set.ofSeq }
        let parent = p.problem
        let cForward = p.subCameras |> Set.toArray

        let mutable cameraCount = 0
        let cameraIndices = Array.create parent.realCameras -1

        let subMeasurements =
            parent.measurements
                |> Array.filteri (fun ci _ -> 
                    if Set.contains ci p.subCameras then
                        cameraIndices.[cameraCount] <- ci
                        cameraCount <- cameraCount + 1
                        true
                    else
                        false      
                )

        let subPoints =
            subMeasurements
                |> Seq.collect (Map.toSeq >> Seq.map fst)
                |> Set.ofSeq
                |> Set.toArray

        let innerPointIndices =
            let arr = Array.zeroCreate parent.realPoints
            for i in subPoints do
                arr.[i] <- 1
            Array.scan (+) 0 arr

        let subMeasurements =
            subMeasurements
                |> Array.map (fun m ->
                    m |> Map.toSeq |> Seq.map (fun (pi, v) -> innerPointIndices.[pi], v) |> Map.ofSeq
                )

        let innerProblem = { realCameras = subMeasurements.Length; realPoints = subPoints.Length; measurements = subMeasurements }
        let innerSolution = { problem = innerProblem; points = sol.subPoints |> Map.toSeq |> Seq.map snd |> Seq.toArray; cameras = sol.subCameras |> Map.toSeq |> Seq.map snd |> Seq.toArray }

        let innerSol = improve innerSolution

        {
            problem = parent
            subPoints = innerSol.points |> Seq.mapi (fun i p -> subPoints.[i], p) |> Map.ofSeq
            subCameras = innerSol.cameras |> Seq.mapi (fun i c -> cameraIndices.[i], c) |> Map.ofSeq
        }

    let solve (input : BundlerProblem) =
        Log.startTimed "bundling %d cameras" input.realCameras
        use p = new Problem()
        let guessedPoints   : V3d[] = Array.zeroCreate input.realPoints
        let guessedCameras  : Camera3d[][] = Array.init input.realCameras (fun _ -> Array.zeroCreate 1)
        let worldPoints     = p.AddParameterBlock guessedPoints
        let camBlocks       = guessedCameras |> Array.map (fun c -> p.AddParameterBlock<Camera3d, Camera3s>(c))

        for ci in 0 .. guessedCameras.Length - 1 do
            let measurements = input.measurements.[ci] |> Seq.toArray
            let residuals = 2 * measurements.Length
            let res = Array.zeroCreate residuals

            p.AddCostFunction(residuals, worldPoints, camBlocks.[ci], fun world cam ->
                let cam = cam.[0]
                let mutable oi = 0
                for kvp in measurements do
                    let obs = cam.Project world.[kvp.Key]
                    let r = obs - kvp.Value
                    res.[oi + 0] <- r.X 
                    res.[oi + 1] <- r.Y 
                    oi <- oi + 2

                res
            )
        
        let bounds = Box3d(-V3d.III, V3d.III)
        let mutable cost = Double.PositiveInfinity

        let maxCost = 1.0E-4 * float camBlocks.Length

        while cost > maxCost do
            guessedPoints.SetByIndex(fun i -> rand.UniformV3d(bounds)) |> ignore
            for i in 0 .. guessedCameras.Length - 1 do
                let v = Camera3d.LookAt(rand.UniformV3dDirection() * 10.0, rand.UniformV3dDirection(), 1.0,V3d.OOI)
                guessedCameras.[i].[0] <- v

            cost <- p.Solve(CeresOptions(600, CeresSolverType.SparseSchur, false, 1.0E-10, 1.0E-3, 1.0E-5))
            //if cost > maxCost then Log.warn "retry (cost: %.3f)" cost

        let points = worldPoints.Result
        let cameras = camBlocks |> Array.map (fun b -> b.Result.[0])

        Log.stop()

        { problem = input; cameras = cameras; points = points }

    let solveSubProblem (p : BundlerSubProblem) =
        let parent = p.problem
        let cForward = p.subCameras |> Set.toArray

        let mutable cameraCount = 0
        let cameraIndices = Array.create parent.realCameras -1

        let subMeasurements =
            parent.measurements
                |> Array.filteri (fun ci _ -> 
                    if Set.contains ci p.subCameras then
                        cameraIndices.[cameraCount] <- ci
                        cameraCount <- cameraCount + 1
                        true
                    else
                        false      
                )

        let subPoints =
            subMeasurements
                |> Seq.collect (Map.toSeq >> Seq.map fst)
                |> Set.ofSeq
                |> Set.toArray

        let innerPointIndices =
            let arr = Array.zeroCreate parent.realPoints
            for i in subPoints do
                arr.[i] <- 1
            Array.scan (+) 0 arr

        let subMeasurements =
            subMeasurements
                |> Array.map (fun m ->
                    m |> Map.toSeq |> Seq.map (fun (pi, v) -> innerPointIndices.[pi], v) |> Map.ofSeq
                )

        let innerProblem = { realCameras = subMeasurements.Length; realPoints = subPoints.Length; measurements = subMeasurements }
        let innerSol = solve innerProblem

        {
            problem = parent
            subPoints = innerSol.points |> Seq.mapi (fun i p -> subPoints.[i], p) |> Map.ofSeq
            subCameras = innerSol.cameras |> Seq.mapi (fun i c -> cameraIndices.[i], c) |> Map.ofSeq
        }



    let rec tryRegisterCameras (l : Map<int, V2d>) (r : Map<int, V2d>) =
        let points = Map.intersect l r

        if points.Count < 7 then
            Log.warn "cannot register cameras sharing less than 7 points"
            None
        else
            let correspondences =
                points 
                    |> Map.toSeq
                    |> Seq.map snd
                    |> Seq.toArray

            use p = new Problem()

            use pts = p.AddParameterBlock(Array.init correspondences.Length (fun _ -> rand.UniformV3dDirection()))
            use cams = p.AddParameterBlock<Camera3d, Camera3s>(Array.init 2 (fun _ -> Camera3d.LookAt(20.0 * rand.UniformV3dDirection(), V3d.Zero, 1.0, V3d.OOI)))

            //let c0 = Camera3d(V3d.Zero, V3d.Zero, 1.0)
            let residuals = Array.zeroCreate (4*correspondences.Length)
            p.AddCostFunction(residuals.Length, pts, cams, fun pts cams ->
                let c0 = cams.[0] //Camera3s(V3s(c0.Position), V3s(c0.AngleAxis), scalar c0.SqrtFocalLength)
                let c1 = cams.[1]

                let mutable oi = 0
                for i in 0 .. correspondences.Length - 1 do
                    let p0, p1 = correspondences.[i]
                    let p = pts.[i]

                    let r0 = c0.Project p - p0
                    let r1 = c1.Project p - p1

                    residuals.[oi+0] <- r0.X
                    residuals.[oi+1] <- r0.Y
                    residuals.[oi+2] <- r1.X
                    residuals.[oi+3] <- r1.Y

                    oi <- oi + 4

                residuals
            )


            if p.Solve(CeresOptions(200, CeresSolverType.SparseSchur, true, 1.0E-8, 1.0E-8, 1.0E-8)) < 1.0E-4 then
                let pts = pts.Result
                let cams = cams.Result

                let trafo = Camera3d.Delta(cams.[0], Camera3d(V3d.Zero, V3d.Zero, 1.0))

                let cam = cams.[1].Transformed(trafo)
                let points = points |> Map.toSeq |> Seq.mapi (fun i (pi,_) -> pi, trafo.Forward.TransformPos pts.[i]) |> Map.ofSeq

                let error = points |> Map.toSeq |> Seq.map (fun (pi, p) -> cam.Project p - r.[pi]) |> Seq.fold (fun err v -> err + Vec.lengthSquared v) 0.0
                Log.warn "err: %A" (0.5 * error)

                Some (cam, points)
            else
                Log.warn "retry"
                tryRegisterCameras l r
