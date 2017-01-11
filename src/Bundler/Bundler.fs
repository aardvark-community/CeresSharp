namespace Aardvark.Ceres

open System
open Aardvark.Base
open CeresSharp


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Bundler =
    open CeresSharp

    let private rand = RandomSystem()

    let private improveInternal (useDistortion : bool) (options : CeresOptions) (points : V3d[]) (cameras : Camera3d[]) (measurements : array<Map<int, V2d>>) =
        use p = new Problem()
        let guessedPoints   : V3d[] = points |> Array.copy
        let guessedCameras  : Camera3d[][] = cameras |> Array.map (fun c -> [| c |])

        let worldPoints     = p.AddParameterBlock guessedPoints
        let camBlocks       = guessedCameras |> Array.map (fun c -> p.AddParameterBlock<Camera3d, Camera3s>(c))

        for ci in 0 .. guessedCameras.Length - 1 do
            let measurements = measurements.[ci] |> Seq.toArray
            let residuals = 2 * measurements.Length
            let res = Array.zeroCreate residuals

            p.AddCostFunction(residuals, worldPoints, camBlocks.[ci], fun world cam ->
                let cam = cam.[0]
                let mutable oi = 0
                for kvp in measurements do
                    let obs =
                        if useDistortion then cam.Project world.[kvp.Key]
                        else cam.ProjectNoDistortion world.[kvp.Key]

                    let r = obs - kvp.Value
                    res.[oi + 0] <- r.X 
                    res.[oi + 1] <- r.Y 
                    oi <- oi + 2

                res
            )
        

        let cost = p.Solve(options)

        let points = worldPoints.Result
        let cameras = camBlocks |> Array.map (fun b -> b.Result.[0])

        cost, points, cameras

    let private improveSol (useDistortion : bool) (options : CeresOptions) (sol : BundlerSolution) =
        let parent = sol.problem
        let input = parent.input
        let cForward = sol.cameras |> Map.toSeq |> Seq.map fst |> Seq.toArray

        let mutable cameraCount = 0
        let cameraIndices = Set.toArray sol.problem.cameras

        let subMeasurements =
            input.measurements
                |> Array.filteri (fun ci _ -> Map.containsKey ci sol.cameras)

        let subPoints =
            subMeasurements
                |> Seq.collect (Map.toSeq >> Seq.map fst)
                |> Set.ofSeq
                |> Set.toArray

        let innerPointIndices =
            let count = 1 + subPoints.[subPoints.Length - 1]
            let arr = Array.zeroCreate count
            for i in subPoints do
                arr.[i] <- 1
            Array.scan (+) 0 arr

        let subMeasurements =
            subMeasurements
                |> Array.map (fun m ->
                    m |> Map.toSeq |> Seq.map (fun (pi, v) -> innerPointIndices.[pi], v) |> Map.ofSeq
                )


        let cost, points, cameras = 
            let points = sol.points |> Map.toSeq |> Seq.map snd |> Seq.toArray
            let cameras = sol.cameras |> Map.toSeq |> Seq.map snd |> Seq.toArray
            improveInternal useDistortion options points cameras subMeasurements

        {
            cost = cost
            problem = parent
            points = points |> Seq.mapi (fun i p -> subPoints.[i], p) |> Map.ofSeq
            cameras = cameras |> Seq.mapi (fun i c -> cameraIndices.[i], c) |> Map.ofSeq
        }

    let improve (sol : BundlerSolution) =
        let options = CeresOptions(400, CeresSolverType.SparseSchur, true, 1.0E-10, 1.0E-3, 1.0E-5)
        let p = sol.problem
        let measurementCount = p.cameras |> Seq.sumBy (fun ci -> p.input.measurements.[ci].Count)
        let tinyCost = 1.0E-5 * float measurementCount
        if sol.cost <= tinyCost then
            Log.startTimed "solution already optimal"
            Log.stop()
            sol
        else
            Log.startTimed "improve %d cameras" sol.cameras.Count
            let res = improveSol true options sol
            Log.line "%.4f ==> %.4f" sol.cost res.cost
            Log.stop()
            res

    let solveSimple (cnt : int) (p : BundlerProblem) =
        let options = CeresOptions(100, CeresSolverType.SparseSchur, false, 1.0E-10, 1.0E-2, 1.0E-3)
        let measurementCount = p.cameras |> Seq.sumBy (fun ci -> p.input.measurements.[ci].Count)

        let tinyCost = 1.0E-5 * float measurementCount

        let mutable bestCost = Double.PositiveInfinity
        let mutable best : Option<BundlerSolution> = None
        let mutable iter = 0
        while iter < cnt && bestCost > tinyCost do
            let n = improveSol false options (BundlerSolution.random p)
            if n.cost < bestCost then
                bestCost <- n.cost
                best <- Some n
            iter <- iter + 1

        best

    let solve (p : BundlerProblem) =
        Log.startTimed "solve %d cameras" p.cameras.Count
        let options = CeresOptions(700, CeresSolverType.SparseSchur, true, 1.0E-10, 1.0E-4, 1.0E-6)
        let measurementCount = p.cameras |> Seq.sumBy (fun ci -> p.input.measurements.[ci].Count)

        let tinyCost = 1.0E-5 * float measurementCount

        let mutable bestCost = Double.PositiveInfinity
        let mutable best : Option<BundlerSolution> = None
        let mutable iter = 0
        while iter < 8 && bestCost > tinyCost do
            let n = improveSol true options (BundlerSolution.random p)
            if n.cost < bestCost then
                bestCost <- n.cost
                best <- Some n
                Log.line "%d: %.3f" iter n.cost
            iter <- iter + 1

        Log.stop()
        Option.get best

    
