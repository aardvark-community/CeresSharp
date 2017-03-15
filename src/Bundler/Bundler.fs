namespace Aardvark.Ceres

open System
open Aardvark.Base
open CeresSharp


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Bundler =
    open CeresSharp

    let private rand = RandomSystem()

    let private improveInternal (useDistortion : bool) (options : CeresOptions) 
                                (points : V3d[]) (k0 : float) (k1 : float) 
                                (cameras : (Camera3d*bool)[]) (measurements : Map<int,Map<int, V2d>>) : float * V3d[] * Map<int,(Camera3d*bool)> =
        use p = new Problem()
        let guessedPoints   : V3d[] = points |> Array.copy
        let guessedCameras  : Map<int,(Camera3d*bool)[]> =
            [|
                let mutable cc = 0
                for kvp in measurements do
                    let ci = kvp.Key
                    let cam = [| cameras.[cc] |]
                    yield ci, cam
                    cc <- cc+1
            |] |> Map.ofArray

        let worldPoints     = p.AddParameterBlock guessedPoints
        let camBlocks       = guessedCameras 
                                |> Map.filter (fun _ o -> let (_,f) = o |> Array.head in not f) 
                                |> Map.map (fun _ c -> c |> Array.map fst )
                                |> Map.map (fun _ c -> p.AddParameterBlock<Camera3d, Camera3s>(c))

        let camFixed        = guessedCameras
                                |> Map.filter (fun _ o -> let (_,f) = o |> Array.head in f) 
                                |> Map.map (fun _ c -> c |> Array.map fst )
        
        for kvp in measurements do
            let ci = kvp.Key
            let measurements = measurements.[ci] |> Seq.toArray
            let residuals = 2 * measurements.Length
            let res = Array.zeroCreate residuals

            match camBlocks |> Map.tryFind ci, camFixed |> Map.tryFind ci with
            | Some cb, None ->  //this camera is a free variable
                p.AddCostFunction(residuals, worldPoints, cb, fun world cam ->
                    let cam = cam.[0]
                    let mutable oi = 0
                    for kvp in measurements do
                        let obs = cam.Project(world.[kvp.Key])

                        let r = obs - kvp.Value
                        res.[oi + 0] <- r.X 
                        res.[oi + 1] <- r.Y 
                        oi <- oi + 2

                    res
                )

            | None, Some cam ->  //this camera is fixed.
                let cam = cam.[0]
                p.AddCostFunction(residuals, worldPoints, fun world ->
                    let mutable oi = 0
                    for kvp in measurements do
                        let obs = world.[kvp.Key] |> V3s.getProjectedBy cam

                        let r = obs - kvp.Value
                        res.[oi + 0] <- r.X 
                        res.[oi + 1] <- r.Y 
                        oi <- oi + 2

                    res
                )

            | _ -> 
                failwith "can not happen"

        let cost = p.Solve(options)

        let points = worldPoints.Result
        let cameras = 
            let cb = camBlocks |> Map.map (fun k b -> 
                                        let c = b.Result.[0]  
                                        Camera3d(c.Position, c.AngleAxis, c.FocalLength),false)
            let cf = (camFixed |> Map.map (fun _ cb -> cb.[0],true))

            Map.union cb cf

        cost, points, cameras

    let private improveSol (useDistortion : bool) (options : CeresOptions) (sol : BundlerSolution) =
        let parent = sol.problem
        let input = parent.input

        let subMeasurements =
            input.measurements
                |> Map.filter (fun ci _ -> Map.containsKey ci sol.cameras)

        let subPoints =
            subMeasurements
                |> Map.toSeq
                |> Seq.map snd
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
                |> Map.map (fun _ m ->
                    m |> Map.toSeq |> Seq.map (fun (pi, v) -> innerPointIndices.[pi], v) |> Map.ofSeq
                )

        let cost, points, cameras =  
            let points = sol.points |> Map.toSeq |> Seq.map snd |> Seq.toArray
            let cameras = sol.cameras |> Map.toSeq |> Seq.map snd |> Seq.toArray
            let k1 =  0.0
            let k2 =  0.0
            improveInternal useDistortion options points k1 k2 cameras subMeasurements

        {
            cost = cost
            problem = parent
            points = points |> Seq.mapi (fun i p -> subPoints.[i], p) |> Map.ofSeq
            cameras = cameras
        }

    let solve (useDistortion : bool) (p : BundlerProblem) =
        Log.startTimed "solve %d cameras" p.cameras.Count
        printfn " "
        let options = CeresOptions(700, CeresSolverType.SparseSchur, true, 1.0E-10, 1.0E-5, 1.0E-5)
        let measurementCount = p.cameras |> Seq.sumBy (fun ci -> p.input.measurements.[ci].Count)

        let tinyCost = 1.0E-6 * float measurementCount

        let mutable bestCost = Double.PositiveInfinity
        let mutable best : Option<BundlerSolution> = None
        let mutable iter = 0
        while iter < 1 && bestCost > tinyCost do    //only one iteration with non-random starting values
            let n = improveSol useDistortion options (BundlerSolution.random p)
            if n.cost < bestCost then
                bestCost <- n.cost
                best <- Some n
                Log.line "%d: %.3f" iter n.cost
            iter <- iter + 1

        Log.stop()
        Option.get best
    
