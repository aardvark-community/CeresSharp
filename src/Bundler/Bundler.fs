namespace Aardvark.Ceres

open System
open Aardvark.Base
open CeresSharp


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Bundler =
    open CeresSharp

    let private rand = RandomSystem()

    let private improveInternal (useDistortion : bool) (options : CeresOptions) (points : V3d[]) (k0 : float) (k1 : float) (cameras : Camera3d[]) (measurements : Map<int,Map<int, V2d>>) =
        use p = new Problem()
        let guessedPoints   : V3d[] = points |> Array.copy
        let guessedCameras  : Map<int,Camera3d[]> =
            [|
                let mutable cc = 0
                for kvp in measurements do
                    let ci = kvp.Key
                    let cam = [| cameras.[cc] |]
                    yield ci, cam
                    cc <- cc+1
            |] |> Map.ofArray

        let worldPoints     = p.AddParameterBlock guessedPoints
        let camBlocks       = guessedCameras |> Map.map (fun _ c -> p.AddParameterBlock<Camera3d, Camera3s>(c))
        
        if useDistortion then

            for kvp in measurements do
                let ci = kvp.Key
                let measurements = measurements.[ci] |> Seq.toArray
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
            let cost = p.Solve(options)

            let points = worldPoints.Result
            let cameras = camBlocks |> Map.map (fun k b -> b.Result.[0])

            cost, points, cameras
        else
            let k0k1 = p.AddParameterBlock [| k0; k1 |]
            
            for kvp in measurements do
                let ci = kvp.Key
                let measurements = measurements.[ci] |> Seq.toArray
                let residuals = 2 * measurements.Length
                let res = Array.zeroCreate residuals

                p.AddCostFunction(residuals, worldPoints, k0k1, camBlocks.[ci], fun world ks cam ->
                    let cam = cam.[0]
                    let mutable oi = 0
                    for kvp in measurements do
                        let obs = cam.ProjectUniformDistortion(world.[kvp.Key], ks.[0], ks.[1])

                        let r = obs - kvp.Value
                        res.[oi + 0] <- r.X 
                        res.[oi + 1] <- r.Y 
                        oi <- oi + 2

                    res
                )
            let cost = p.Solve(options)

            let points = worldPoints.Result
            let cameras = 
                camBlocks |> Map.map (fun k b -> let c = b.Result.[0] in Camera3d(c.Position, c.AngleAxis, c.SqrtFocalLength, V2d(k0k1.Result.[0], k0k1.Result.[1]))
                )

            cost, points, cameras


    let private improvePointCloudAffine (useDistortion : bool) (options : CeresOptions) (knownPoints : Map<int,V3d>) (newCamera : Camera3d) (measurements : Map<int, V2d>) =
        use p = new Problem()

        let pcScale     = p.AddParameterBlock [| V3d.III |]
        let camBlock    = p.AddParameterBlock<Camera3d, Camera3s>([| newCamera |]) 

        let residuals = 2 * knownPoints.Count
        let res = Array.zeroCreate residuals

        p.AddCostFunction(residuals, pcScale, camBlock, fun scale cam ->
            let cam = cam.[0]
            let mutable oi = 0
            for kvp in knownPoints do
                let obs = 
                    let p = scale.[0] * kvp.Value
                    if useDistortion then cam.Project p
                    else cam.ProjectNoDistortion p

                let r = obs - measurements.[kvp.Key]
                res.[oi + 0] <- r.X 
                res.[oi + 1] <- r.Y 
                oi <- oi + 2

            res
        )

        let cost = p.Solve(options)

        let newScale = pcScale.Result.[0]
        let newCamera = camBlock.Result.[0]

        cost, newScale, newCamera

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

    let solve (useDistortion : bool) (p : BundlerProblem) =
        Log.startTimed "solve %d cameras" p.cameras.Count
        printfn " "
        let options = CeresOptions(700, CeresSolverType.SparseSchur, true, 1.0E-10, 1.0E-4, 1.0E-6)
        let measurementCount = p.cameras |> Seq.sumBy (fun ci -> p.input.measurements.[ci].Count)

        let tinyCost = 1.0E-4 * float measurementCount

        let mutable bestCost = Double.PositiveInfinity
        let mutable best : Option<BundlerSolution> = None
        let mutable iter = 0
        while iter < 4 && bestCost > tinyCost do
            let n = improveSol useDistortion options (BundlerSolution.random p)
            if n.cost < bestCost then
                bestCost <- n.cost
                best <- Some n
                Log.line "%d: %.3f" iter n.cost
            iter <- iter + 1

        Log.stop()
        Option.get best

    let solveTowardsKnown (known : BundlerSolution) (unknown : BundlerProblem) = 
        Log.startTimed "Adding a camera %A to solution" (unknown.cameras |> Set.toArray).[0]
        let options = CeresOptions(700, CeresSolverType.SparseSchur, false, 1.0E-10, 1.0E-4, 1.0E-6)
        
        let cam = Camera3d.LookAt(rand.UniformV3dDirection() * 10.0, V3d.Zero, 1.0, V3d.OOI)

        let unknownMeasurements = unknown.input.measurements.[(unknown.cameras |> Set.toArray).[0]]
        let remainingKnown = 
            let n = unknownMeasurements
            let k = known.points

            let remainingKnown =
                [
                    for kvp in n do
                        match k |> Map.tryFind kvp.Key with
                        | Some value -> yield kvp.Key,value
                        | _ -> ()
                ] |> Map.ofList
            
            Log.line "points common with established cloud: %A -> %A" n.Count remainingKnown.Count

            remainingKnown

        let (cost, scaleForKnown, newCam) = improvePointCloudAffine true options remainingKnown cam unknownMeasurements
        
        let scaledPoints =
            known.points |> Map.map ( fun _ v -> scaleForKnown * v)

        let scaledCam = 
            known.cameras |> Map.map ( fun _ cam -> cam.Transformed(Trafo3d.Scale scaleForKnown) )

        let combinedSolution =
            {
                cost = cost
                problem = { unknown with input ={ unknown.input with measurements = [(unknown.cameras |> Set.toArray).[0],[ for kvp in remainingKnown do match unknownMeasurements.TryFind kvp.Key with Some v -> yield kvp.Key,v | _ -> () ] |> Map.ofList ] |> Map.ofList }}
                points = scaledPoints
                cameras = scaledCam |> Map.union ( [ (unknown.cameras |> Set.toArray).[0], newCam ] |> Map.ofList )
            }

        Log.stop()

        combinedSolution
    
