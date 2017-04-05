namespace Aardvark.Ceres

open System
open Aardvark.Base
open CeresSharp


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Bundler =
    open CeresSharp
    open OpenTK.Platform.Windows

    let private rand = RandomSystem()

    let private improveInternal (options : CeresOptions) (points : V3d[]) (cameras : Map<int,Camera3d*bool>) 
                                ( iterationAdorner : V3d[] -> Map<int,(Camera3d*bool)> -> unit )
                                (measurements : Map<int,Map<int, V2d>>) : float * V3d[] * Map<int,Camera3d*bool> =
        use p = new Problem()
        let guessedPoints   : V3d[] = points |> Array.copy
        let guessedCameras  : Map<int,(Camera3d*bool)[]> =
            [|
                for kvp in measurements do
                    let ci = kvp.Key
                    let cam = [| if cameras.ContainsKey ci then yield cameras.[ci] |]
                    yield ci, cam
            |] |> Map.ofArray

        let pointBlocks      = 
            guessedPoints   |> Array.map ( fun po -> [| po |] )
                            |> Array.map p.AddParameterBlock

        let (camIdx, camb)  = guessedCameras 
                                |> Map.filter (fun _ o -> let (_,f) = o.[0] in not f) 
                                |> Map.map (fun _ c -> c |> Array.map fst )
                                |> Map.toArray
                                |> Array.unzip

        let camBlocks        = camb |> Array.map p.AddParameterBlock<Camera3d,Camera3s>
        
        let (camFixedIdx, camFixed) = 
                              guessedCameras
                                |> Map.filter (fun _ o -> let (_,f) = o.[0] in f) 
                                |> Map.map (fun _ c -> (c |> Array.map fst) )
                                |> Map.toArray
                                |> Array.unzip
        
        let getCurrentPointsCams() =  
            
            let points =
                pointBlocks |> Array.map ( fun pb -> pb.Result ) |> Array.concat

            let cameras = 

                let cb = camBlocks 
                            |> Array.map (fun b -> b.Result.[0], false)

                let cf = camFixed
                            |> Array.map (fun b -> b.[0], true)
                
                [|
                    for i in 0 .. cf.Length-1 do
                        yield camFixedIdx.[i], cf.[i]
                    for i in 0 .. cb.Length-1 do
                        yield camIdx.[i], cb.[i]
                |] |> Map.ofArray
            
            points, cameras


        let ipointsCams() =  
            
            let points =
                pointBlocks |> Array.map ( fun pb -> pb.IntermediaryResult |> Array.map ( fun s -> s.Value ) ) |> Array.concat

            let cameras = 

                let cb = camBlocks 
                            |> Array.map (fun b -> b.IntermediaryResult.[0] |> Camera3s.Value, false)

                let cf = camFixed
                            |> Array.map (fun b -> b.[0], true)
                
                [|
                    for i in 0 .. cf.Length-1 do
                        yield camFixedIdx.[i], cf.[i]
                    for i in 0 .. cb.Length-1 do
                        yield camIdx.[i], cb.[i]
                |] |> Map.ofArray
            
            points, cameras




        let mutable iterationCounter = 0
        let mutable processedParams = 0
        let numParams = (pointBlocks |> Array.length) * ((camBlocks |> Array.length) + (camFixed |> Array.length))
        let iterationCallback() =
            processedParams <- processedParams + 1
            if processedParams > numParams then
                processedParams <- 0
                iterationCounter <- iterationCounter + 1
                let (points, cameras) = ipointsCams()
                iterationAdorner points cameras

        for pi in 0 .. pointBlocks.Length-1 do
            let pb = pointBlocks.[pi]
            let pointIndex = pi

            for ci in 0 .. camFixed.Length-1 do 
                let cf = camFixed.[ci]
                let fixedIndex = camFixedIdx.[ci]
                
                if measurements.[fixedIndex] |> Map.containsKey pointIndex then
                    let real = measurements.[fixedIndex].[pointIndex]

                    p.AddCostFunction(2, pb, fun point ->
                        
                            let obs = point.[0] |> V3s.getProjectedBy cf.[0]

                            let diff = real - obs
                        
                            iterationCallback()

                            [|
                                diff.X
                                diff.Y
                            |]

                        )
                    
            for ci in 0 .. camBlocks.Length-1 do
                let cb = camBlocks.[ci]
                let camIndex = camIdx.[ci]

                if measurements.[camIndex] |> Map.containsKey pointIndex then
                    let real = measurements.[camIndex].[pointIndex]

                    p.AddCostFunction(2, pb, cb, fun point cam ->
                    
                        let obs = cam.[0].Project point.[0]
                    
                        let diff = real - obs

                        iterationCallback()

                        [|
                            diff.X
                            diff.Y
                        |]
                    )

        let cost = p.Solve(options)

        let (points, cameras) = getCurrentPointsCams()

        cost, points, cameras

    let private improveSol (options : CeresOptions) (adorner : BundlerSolution -> unit) (sol : BundlerSolution) =
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
        
        let mkSolution cost points cameras =
            {
                cost = cost
                problem = parent
                points = points |> Seq.mapi (fun i p -> subPoints.[i], p) |> Map.ofSeq
                cameras = cameras
            }

        let iterationCallback points cams = 
            let sol = mkSolution 0.0 points cams
            adorner sol

        let cost, points, cameras =  
            let points = sol.points |> Map.toSeq |> Seq.map snd |> Seq.toArray
            let cameras = sol.cameras
            improveInternal options points cameras iterationCallback subMeasurements
        
        mkSolution cost points cameras

    let solve (adorner : BundlerSolution -> unit) (p : BundlerProblem)  =
        Log.startTimed "solve %d cameras" p.cameras.Count
        printfn " "
        let options = CeresOptions(2500, CeresSolverType.SparseSchur, true, 1.0E-32, 1.0E-32, 1.0E-32)
        let measurementCount = p.cameras |> Seq.sumBy (fun ci -> p.input.measurements.[ci].Count)

        let tinyCost = 1.0E-16 * float measurementCount

        let mutable bestCost = Double.PositiveInfinity
        let mutable best : Option<BundlerSolution> = None
        let mutable iter = 0
        while iter < 8 && bestCost > tinyCost do
            let n = improveSol options adorner (BundlerSolution.random p)
            if n.cost < bestCost then
                bestCost <- n.cost
                best <- Some n
                Log.line "%d: %.3f" iter n.cost
            iter <- iter + 1

        Log.stop()
        best
    
