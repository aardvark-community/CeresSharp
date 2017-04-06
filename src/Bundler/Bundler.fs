namespace Aardvark.Ceres

open System
open Aardvark.Base
open CeresSharp


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Bundler =
    open CeresSharp
    open OpenTK.Platform.Windows

    let private rand = RandomSystem()

    let private improveInternal (options : CeresOptions) (prob : BundlerProblem) (points : Map<int,V3d>) (cameras : Map<int,SolverCamera>) 
                                ( iterationAdorner : Map<int,V3d> -> Map<int,SolverCamera> -> unit )
                                (measurements : Map<int,Map<int, V2d>>) : float * Map<int,V3d> * Map<int,SolverCamera> =
        use p = new Problem()
        let guessedPoints   : Map<int,V3d> = points
        let guessedCameras  : Map<int,SolverCamera[]> =
            [|
                for kvp in measurements do
                    let ci = kvp.Key
                    let cam = [| cameras.[ci] |]
                    yield ci, cam
            |] |> Map.ofArray

        let (pointIdx, pointb) = 
            guessedPoints   |> Map.map ( fun _ po -> [| po |] )
                            |> Map.toArray
                            |> Array.unzip
        
        let pointBlocks = pointb
                            |> Array.map p.AddParameterBlock

        let (camIdx, camb)  = guessedCameras 
                                |> Map.filter (fun _ o -> not o.[0].isFixed) 
                                |> Map.map (fun _ o -> o |> Array.map ( fun c -> c.cam ))
                                |> Map.toArray
                                |> Array.unzip

        let camBlocks        = camb |> Array.map p.AddParameterBlock<Camera3d,Camera3s>
        
        let (camFixedIdx, camFixed) = 
                              guessedCameras
                                |> Map.filter (fun _ o -> o.[0].isFixed) 
                                |> Map.map (fun _ o -> o |> Array.map ( fun c -> c.cam ))
                                |> Map.toArray
                                |> Array.unzip
        
        let getCurrentPointsCams() =  
            
            let points =
                let pb = pointBlocks |> Array.map ( fun pb -> pb.Result ) |> Array.concat

                [|
                    for i in 0 .. pb.Length-1 do
                        yield pointIdx.[i], pb.[i]
                |] |> Map.ofArray

            let cameras = 

                let cb = camBlocks 
                            |> Array.map (fun b -> { cam = b.Result.[0]; isFixed = false })

                let cf = camFixed
                            |> Array.map (fun b -> { cam = b.[0]; isFixed = true })
                
                [|
                    for i in 0 .. cf.Length-1 do
                        yield camFixedIdx.[i], cf.[i]
                    for i in 0 .. cb.Length-1 do
                        yield camIdx.[i], cb.[i]
                |] |> Map.ofArray
            
            points, cameras

        let indexOf x xs =
            xs |> Array.findIndex ( fun y -> y = x )

        let ipointsCams() =  
            
            let points =
                let pb = pointBlocks |> Array.map ( fun pb -> pb.IntermediaryResult |> Array.map ( fun s -> s.Value ) ) |> Array.concat

                [|
                    for i in 0 .. pb.Length-1 do
                        yield pointIdx.[i], pb.[i]
                |] |> Map.ofArray

            let cameras = 

                let cb = camBlocks 
                            |> Array.map (fun b -> { cam = b.IntermediaryResult.[0] |> Camera3s.Value; isFixed = false } )

                let cf = camFixed
                            |> Array.map (fun b ->  { cam = b.[0]; isFixed = true })
                
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

        for i in 0 .. prob.input.tracks.Length-1 do
            let pb = pointBlocks.[pointIdx.[i]]

            let track = prob.input.tracks.[i]

            for (ci,lpi) in track do
                if camIdx |> Array.contains ci then
                    let cb = camBlocks.[camIdx |> indexOf ci]

                    let real = measurements.[ci].[lpi]

                    p.AddCostFunction(2, pb, cb, fun point cam ->
                      
                        let obs = cam.[0].Project point.[0]
                      
                        let diff = real - obs
                    
                        iterationCallback()
                    
                        [|
                            diff.X
                            diff.Y
                        |]
                    )
                
                elif camFixedIdx |> Array.contains ci then
                    let cf = camFixed.[camFixedIdx |> indexOf ci]

                    let real = measurements.[ci].[lpi] 

                    p.AddCostFunction(2, pb, fun point ->
                          
                            let obs = point.[0] |> V3s.getProjectedBy cf.[0]
                    
                            let diff = real - obs
                          
                            iterationCallback()
                    
                            [|
                                diff.X
                                diff.Y
                            |]
                    
                        )
                else    
                    Log.error "CAN NOT HAPPEN!!!!!!!!!"

        let cost = p.Solve(options)

        let (points, cameras) = getCurrentPointsCams()

        cost, points, cameras

    let private improveSol (options : CeresOptions) (adorner : BundlerSolution -> unit) (sol : BundlerSolution) =
        
        let mkSolution cost points cameras =
            {
                cost = cost
                problem = sol.problem
                points = points
                cameras = cameras
            }

        let iterationCallback points cams = 
            let sol = mkSolution 0.0 points cams
            adorner sol

        let cost, points, cameras =  
            improveInternal options sol.problem sol.points sol.cameras iterationCallback sol.problem.input.measurements
        
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
            let initial = BundlerSolution.random p
            let n = improveSol options adorner initial
            if n.cost < bestCost then
                bestCost <- n.cost
                best <- Some n
                Log.line "%d: %.3f" iter n.cost
            iter <- iter + 1

        Log.stop()
        best
    
