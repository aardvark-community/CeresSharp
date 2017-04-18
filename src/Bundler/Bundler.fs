namespace Aardvark.Ceres

open System
open Aardvark.Base
open CeresSharp


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Bundler =
    open CeresSharp
    open OpenTK.Platform.Windows

    let private rand = RandomSystem()

    let private improveInternal (options : CeresOptions) (prob : BundlerProblem) (points : Map<int,SolverPoint>) (cameras : Map<int,SolverCamera>) 
                                ( iterationAdorner : Map<int,SolverPoint> -> Map<int,SolverCamera> -> unit )
                                (measurements : Map<int,Map<int, V2d>>) : float * Map<int,SolverPoint> * Map<int,SolverCamera> =
        use p = new Problem()
        let guessedPoints   : Map<int,SolverPoint> = points
        let guessedCameras  : Map<int,SolverCamera[]> =
            [|
                for kvp in measurements do
                    let ci = kvp.Key
                    let cam = [| cameras.[ci] |]
                    yield ci, cam
            |] |> Map.ofArray

        let (pointIdx, pointb) = 
            guessedPoints   |> Map.filter ( fun _ po -> not po.isFixed )
                            |> Map.map ( fun _ po -> [| po |] )
                            |> Map.toArray
                            |> Array.map ( fun (idx,po) -> idx,po |> Array.map ( fun p -> p.point ) )
                            |> Array.unzip
        
        let (fixedpointIdx, fixedPointb) = 
            guessedPoints   |> Map.filter ( fun _ po -> po.isFixed )
                            |> Map.map ( fun _ po -> [| po |] )
                            |> Map.toArray
                            |> Array.map ( fun (idx,po) -> idx,po |> Array.map ( fun p -> p.point ) )
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
                let pb = pointBlocks |> Array.map ( fun pb -> { point = pb.Result.[0]; isFixed = false } )

                let pf = fixedPointb |> Array.map ( fun pb -> { point = pb.[0]; isFixed = true } )

                [|
                    for i in 0 .. pb.Length-1 do
                        yield pointIdx.[i], pb.[i]
                    for i in 0 .. pf.Length-1 do
                        yield fixedpointIdx.[i], pf.[i]
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

        let indexOf x = Array.findIndex ( (=) x )

        let ipointsCams() =  
            
            let points =
                let pb = pointBlocks |> Array.map ( fun pb -> { point = pb.IntermediaryResult.[0]; isFixed = false } )

                let pf = fixedPointb |> Array.map ( fun pb -> { point = pb.[0]; isFixed = true } )

                [|
                    for i in 0 .. pb.Length-1 do
                        yield pointIdx.[i], pb.[i]
                    for i in 0 .. pf.Length-1 do
                        yield fixedpointIdx.[i], pf.[i]
                |] |> Map.ofArray

            let cameras = 

                let cb = camBlocks 
                            |> Array.map (fun b -> { cam = b.IntermediaryResult.[0]; isFixed = false } )

                let cf = camFixed
                            |> Array.map (fun b ->  { cam = b.[0]; isFixed = true })
                
                [|
                    for i in 0 .. cf.Length-1 do
                        yield camFixedIdx.[i], cf.[i]
                    for i in 0 .. cb.Length-1 do
                        yield camIdx.[i], cb.[i]
                |] |> Map.ofArray
            
            points, cameras


        let pointWeights = 
            prob.input.tracks |> Array.map (fun t -> float t.Length)


        let mutable iterationCounter = 0
        let numParams = ((pointBlocks |> Array.length) + (fixedPointb |> Array.length)) * ((camBlocks |> Array.length) + (camFixed |> Array.length))
        let mutable processedParams = numParams
        let iterationCallback() =
            processedParams <- processedParams + 1
            if processedParams > numParams then
                processedParams <- 0
                iterationCounter <- iterationCounter + 1
                let (points, cameras) = ipointsCams()
                iterationAdorner points cameras

        let clamp (v : scalar) min max =
            let err = 
                if v.Value < min then (scalar min - v)
                elif v.Value > max then (v - scalar max)
                else scalar 0.0

            (100.0 * err) ** 20.0
            
        iterationCallback()

        let costFunction (real : V2d) (obs : V2s, depth : scalar) =
            let diff = real - obs
                    
            let depth = depth * depth

            //let mega = 
            //    if depth.Value <= 1.0 then
            //        let d = (scalar 1.0 - depth) * scalar 1000.0
            //        d ** 100.0
            //    else
            //        scalar 0.0

            
                

            iterationCallback()
                    
            [|
                diff.X 
                diff.Y 
                //clamp obs.X -1.0 1.0
                //clamp obs.Y -1.0 1.0
                //clamp depth 2.0 Double.PositiveInfinity
//                mega 
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
        let options = CeresOptions(2500, CeresSolverType.SparseSchur, true, 1.0E-16, 1.0E-16, 1.0E-16)
        let measurementCount = p.cameras |> Seq.sumBy (fun ci -> p.input.measurements.[ci].Count)

        if p.cameras.Count < 2 then 
            Log.error "Less than 3 cams (only %A), no stable solution possible" p.cameras.Count
            None
        else
            let pc = p.input.measurements |> Seq.sumBy (fun m -> m.Value.Count)
            if pc < 8 then
                Log.error "Less than 8 total points (only %A), no solution possible." pc
                None
            else
                let tinyCost = 1.0E-16 * float measurementCount

                let mutable bestCost = Double.PositiveInfinity
                let mutable best : Option<BundlerSolution> = None
                let mutable iter = 0

                let adorn name v =
                    printf "%s" name
                    Console.ReadLine() |> ignore
                    v
                
                let rec redorn n f a =  
                    match n with
                    | 0 -> a
                    | _ ->
                        redorn (n-1) f (f a)

                let solution =
                    BundlerSolution.estimateStartingValues p
                        //|> redorn 10 (
                        //    BundlerSolution.withFixings true false >>
                        //    improveSol options adorner >>
                        //    adorn "pts adorned" >>
                        //    BundlerSolution.withFixings false true >>
                        //    improveSol options adorner >>
                        //    adorn "cams adorned" >>
                        //    unbox
                        //)
                        //|> BundlerSolution.withFixings true false
                        //|> improveSol (CeresOptions(2500, CeresSolverType.SparseSchur, true, 1.0E-16, 1.0E-16, 1.0E-16)) adorner
                        |> BundlerSolution.withFixings false false
                        |> improveSol (CeresOptions(2500, CeresSolverType.SparseSchur, true, 1.0E-16, 1.0E-16, 1.0E-16)) adorner
        
                
                best <- Some solution
                best |> Option.iter ( fun sol -> Log.warn "Final cost: %A" sol.cost; Log.warn "Final error metrics: \n%A" (BundlerSolution.errorMetrics sol) )

                Log.stop()
                best
    
