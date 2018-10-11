open CeresSharp
open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Aardvark.Base

let rand = RandomSystem()

let randomRot() =
    let axis  = rand.UniformV3dDirection()
    let angle = rand.UniformDouble() * Constant.Pi
    Rot3d(axis, angle)

let randomEuclidean() =
    let trans = rand.UniformV3dDirection() * rand.UniformDouble() * 10.0
    Euclidean3d(randomRot(), trans)

let randomSimilarity() =
    let scale = rand.UniformDouble() * 10.0 + 0.01
    Similarity3d(scale, randomEuclidean())


let findRot () =
    
    let trafo = randomRot()

    let samples =
        Array.init 100 (fun _ ->
            let pt = rand.UniformV3dDirection() * rand.UniformDouble() * 10.0
            pt, trafo.TransformPos pt
        )

    let guess = randomRot()

    use problem = new Problem()
    use pTrafo = problem.AddParameterBlock [| guess |]
    
    problem.AddCostFunctionScalar(samples.Length * 3, pTrafo, TrivialLoss, fun trafo res  ->
        let trafo = trafo.[0]

        let mutable ri = 0
        for i in 0 .. samples.Length - 1 do
            let (l,r) = samples.[i]

            let r = trafo.TransformPos l - r

            res.[ri + 0] <- r.X
            res.[ri + 1] <- r.Y
            res.[ri + 2] <- r.Z
            ri <- ri + 3
    )


    let residual =
        problem.Solve {
            maxIterations = 50
            solverType = DenseSchur
            print = true
            functionTolerance = 1.0E-16
            gradientTolerance = 1.0E-16
            parameterTolerance = 1.0E-16
        }
    
    let recovered = pTrafo.Result.[0]

    printfn "residual %.4f" residual
    printfn "org: %A" trafo
    printfn "rec: %A" recovered

let findEuclidean () =
    
    let trafo = randomEuclidean()

    let samples =
        Array.init 100 (fun _ ->
            let pt = rand.UniformV3dDirection() * rand.UniformDouble() * 10.0
            pt, trafo.TransformPos pt
        )

    let guess = randomEuclidean()

    use problem = new Problem()
    use pTrafo = problem.AddParameterBlock [| guess |]
    
    
    problem.AddCostFunctionScalar(samples.Length * 3, pTrafo, TrivialLoss, fun trafo res ->
        let trafo = trafo.[0]

        let mutable ri = 0
        for i in 0 .. samples.Length - 1 do
            let (l,r) = samples.[i]

            let r = trafo.TransformPos l - r

            res.[ri + 0] <- r.X
            res.[ri + 1] <- r.Y
            res.[ri + 2] <- r.Z
            ri <- ri + 3
    )


    let residual =
        problem.Solve {
            maxIterations = 50
            solverType = DenseSchur
            print = true
            functionTolerance = 1.0E-16
            gradientTolerance = 1.0E-16
            parameterTolerance = 1.0E-16
        }
    
    let recovered = pTrafo.Result.[0]

    printfn "residual %.4f" residual
    printfn "org: %A" trafo
    printfn "rec: %A" recovered

let findSimilarity () =
    let trafo = randomSimilarity()

    let samples =
        Array.init 100 (fun _ ->
            let pt = rand.UniformV3dDirection() * rand.UniformDouble() * 10.0
            pt, trafo.TransformPos pt
        )

    let guess = randomSimilarity()

    use problem = new Problem()
    use pTrafo = problem.AddParameterBlock [| guess |]
    
    //problem.AddCostFunction(samples.Length, pTrafo, fun trafo i ->
    //    let trafo = trafo.[0]
    //    let (l,r) = samples.[i]
    //    trafo.TransformPos l - r
    //)
    
    problem.AddCostFunction(samples.Length, pTrafo, fun trafo ->
        let trafo = trafo.[0]

        samples |> Array.map (fun (l,r) ->
            trafo.TransformPos l - r
        )
    )

    let residual =
        problem.Solve {
            maxIterations = 50
            solverType = DenseSchur
            print = true
            functionTolerance = 1.0E-16
            gradientTolerance = 1.0E-16
            parameterTolerance = 1.0E-16
        }
    
    let recovered = pTrafo.Result.[0]

    printfn "residual %.4f" residual
    printfn "org: %A" trafo
    printfn "rec: %A" recovered


let cosSin () =
    use p = new Problem()
    use b = p.AddParameterBlock [| 1.0 |]
    use c = p.AddParameterBlock [| 1.0 |]

    p.AddCostFunction(2, b, c, TrivialLoss, fun b c ->
        let x = b.[0]
        let y = c.[0]

        [|
            x - y
            x * x + y * y - 1.0
        |]
    )
    p.Solve {
        maxIterations = 50
        solverType = DenseSchur
        print = false
        functionTolerance = 1.0E-16
        gradientTolerance = 1.0E-16
        parameterTolerance = 1.0E-16
    } |> ignore

    let b = b.Result 
    let c = c.Result 

    printfn "b = %A" b
    printfn "c = %A" c




[<EntryPoint>]
let main argv =
    //cosSin()
    //findSimilarity()
    //findEuclidean()
    findRot()
    

    0 
