open CeresSharp
open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Aardvark.Base

let rand = RandomSystem()

let randomDistortion() =
    RadialDistortion2d(
        (rand.UniformDouble() - 0.5),
        (rand.UniformDouble() - 0.5),
        (rand.UniformDouble() - 0.5),
        (rand.UniformDouble() - 0.5)
    )

    
let randomProjection() =
    {
        focalLength = rand.UniformDouble() * 2.0 + 0.1
        aspect = rand.UniformDouble() * 4.0 + 0.5
        principalPoint = rand.UniformV2dDirection() * rand.UniformDouble() * 0.1
        distortion = 
            RadialDistortion2d(
                (rand.UniformDouble() - 0.5) * 0.01,
                (rand.UniformDouble() - 0.5) * 0.01,
                (rand.UniformDouble() - 0.5) * 0.01,
                (rand.UniformDouble() - 0.5) * 0.01
            )

    }

let randomRot2d() =
    let angle = rand.UniformDouble() * Constant.PiTimesTwo
    Rot2d(angle)

let randomRot3d() =
    let axis  = rand.UniformV3dDirection()
    let angle = rand.UniformDouble() * Constant.Pi
    Rot3d(axis, angle)

let randomEuclidean2d() =
    let trans = rand.UniformV2dDirection() * rand.UniformDouble() * 10.0
    Euclidean2d(randomRot2d(), trans)

let randomEuclidean3d() =
    let trans = rand.UniformV3dDirection() * rand.UniformDouble() * 10.0
    Euclidean3d(randomRot3d(), trans)

let randomSimilarity2d() =
    let scale = rand.UniformDouble() * 10.0 + 0.01
    Similarity2d(scale, randomEuclidean2d())

let randomSimilarity3d() =
    let scale = rand.UniformDouble() * 10.0 + 0.01
    Similarity3d(scale, randomEuclidean3d())

let randomCircle() =
    let r = rand.UniformDouble() * 10.0 + 0.01
    let c = rand.UniformV2dDirection() * rand.UniformDouble() * 5.0
    Circle2d(c, r)

let randomSphere() =
    let r = rand.UniformDouble() * 10.0 + 0.01
    let c = rand.UniformV3dDirection() * rand.UniformDouble() * 5.0
    Sphere3d(c, r)


let findRot2d () =
    Log.start "Rot2d"
    let trafo = randomRot2d()

    let samples =
        Array.init 5 (fun _ ->
            let pt = rand.UniformV2dDirection() * rand.UniformDouble() * 10.0
            pt, trafo.TransformPos pt
        )

    let guess = randomRot2d()

    use problem = new Problem()
    use pTrafo = problem.AddParameterBlock [| guess |]
    
    problem.AddCostFunctionScalar(samples.Length * 2, pTrafo, TrivialLoss, fun trafo res  ->
        let trafo = trafo.[0]

        let mutable ri = 0
        for i in 0 .. samples.Length - 1 do
            let (l,r) = samples.[i]

            let r = trafo.TransformPos l - r

            res.[ri + 0] <- r.X
            res.[ri + 1] <- r.Y
            ri <- ri + 2
    )


    let residual =
        problem.Solve {
            maxIterations = 50
            solverType = DenseQr
            print = false
            functionTolerance = 1.0E-16
            gradientTolerance = 1.0E-16
            parameterTolerance = 1.0E-16
        }
    
    let recovered = pTrafo.Result.[0]

    Log.line "residual %.4f" residual
    Log.line "org: %A" trafo
    Log.line "rec: %A" recovered
    Log.stop()

let findRot3d () =
    Log.start "Rot3d"
    let trafo = randomRot3d()

    let samples =
        Array.init 5 (fun _ ->
            let pt = rand.UniformV3dDirection() * rand.UniformDouble() * 10.0
            pt, trafo.TransformPos pt
        )

    let guess = randomRot3d()

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
            solverType = DenseQr
            print = false
            functionTolerance = 1.0E-16
            gradientTolerance = 1.0E-16
            parameterTolerance = 1.0E-16
        }
    
    let recovered = pTrafo.Result.[0]

    Log.line "residual %.4f" residual
    Log.line "org: %A" trafo
    Log.line "rec: %A" recovered
    Log.stop()

let findEuclidean2d () =
    Log.start "Euclidean2d"
    let trafo = randomEuclidean2d()

    let samples =
        Array.init 100 (fun _ ->
            let pt = rand.UniformV2dDirection() * rand.UniformDouble() * 10.0
            pt, trafo.TransformPos pt
        )

    let guess = randomEuclidean2d()

    use problem = new Problem()
    use pTrafo = problem.AddParameterBlock [| guess |]
    
    
    problem.AddCostFunctionScalar(samples.Length * 2, pTrafo, TrivialLoss, fun trafo res ->
        let trafo = trafo.[0]

        let mutable ri = 0
        for i in 0 .. samples.Length - 1 do
            let (l,r) = samples.[i]

            let r = trafo.TransformPos l - r

            res.[ri + 0] <- r.X
            res.[ri + 1] <- r.Y
            ri <- ri + 2
    )


    let residual =
        problem.Solve {
            maxIterations = 50
            solverType = DenseQr
            print = false
            functionTolerance = 1.0E-16
            gradientTolerance = 1.0E-16
            parameterTolerance = 1.0E-16
        }
    
    let recovered = pTrafo.Result.[0]

    Log.line "residual %.4f" residual
    Log.line "org: %A" trafo
    Log.line "rec: %A" recovered
    Log.stop()

let findEuclidean3d () =
    Log.start "Euclidean3d"
    let trafo = randomEuclidean3d()

    let samples =
        Array.init 100 (fun _ ->
            let pt = rand.UniformV3dDirection() * rand.UniformDouble() * 10.0
            pt, trafo.TransformPos pt
        )

    let guess = randomEuclidean3d()

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
            solverType = DenseQr
            print = false
            functionTolerance = 1.0E-16
            gradientTolerance = 1.0E-16
            parameterTolerance = 1.0E-16
        }
    
    let recovered = pTrafo.Result.[0]

    Log.line "residual %.4f" residual
    Log.line "org: %A" trafo
    Log.line "rec: %A" recovered
    Log.stop()

let findSimilarity2d () =
    Log.start "Similarity2d"
    let trafo = randomSimilarity2d()

    let samples =
        Array.init 100 (fun _ ->
            let pt = rand.UniformV2dDirection() * rand.UniformDouble() * 10.0
            pt, trafo.TransformPos pt
        )

    let guess = randomSimilarity2d()

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
            solverType = DenseQr
            print = false
            functionTolerance = 1.0E-16
            gradientTolerance = 1.0E-16
            parameterTolerance = 1.0E-16
        }
    
    let recovered = pTrafo.Result.[0]

    Log.line "residual %.4f" residual
    Log.line "org: %A" trafo
    Log.line "rec: %A" recovered
    Log.stop()

let findSimilarity3d () =
    Log.start "Similarity3d"
    let trafo = randomSimilarity3d()

    let samples =
        Array.init 100 (fun _ ->
            let pt = rand.UniformV3dDirection() * rand.UniformDouble() * 10.0
            pt, trafo.TransformPos pt
        )

    let guess = randomSimilarity3d()

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
            solverType = DenseQr
            print = false
            functionTolerance = 1.0E-16
            gradientTolerance = 1.0E-16
            parameterTolerance = 1.0E-16
        }
    
    let recovered = pTrafo.Result.[0]

    Log.line "residual %.4f" residual
    Log.line "org: %A" trafo
    Log.line "rec: %A" recovered
    Log.stop()

let findCircle () =
    Log.start "Circle2d"
    let circle = randomCircle()

    let samples =
        Array.init 15 (fun _ ->
            let pt = rand.UniformV2dDirection() * circle.Radius + circle.Center
            let noise = rand.UniformV2dDirection() * rand.UniformDouble() * 0.1
            pt 

        )

    let guess = randomCircle()
    use problem = new Problem()
    use r = problem.AddParameterBlock [| guess |]

        
    problem.AddCostFunction(samples.Length, r, fun r i ->
        r.[0].DistanceSquared samples.[i]
    )
    
    let residual =
        problem.Solve {
            maxIterations = 50
            solverType = DenseQr
            print = false
            functionTolerance = 1.0E-16
            gradientTolerance = 1.0E-16
            parameterTolerance = 1.0E-16
        }
    
    let recovered = r.Result.[0]

    Log.line "residual %.4f" residual
    Log.line "org: %A" circle
    Log.line "rec: %A" recovered
    
    Log.stop()

let findSphere () =
    Log.start "Sphere3d"
    let sphere = randomSphere()

    let samples =
        Array.init 15 (fun _ ->
            let pt = rand.UniformV3dDirection() * sphere.Radius + sphere.Center
            let noise = rand.UniformV3dDirection() * rand.UniformDouble() * 0.1
            pt 

        )

    let guess = randomSphere()
    use problem = new Problem()
    use r = problem.AddParameterBlock [| guess |]

        
    problem.AddCostFunction(samples.Length, r, fun r i ->
        r.[0].DistanceSquared samples.[i]
    )
    
    let residual =
        problem.Solve {
            maxIterations = 50
            solverType = DenseQr
            print = false
            functionTolerance = 1.0E-16
            gradientTolerance = 1.0E-16
            parameterTolerance = 1.0E-16
        }
    
    let recovered = r.Result.[0]

    Log.line "residual %.4f" residual
    Log.line "org: %A" sphere
    Log.line "rec: %A" recovered
    
    Log.stop()


let cosSin () =
    Log.start "{ x = y, x² + y² = 1 }"
    use p = new Problem()
    use x = p.AddParameterBlock [| 1.0 |]
    use y = p.AddParameterBlock [| -1.0 |]

    p.AddCostFunction(2, x, y, TrivialLoss, fun x y ->
        let x = x.[0]
        let y = y.[0]

        [|
            x - y
            x * x + y * y - 1.0
        |]
    )
    let res = 
        p.Solve {
            maxIterations = 50
            solverType = DenseQr
            print = false
            functionTolerance = 1.0E-16
            gradientTolerance = 1.0E-16
            parameterTolerance = 1.0E-16
        } 

    let x = x.Result 
    let y = y.Result 

    Log.line "residual: %.4f" res
    Log.line "x = %A" x.[0]
    Log.line "y = %A" y.[0]
    Log.stop()

let powell() =
    Log.start "powell"
    use problem = new Problem()
    
    use x = problem.AddParameterBlock [| 3.0; -1.0; 0.; 1.0 |]

    problem.AddCostFunctionScalar(4, x, fun x r ->
        r.[0] <- x.[0] + 10.0 * x.[1]
        r.[1] <- sqrt (5.0) * (x.[2] - x.[3])
        r.[2] <- (x.[1] - 2.0 * x.[2]) ** 2.0
        r.[3] <- sqrt 10.0 * (x.[0] - x.[3]) ** 2.0
    )

    let res = 
        problem.Solve {
            maxIterations = 100
            solverType = DenseQr
            print = false
            functionTolerance = 1.0E-16
            gradientTolerance = 1.0E-16
            parameterTolerance = 1.0E-16
        } 

    let x = x.Result 
    Log.line "residual: %.4f" res
    Log.line "x = %A" x
    Log.stop()

let tenthOrder() =
    Log.start "(x-10)^10"
    use problem = new Problem()
    
    use x = problem.AddParameterBlock [| 3.0 |]

    problem.AddCostFunction(1, x, fun x _ ->
        let x = x.[0]
        (x - 10.0) ** 10.0
    )

    let res = 
        problem.Solve {
            maxIterations = 100
            solverType = DenseQr
            print = true
            functionTolerance = 1.0E-16
            gradientTolerance = 1.0E-16
            parameterTolerance = 1.0E-16
        } 

    let x = x.Result 
    Log.line "residual: %.4f" res
    Log.line "x = %A" x
    Log.stop()
    

let findDistortion() =
    Log.start "RadialDistortion2d"

    let distortion = randomDistortion() 

    let samples = 
        let ndc = Box2d(-V2d.II, V2d.II)
        Array.init 100 (fun _ ->
            let pt = rand.UniformV2d(ndc)
            let d = distortion.TransformPos pt
            if ndc.Contains d then
                Some (pt, d)
            else
                None
        )
        |> Array.choose id

    use problem = new Problem()
    use dist = problem.AddParameterBlock [| RadialDistortion2d.Identitiy |]
    problem.AddCostFunction(samples.Length, dist, fun dist i ->
        let dist = dist.[0]
        let (l,r) = samples.[i]
        dist.TransformPos l - r
    )

    let residual = 
        problem.Solve {
            maxIterations = 50
            solverType = DenseQr
            print = false
            functionTolerance = 1.0E-16
            gradientTolerance = 1.0E-16
            parameterTolerance = 1.0E-16
        } 

    let recovered = dist.Result.[0]
    Log.line "residual %.4f" residual
    Log.line "org: %A" distortion
    Log.line "rec: %A" recovered
    
    Log.stop()
    

let findProjection() =
    Log.start "Projection3d"

    let original = randomProjection() 

    let samples = 
        let ndc = Box2d(-V2d.II, V2d.II)
        Array.init 100 (fun _ ->
            let pt = rand.UniformV2d(ndc)
            let d = original.Unproject pt
            

            d, pt
        )

    use problem = new Problem()
    use proj = problem.AddParameterBlock [| randomProjection() |]
    problem.AddCostFunction(samples.Length, proj, fun proj i ->
        let proj = proj.[0]
        let (l,r) = samples.[i]
        proj.ProjectUnsafe(V3s l) - r
    )

    let residual = 
        problem.Solve {
            maxIterations = 100
            solverType = DenseQr
            print = false
            functionTolerance = 1.0E-16
            gradientTolerance = 1.0E-16
            parameterTolerance = 1.0E-16
        } 

    let recovered = proj.Result.[0]
    Log.line "residual %.4f" residual
    Log.line "org: %A" original
    Log.line "rec: %A" recovered
    
    Log.stop()
    
let findPointTrafo() =
    


    let sim = randomSimilarity3d()
    let box = Box3d(-10.0 * V3d.III, 10.0 * V3d.III)
    let corresponding =
        Array.init 200 (fun i ->
            if i > 3 && rand.UniformDouble() < 0.0 then
                rand.UniformV3d(box), rand.UniformV3d(box)
            else
                let a = rand.UniformV3d(box)
                let b = sim.TransformPos a

                let noise = rand.UniformV3dDirection() * rand.UniformDouble() * 2.0

                a, b + noise
        )

    match Ceres.registerPointsSimilarity 0.05 corresponding with
        | Some trafo ->
            let test = sim * trafo.Inverse
            
            

            Log.line "org:  %A" sim
            Log.line "rec:  %A" trafo
            Log.line "scale: %A" test.Scale
            Log.line "angle: %.4f°" (Constant.DegreesPerRadian * test.Rot.ToAngleAxis().Length)
            Log.line "trans: %.4f" (sim.Trans.Length / trafo.Trans.Length)

        | None ->
            Log.error "no work"




[<EntryPoint>]
let main argv =
    findPointTrafo()

    //cosSin()
    //powell()

    //findRot2d()
    //findEuclidean2d()
    //findSimilarity2d()
    //findCircle()

    //findRot3d()
    //findEuclidean3d()
    //findSimilarity3d()
    //findSphere()
    
    //findDistortion()
    //tenthOrder()
    //findProjection()

    0 
