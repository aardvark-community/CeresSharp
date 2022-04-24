namespace CeresSharp

open Aardvark.Base
open CeresSharp
open CeresSharp.Raw
open Microsoft.FSharp.NativeInterop

#nowarn "9"

module Ceres =

    let private gramSchmidt (m : M33d) =
        let x = m.C0 |> Vec.normalize
        let y = m.C1 |> Vec.normalize
        let z = m.C2 |> Vec.normalize

        let x = x
        let y = y - Vec.dot x y * x |> Vec.normalize
        let z = z - Vec.dot x z * x - Vec.dot y z * y |> Vec.normalize

        M33d.FromCols(x, y, z)
        

    let registerPointsSimilarity (outlierDistance : float) (correspondences : array<V3d * V3d>) =
        if correspondences.Length < 3 then
            None
        else
            let (a0, b0) = correspondences.[0]
            let (a1, b1) = correspondences.[1]
            let (a2, b2) = correspondences.[2]
            
            let ua = a1 - a0 |> Vec.normalize
            let va = a2 - a0 |> Vec.normalize
            let wa = Vec.cross ua va
            let ub = b1 - b0 |> Vec.normalize
            let vb = b2 - b0 |> Vec.normalize
            let wb = Vec.cross ub vb

            let ma = M33d.FromCols(ua, va, wa)
            let mb = M33d.FromCols(ub, vb, wb)
            let m = mb * ma.Inverse
            //let ma = gramSchmidt ua va wa
            //let mb = gramSchmidt ub vb wb
            
            let rot = Rot3d.FromM33d(gramSchmidt m, 1E-6)
            let scale = Vec.length (b1 - b0) / Vec.length (a1 - a0)
            
            let guess =
                Euclidean3d(rot, b0) *
                Similarity3d(scale, Euclidean3d.Identity) *
                Euclidean3d(Rot3d.Identity, -a0)

            use problem = new Problem()
            use trafo = problem.AddParameterBlock [| guess |]

            let loss =
                if outlierDistance <= 0.0 then TrivialLoss
                else HuberLoss outlierDistance

            problem.AddCostFunction(correspondences.Length, trafo, loss, fun trafo i ->
                let trafo = trafo.[0]
                let (a,b) = correspondences.[i]
                let res = trafo.TransformPos a - b
                res
            )
            
            let residual =
                problem.Solve {
                    maxIterations = 100
                    solverType = DenseQr
                    print = true
                    functionTolerance = 1.0E-7
                    gradientTolerance = 1.0E-7
                    parameterTolerance = 1.0E-7
                }

            if System.Double.IsInfinity residual then
                None
            else
                Some trafo.Result.[0]

    let registerPointsEuclidean (outlierDistance : float) (correspondences : array<V3d * V3d>) =
        if correspondences.Length < 3 then
            None
        else
            let (a0, b0) = correspondences.[0]
            let (a1, b1) = correspondences.[1]
            let (a2, b2) = correspondences.[2]
            
            let ua = a1 - a0 |> Vec.normalize
            let va = a2 - a0 |> Vec.normalize
            let wa = Vec.cross ua va
            let ub = b1 - b0 |> Vec.normalize
            let vb = b2 - b0 |> Vec.normalize
            let wb = Vec.cross ub vb

            let ma = M33d.FromCols(ua, va, wa)
            let mb = M33d.FromCols(ub, vb, wb)
            let m = mb * ma.Inverse
            //let ma = gramSchmidt ua va wa
            //let mb = gramSchmidt ub vb wb
            
            let rot = Rot3d.FromM33d(gramSchmidt m, 1E-6)
 
            let guess =
                Euclidean3d(rot, b0) *
                Euclidean3d(Rot3d.Identity, -a0)

            use problem = new Problem()
            use trafo = problem.AddParameterBlock [| guess |]

            let loss =
                if outlierDistance <= 0.0 then TrivialLoss
                else HuberLoss outlierDistance

            problem.AddCostFunction(correspondences.Length, trafo, loss, fun trafo i ->
                let trafo = trafo.[0]
                let (a,b) = correspondences.[i]
                let res = trafo.TransformPos a - b
                res
            )
            
            let residual =
                problem.Solve {
                    maxIterations = 100
                    solverType = DenseQr
                    print = true
                    functionTolerance = 1.0E-7
                    gradientTolerance = 1.0E-7
                    parameterTolerance = 1.0E-7
                }

            if System.Double.IsInfinity residual then
                None
            else
                Some trafo.Result.[0]

    
    
    let optimizePhotoNetwork (options : Config) (iterations : CeresBundleIteration[]) (projections : CeresProjection[]) (distortions : CeresDistortion[]) (cameras : Euclidean3d[]) (points : V3d[]) (fixedPoints : V3d[]) (residuals : CeresBundleResidual[]) =
        let localCameras = cameras |> Array.map CeresCamera3d.FromEuclidean3d
        
        use pProjections = fixed projections
        use pDistortions = fixed distortions
        use pCameras = fixed localCameras
        use pPoints = fixed points
        use pResiduals = fixed residuals
        use pIterations = fixed iterations
        use pOptions = fixed [| Config.toCeresOptions options |]
        use pFixed = fixed fixedPoints

        let final =
            CeresRaw.cOptimizePhotonetwork(
                pOptions,
                iterations.Length, pIterations,
                projections.Length, pProjections, pDistortions,
                cameras.Length, pCameras,
                points.Length, pPoints,
                fixedPoints.Length, pFixed,
                residuals.Length, pResiduals
            )
            
        for i in 0 .. cameras.Length - 1 do
            cameras.[i] <- localCameras.[i].ToEuclidean3d()
            
        final
        
        
    


