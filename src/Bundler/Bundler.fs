namespace Aardvark.Ceres

open System
open Aardvark.Base
open CeresSharp

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Bundler =
    let private rand = RandomSystem()

    let solve (input : BundlerProblem) =
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
        
        let mutable success = false
        while not success do
            guessedPoints.SetByIndex(fun i -> rand.UniformV3dDirection()) |> ignore
            for i in 0 .. guessedCameras.Length - 1 do
                let v = Camera3d.LookAt(rand.UniformV3dDirection() * 10.0, V3d.Zero, 1.0, V3d.OOI)
                guessedCameras.[i].[0] <- v

            success <- p.Solve(CeresOptions(150, CeresSolverType.SparseSchur, true, 1.0E-10, 1.0E-10, 1.0E-10))
            if not success then Log.warn "retry"

        let points = worldPoints.Result
        let cameras = camBlocks |> Array.map (fun b -> b.Result.[0])


        { problem = input; cameras = cameras; points = points }
