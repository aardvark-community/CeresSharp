namespace Ceres

open CeresSharp
open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Aardvark.Base

#nowarn "9"

module Entry =

    //[<EntryPoint>]
    let main argv =

        use p = new Problem()

        let b = p.AddParameterBlock [| 1.0 |]
        let c = p.AddParameterBlock [| 1.0 |]

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
            print = true
            functionTolerance = 1.0E-16
            gradientTolerance = 1.0E-16
            parameterTolerance = 1.0E-16
        } |> ignore

        let b = b.Result 
        let c = c.Result 

        printfn "b = %A" b
        printfn "c = %A" c


        0 
