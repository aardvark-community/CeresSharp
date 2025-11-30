# CeresSharp

F# wrappers for [Ceres Solver](http://ceres-solver.org/) and [IpOpt](https://coin-or.github.io/Ipopt/) with aardvark-style native dependencies.

[![Ceres NuGet](https://badgen.net/nuget/v/Ceres)](https://www.nuget.org/packages/Ceres/)
[![IpOptSharp NuGet](https://badgen.net/nuget/v/IpOptSharp)](https://www.nuget.org/packages/IpOptSharp/)

## Examples

### Ceres

```fsharp
open CeresSharp
open Aardvark.Base

// Solve a simple system: { x = y, x² + y² = 1 }
use p = new Problem()
use x = p.AddParameterBlock [| 1.0 |]
use y = p.AddParameterBlock [| -1.0 |]

p.AddCostFunction(2, x, y, TrivialLoss, fun x y ->
    [| x.[0] - y.[0]; x.[0] * x.[0] + y.[0] * y.[0] - 1.0 |]
)

let residual = p.Solve {
    maxIterations = 50
    solverType = DenseQr
    functionTolerance = 1E-16
    gradientTolerance = 1E-16
    parameterTolerance = 1E-16
    print = false
}

printfn "x = %.4f, y = %.4f" x.Result.[0] y.Result.[0]
```

### IpOpt

```fsharp
open IpOptSharp
open Aardvark.Base

// Find the largest equilateral triangle inscribed in a unit circle
let a = ref (V2d(-1.0, -1.0))
let b = ref (V2d(1.0, -1.0))
let c = ref (V2d(0.0, 1.0))

let (status, objective) =
    ipopt {
        let! a = a
        let! b = b
        let! c = c

        // Maximize the circumference
        let circumference = Vec.length (b - a) + Vec.length (c - b) + Vec.length (a - c)
        IpOpt.Maximize circumference

        // Constrain vertices to the unit circle
        IpOpt.Equal(a.Length, 1.0)
        IpOpt.Equal(b.Length, 1.0)
        IpOpt.Equal(c.Length, 1.0)
    }

printfn "status: %A, objective: %.4f" status objective
```

## Building Locally

### Prerequisites

* `cmake`
* `dotnet >= 6.0`
* C/C++ compiler

### Steps
1. setup environment variable `GITHUB_TOKEN` with a token having at least read-access to packages on github (see [here](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token) how to create a token)
2. run `buildnative`
3. `dotnet tool restore`
4. `dotnet paket restore`
5. `dotnet build src/Aardvark.Optimization.sln`

## Pushing Packages

New Packages can be created by simply adding a new version in `RELEASE_NOTES.md` and pushing the file to the `master` branch.
The GitHub [CI Action](https://github.com/aardvark-community/CeresSharp/actions/workflows/publish.yml) will take care of building native code and publishing packages to NuGet/GitHub Packages

