#load @"paket-files/build/vrvis/Aardvark.Fake/DefaultSetup.fsx"

open Fake
open System
open System.IO
open System.Diagnostics
open Aardvark.Fake
open Fake.Testing


do Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let sln = ["src/Ceres.sln"]
DefaultSetup.install sln
(*
Target "Deploy" (fun () ->
    Fake.MSBuildHelper.MSBuild "bin/Release" "build" [ "Configuration", "Deploy" ] sln |> printfn "%A"
)

"Deploy" ==> "AddNativeResources" ==> "CreatePackage"
*)
#if DEBUG
do System.Diagnostics.Debugger.Launch() |> ignore
#endif


entry()