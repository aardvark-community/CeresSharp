#load @"paket-files/build/aardvark-platform/Aardvark.Fake/DefaultSetup.fsx"

open Fake
open System
open System.IO
open System.Diagnostics
open Aardvark.Fake
open Fake.Testing


do Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let sln = ["src/Ceres.sln"]
DefaultSetup.install sln

Target "UnpackCeresNative" (fun () ->
    // todo: repack on push?
    Console.WriteLine("Unzipping CERES static lib.")
    
    let unzipToIfNotExists zip target =
        if File.Exists target |> not then
            let dir = Path.GetDirectoryName target
            System.IO.Compression.ZipFile.ExtractToDirectory(zip,dir)
    
    unzipToIfNotExists @"lib\ceres\x64\Debug\ceres_static.zip" @"lib\ceres\x64\Debug\ceres_static.lib"
    unzipToIfNotExists @"lib\ceres\x64\Release\ceres_static.zip" @"lib\ceres\x64\Release\ceres_static.lib"
)

"UnpackCeresNative" ==> "Restore" |> ignore

#if DEBUG
do System.Diagnostics.Debugger.Launch() |> ignore
#endif


entry()