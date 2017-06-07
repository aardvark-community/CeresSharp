namespace Examples

open Aardvark.Base
open Aardvark.Reconstruction

open System.IO
open System.Text

module App =
    open Aardvark.Reconstruction.Feature

    [<EntryPoint>]
    let main argv = 
    
        //let config = 
        //    RenderSponzaConfig.subsampled 
        //        @"D:\file\sponza\sponza3.obj" 
        //        5
        //        (Some 500)

        //Sponza.fotos config

        //System.Environment.Exit 0 

        //let input = 
        //    let config = 
        //        RenderSponzaConfig.subsampled 
        //            @"D:\file\sponza\sponza_NoMaterials_cm.obj" 
        //            5
        //            (Some 500)

        //    Sponza.tracks
        //     { config with
        //         JitterNDC = 0.0
        //     }

        let input =
            
            let fotos = 
                @"D:\bla2\sponza2"
                |> Directory.GetFiles
                |> Array.toList 
                |> List.filter ( fun fn -> (Path.GetExtension fn).ToLower() = ".jpg" )
                |> List.map PixImage<byte>

            let featureType = FeatureExtract.Orb

            let matcher = Match.GetMatches.probabilityOnly 0.9 20.0 0.6 0.5

            let ((input,_),_) = BundlerInput.mk featureType matcher fotos

            input |> Tracks.toMeasurements

        let bundlation = 
            
            let p = BundlerProblem.ofMeasurements input
            CoolNameGoesHere.miniCV p

        Viewer.bundleViewer bundlation

        0 // return an integer exit code