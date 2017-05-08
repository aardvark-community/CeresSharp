namespace Examples

open Aardvark.Base
open Aardvark.Reconstruction

module App =
    [<EntryPoint>]
    let main argv = 
    
        let sponza = 
            let config = 
                RenderSponzaConfig.subsampled 
                    @"D:\file\sponza\sponza_NoMaterials_cm.obj" 
                    5
                    (Some 500)

            Sponza.tracks
             { config with
                 JitterNDC = 0.25
             }

        let bundlation = 
            
            let p = BundlerProblem.ofMeasurements sponza
            CoolNameGoesHere.miniCV p

        Viewer.bundleViewer bundlation

        0 // return an integer exit code