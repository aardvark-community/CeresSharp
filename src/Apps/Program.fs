namespace Examples

open Aardvark.Base
open Aardvark.Reconstruction

module App =
    [<EntryPoint>]
    let main argv = 

        let sponza = 
            let config = RenderSponzaConfig.subsampled @"D:\file\sponza\sponza_NoMaterials_cm.obj" 5 (Some 5000)
            Sponza.tracks config

        Viewer.ofObservations sponza

        0 // return an integer exit code
