namespace Examples

open System
open Aardvark.Base
open Aardvark.Reconstruction

type RenderSponzaConfig = 
        {
            SponzaPath  : string
            NumCams     : int
            VertexCount : Option<int>
            PercentObservations : float
            JitterNDC : float

            //implement this: every track has chance to be false. 
            //if track is false, each observation in the track has 
            //a chance to have randomized v2d.
//            PercentFalse : float * float
        }

module RenderSponzaConfig =
    
    let subsampled path cams points =
        {
            SponzaPath = path
            NumCams    = cams
            VertexCount= points

            PercentObservations = 1.0
            JitterNDC           = 0.0
//            PercentFalse        = 0.0 
        }


module Sponza =
    
    open Aardvark.SceneGraph.IO
    open System.Collections.Generic
    
    let tracks (cfg : RenderSponzaConfig) =
        let cams = cfg.NumCams

        //euclid/inout/backend-paper/sponza_obj_copy
        //@"D:\file\sponza\sponza_NoMaterials_cm.obj" 
        Loader.Assimp.initialize()
        let scene = Loader.Assimp.loadFrom cfg.SponzaPath Assimp.PostProcessSteps.None
        
        let rand = RandomSystem()
        let (sponzaNormals, sponzaVertices) = 

            let center = 
                let middle = scene.bounds.Center
                Trafo3d.Translation -middle

            let pos = 
                [|
                    for a in scene.meshes do
                        yield a.geometry.IndexedAttributes.[DefaultSemantic.Positions] :?> V3f[]
                |]  |> Array.concat
                    |> Array.map V3d.op_Explicit
                    |> Array.map center.Forward.TransformPos

            let norm = 
                [|
                    for a in scene.meshes do
                        yield a.geometry.IndexedAttributes.[DefaultSemantic.Normals] :?> V3f[]
                |]  |> Array.concat
                    |> Array.map V3d.op_Explicit

            pos
                |> Array.zip norm
                |> Array.sortBy ( fun _ -> rand.UniformDouble())
                |> (match cfg.VertexCount with Some count -> Array.take count | None -> id)
                |> Array.unzip
           
        let Cids = [ for i in 0..cams-1 do yield CameraId(i) ]

        let c = V3d.OOO
        let r = 3000.0

        let mutable i = 0.0
        let step = (2.0 * Math.PI) / float cams
        
        let mutable fs = MapExt<CameraId, ref<MapExt<TrackId, V2d>>>.Empty
        

        let rand = RandomSystem()
        for cid in Cids do
            let x = r * sin i
            let y = r * cos i
            let cam = Camera3d.LookAt(c + V3d(x,y,0.0), c, 1.0, V3d.OOI)
            
            for pi in 0..sponzaVertices.Length-1 do
                let tid = TrackId(pi)

                if rand.UniformDouble() < cfg.PercentObservations then
                    let v = sponzaVertices.[pi]

                    let proj = cam.Project v
                    
                    let f = proj + ( rand.UniformV2dDirection() * cfg.JitterNDC )
                    
                    if f.X < 1.0 && f.X > -1.0 && f.Y < 1.0 && f.Y > -1.0 then
                        
                        fs <- fs |> MapExt.alter cid ( fun ms -> 
                                        match ms with
                                        | None -> 
                                            let m = ref MapExt.empty
                                            m := !m |> MapExt.add tid f
                                            Some m
                                        | Some m ->
                                            m := !m |> MapExt.add tid f
                                            Some m
                                    )
            i <- i + step
            
        fs  |> MapExt.map ( fun _ ms -> !ms )
        