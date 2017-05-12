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
    
    open Aardvark.SceneGraph
    open Aardvark.Rendering
    open Aardvark.Base
    open Aardvark.Base.Rendering
    open Aardvark.Base.Incremental
    open Aardvark.Application.WinForms
    open Aardvark.Application

    let fotos (cfg : RenderSponzaConfig) =
        let cams = cfg.NumCams

        //euclid/inout/backend-paper/sponza_obj_copy
        //@"D:\file\sponza\sponza_NoMaterials_cm.obj" 
        Loader.Assimp.initialize()
        let scene = Loader.Assimp.loadFrom cfg.SponzaPath (Assimp.PostProcessSteps.Triangulate ||| Assimp.PostProcessSteps.GenerateSmoothNormals)
        
        Ag.initialize()
        Aardvark.Init()

        use app = new OpenGlApplication()
        use win = app.CreateSimpleRenderWindow(1)
        
        win.Size <- V2i(2048,2048)

        let proj = win.Sizes    |> Mod.map (fun s -> Frustum.perspective 60.0 0.001 100.0 (float s.X / float s.Y))
                                |> Mod.map Frustum.projTrafo

        let view = CameraView.lookAt (V3d(0.0, 2.0, 0.0)) V3d.Zero -V3d.OOI
                            |> DefaultCameraController.controlWithSpeed (Mod.init 0.05) win.Mouse win.Keyboard win.Time
                            |> Mod.map CameraView.viewTrafo
                            
        let sg =   Sg.adapter scene |> Sg.normalizeTo (Box3d(-V3d.III, V3d.III))
                    
                    |> Sg.transform (Trafo3d.FromOrthoNormalBasis(V3d.IOO,(-V3d.OOI),V3d.OIO))
                    |> Sg.viewTrafo view
                    |> Sg.projTrafo proj
                    |> Sg.shader {
                        do! DefaultSurfaces.trafo
                        //do! DefaultSurfaces.constantColor C4f.White
                        do! DefaultSurfaces.diffuseTexture
                        do! DefaultSurfaces.simpleLighting
                       }
                    |> Sg.uniform "LightLocation" (Mod.constant (V3d.Zero))
                    
            
        let render = app.Runtime.CompileRender(win.FramebufferSignature, sg)
        let clear = app.Runtime.CompileClear(win.FramebufferSignature, Mod.constant (C4f(0.6,0.6,0.3)), Mod.constant 1.0)
        
        let task = RenderTask.ofList [clear; render]

        let s = task |> RenderTask.renderToColor (win.Sizes) 
        let r = s |> Mod.map (fun r -> app.Runtime.Download (r |> unbox<IBackendTexture>))
        s.Acquire()

        let mutable i = 0
        let sc () =
            
            printfn "screen %A" i
            (r |> Mod.force).SaveAsImage (sprintf @"D:\bla\sponza\sponza_%A.jpg" i)
            i <- i+1  

        win.Keyboard.KeyDown(Keys.P).Values.Add sc
        win.RenderTask <- task

        win.Run()
        
        //let rand = RandomSystem()
        //let (sponzaNormals, sponzaVertices) = 

        //    let center = 
        //        let middle = scene.bounds.Center
        //        Trafo3d.Translation -middle

        //    let pos = 
        //        [|
        //            for a in scene.meshes do
        //                yield a.geometry.IndexedAttributes.[DefaultSemantic.Positions] :?> V3f[]
        //        |]  |> Array.concat
        //            |> Array.map V3d.op_Explicit
        //            |> Array.map center.Forward.TransformPos

        //    let norm = 
        //        [|
        //            for a in scene.meshes do
        //                yield a.geometry.IndexedAttributes.[DefaultSemantic.Normals] :?> V3f[]
        //        |]  |> Array.concat
        //            |> Array.map V3d.op_Explicit

        //    pos
        //        |> Array.zip norm
        //        |> Array.sortBy ( fun _ -> rand.UniformDouble())
        //        |> (match cfg.VertexCount with Some count -> Array.take count | None -> id)
        //        |> Array.unzip
           

        //let Cids = [ for i in 0..cams-1 do yield CameraId(i) ]

        //let c = V3d.OOO
        //let r = 3000.0

        //let mutable i = 0.0
        //let step = (2.0 * Math.PI) / float cams

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
        