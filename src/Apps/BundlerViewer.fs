namespace Examples

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Rendering.GL
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.WinForms

open FShade
open Aardvark.Reconstruction

module Viewer =

    module Surface =
        let compile (effects : seq<FShadeEffect>) =
            let e = FShade.Effect.compose effects
            FShadeSurface(e) :> ISurface 

        let camEffects() =
            [
                    toEffect (fun (v : Effects.Vertex) ->
                        vertex {
                            return { v with pos = V4d(-v.pos.X, -v.pos.Y, -0.99, v.pos.W ) }
                        })
                    toEffect DefaultSurfaces.trafo
                    toEffect DefaultSurfaces.diffuseTexture
                    toEffect (fun (v : Effects.Vertex) ->
                        fragment {
                            return V4d(v.c.XYZ, 0.5)
                        })
            ]

        let cams() =
            camEffects() |> compile |> Mod.constant
        
        let boxEffects() =
            [ 
                toEffect DefaultSurfaces.trafo
                toEffect (DefaultSurfaces.constantColor C4f.Red)
            ]

        let wire() =
            boxEffects() |> compile |> Mod.constant

        let pointEffects() =
            [ 
                toEffect DefaultSurfaces.trafo
                toEffect (DefaultSurfaces.constantColor C4f.Green)
                toEffect DefaultSurfaces.pointSprite
                toEffect DefaultSurfaces.pointSpriteFragment
            ]

        let points() =
            pointEffects() |> compile |> Mod.constant
            

    let blurb = RenderPass.after "blurb" RenderPassOrder.Arbitrary RenderPass.main
    let blerb = RenderPass.after "asd" RenderPassOrder.Arbitrary blurb
    
    let camSurface = lazy (Surface.cams())
    let wireSurface = lazy (Surface.wire())
    let pointsSurface = lazy (Surface.points())

    let toSceneGraph (prob : BundlerProblem) (state : BundlerState) (getImage : CameraId -> PixImage<byte>) : ISg =
        let frustum = Box3d(-V3d(1.0, 1.0, 100.0), V3d(1.0, 1.0, -1.01))
        let cameras = 
            state.cameras |> MapExt.map (fun cid c -> 
                let quad = 
                    Sg.fullScreenQuad 
                        |> Sg.diffuseTexture' (PixTexture2d(PixImageMipMap [|getImage cid :> PixImage|], true))
                        |> Sg.surface camSurface.Value
                        |> Sg.blendMode (Mod.constant BlendMode.Blend)
                        |> Sg.pass blerb

                let wb =
                    Sg.wireBox' C4b.Green frustum
                        |> Sg.surface wireSurface.Value 
                
                [wb; quad]   
                    |> Sg.ofList 
                    |> Sg.transform (c.ViewProjTrafo(100.0).Inverse)
            )   |> MapExt.toList |> List.map snd
                |> Sg.ofList

        let points =
            IndexedGeometry(
                Mode = IndexedGeometryMode.PointList,
                IndexedAttributes =
                    SymDict.ofList [
                        DefaultSemantic.Positions, 
                            state.points 
                                |> MapExt.toArray 
                                |> Array.map snd 
                                |> Array.map V3f.op_Explicit 
                                :> Array
                    ]
            )
            |> Sg.ofIndexedGeometry
            |> Sg.uniform "PointSize" (Mod.constant 5.0)
            |> Sg.surface pointsSurface.Value

        Sg.ofList [ points; cameras ]
            |> Sg.pass blurb
            |> Sg.blendMode (Mod.constant BlendMode.Blend)
            
    let ofObservations (obs : MapExt<CameraId, MapExt<TrackId, V2d>>) =
        
        let (state, prob) =
            let p = BundlerProblem.ofMeasurements obs
            CoolNameGoesHere.miniCV p
        
        Ag.initialize()
        Aardvark.Init()

        use app = new OpenGlApplication()
        use win = app.CreateSimpleRenderWindow(8)


        let proj = win.Sizes    |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 10000.0 (float s.X / float s.Y))
                                |> Mod.map Frustum.projTrafo

        let view = CameraView.lookAt (V3d(0.0, 50.0, 0.0)) V3d.Zero -V3d.OOI
                            |> DefaultCameraController.controlWithSpeed (Mod.init 2.5) win.Mouse win.Keyboard win.Time
                            |> Mod.map CameraView.viewTrafo

        let sg = toSceneGraph prob state (fun cid -> PixImage<byte>(Col.Format.RGBA, V2i(400,400) ) )
                    |> Sg.viewTrafo view
                    |> Sg.projTrafo proj

        let task = app.Runtime.CompileRender(win.FramebufferSignature, sg)
        win.RenderTask <- task

        win.Run()





