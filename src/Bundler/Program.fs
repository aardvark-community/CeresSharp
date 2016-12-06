open System
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Ceres
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.WinForms


module PointCloud =
    open CeresSharp

    let trafo (source : V3d[]) (target : V3d[]) =
        
        use p = new Problem()

        let b = p.AddParameterBlock([| 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 1.0; 1.0; 1.0 |])

        let res = Array.zeroCreate (3 * source.Length)
        p.AddCostFunction(res.Length, b, fun inputs ->
            let shift   = V3s(inputs.[0], inputs.[1], inputs.[2])
            let aa      = V3s(inputs.[3], inputs.[4], inputs.[5])
            let scale   = V3s(inputs.[6], inputs.[7], inputs.[8])

            let mutable oi = 0

            for i in 0 .. source.Length - 1 do  
                let src = source.[i]
                let dst = target.[i]

                let test = scale * AngleAxis.RotatePoint(aa, src + shift)
                let r = test - dst

                res.[oi + 0] <- r.X
                res.[oi + 1] <- r.Y
                res.[oi + 2] <- r.Z
                oi <- oi + 3

            res
        )

        let worked = p.Solve(CeresOptions(150, CeresSolverType.SparseSchur, true, 1.0E-10, 1.0E-10, 1.0E-10))
        if not worked then
            Log.warn "error in ceres"

        let res = b.Result

        let trafo =
            Trafo3d.Translation(res.[0], res.[1], res.[2]) *
            AngleAxis.Trafo(V3d(res.[3], res.[4], res.[5])) *
            Trafo3d.Scale(res.[6], res.[7], res.[8])
            
        trafo
  

module BundlerTest =
    
    let syntheticCameras (cameras : int) (points : int) =
        let rand = RandomSystem()

        let realPoints = Array.init points (fun _ -> rand.UniformV3dDirection())
        let realCameras = Array.init cameras (fun _ -> Camera3d.LookAt(rand.UniformV3dDirection() * 5.0, V3d.Zero, 2.0, V3d.OOI))

        let measurements =
            Array.init realCameras.Length (fun ci ->
                Seq.init realPoints.Length (fun i -> i, realCameras.[ci].Project realPoints.[i]) |> Map.ofSeq
            )

        let problem =
            {
                realPoints = realPoints.Length
                realCameras = realCameras.Length
                measurements = measurements
            }

        let sol = Bundler.solve problem

        let err = sol |> BundlerSolution.errorMetrics
        Log.start "error metrics"
        Log.line "cost:    %A" err.cost
        Log.line "average: %.4f%%" (100.0 * err.average)
        Log.line "stdev:   %.4f%%" (100.0 * err.stdev)
        Log.line "min:     %.4f%%" (100.0 * err.min)
        Log.line "max:     %.4f%%" (100.0 * err.max)
        Log.stop()

        let input = { problem = problem; points = realPoints; cameras = realCameras }

        input, sol

module BundlerSolution =
    open Aardvark.SceneGraph

    let sg (size : IMod<V2i>) (cameraColor : C4b) (pointSize : int) (pointColor : C4b) (s : BundlerSolution) =
        let frustum = Box3d(-V3d(1.0, 1.0, 10000.0), V3d(1.0, 1.0, -2.0))
        let cameras = 
            s.cameras |> Array.map (fun c -> 
                Sg.wireBox' C4b.Green frustum
                    |> Sg.transform (c.ViewProjTrafo(100.0).Inverse)
            )
            |> Sg.ofArray
            |> Sg.shader { 
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.constantColor (C4f cameraColor)
            }

        let points =
            IndexedGeometry(
                Mode = IndexedGeometryMode.PointList,
                IndexedAttributes =
                    SymDict.ofList [
                        DefaultSemantic.Positions, s.points |> Array.map V3f :> Array
                    ]
            )
            |> Sg.ofIndexedGeometry
            |> Sg.uniform "PointSize" (Mod.constant (float pointSize))
            |> Sg.uniform "ViewportSize" size
            |> Sg.shader { 
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.constantColor (C4f pointColor)
                do! DefaultSurfaces.pointSprite
                do! DefaultSurfaces.pointSpriteFragment
            }

        Sg.ofList [ points; cameras ]
     







[<EntryPoint>]
let main argv =
    Aardvark.Init()
     
    let input, sol = BundlerTest.syntheticCameras 6 50

    use app = new OpenGlApplication()
    use win = app.CreateSimpleRenderWindow(8)

    let cameraView = CameraView.lookAt (V3d(6,6,6)) V3d.Zero V3d.OOI
    let cameraView = cameraView |> DefaultCameraController.control win.Mouse win.Keyboard win.Time
    let frustum = win.Sizes |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y))

    let trafo = PointCloud.trafo sol.points input.points

    let input =
        input 
            |> BundlerSolution.sg win.Sizes C4b.Blue 10 C4b.Yellow
            |> Sg.pass (RenderPass.after "asdasd" RenderPassOrder.Arbitrary RenderPass.main)
            |> Sg.depthTest (Mod.constant DepthTestMode.None)
        
    let sol =
        sol
            |> BundlerSolution.sg win.Sizes C4b.Green 20 C4b.Red
            |> Sg.transform trafo
        
    let sg = 
        Sg.ofList [ input; sol ]
            |> Sg.viewTrafo (cameraView |> Mod.map CameraView.viewTrafo)
            |> Sg.projTrafo (frustum |> Mod.map Frustum.projTrafo)
    
    let task = app.Runtime.CompileRender(win.FramebufferSignature, sg)
    win.RenderTask <- task

    win.Run()

    0 // return an integer exit code
