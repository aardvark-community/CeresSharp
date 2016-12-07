open System
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Ceres
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.WinForms


module BundlerTest =
    
    let createProblem (cameras : int) (points : int) =
        let rand = RandomSystem()

        let realPoints = Array.init points (fun _ -> rand.UniformV3dDirection())
        let realCameras = Array.init cameras (fun _ -> Camera3d.LookAt(rand.UniformV3dDirection() * 5.0, V3d.Zero, 2.0, V3d.OOI))

        let measurements =
            Array.init realCameras.Length (fun ci ->
                Seq.init realPoints.Length (fun i -> i, realCameras.[ci].Project realPoints.[i]) |> Map.ofSeq
            )

        realPoints, realCameras, {
            realPoints = realPoints.Length
            realCameras = realCameras.Length
            measurements = measurements
        }

    let private rand = RandomSystem()
    let rec solve (k : int) (p : BundlerSubProblem) =
        if p.subCameras.Count >= 2 * k then
            
            let l = p.subCameras.RandomOrder() |> Seq.toArray
            let l, r = Array.splitAt (l.Length / 2) l


            let half = Set.ofArray l
            let rest = Set.ofArray r

            let l = solve k { p with subCameras = half }
            let r = solve k { p with subCameras = rest }

            let res = BundlerSubSolution.merge l r
            Bundler.improveSubSolution res
        else
            Bundler.solveSubProblem p



    let syntheticCameras (cameras : int) (points : int) =
        let rand = RandomSystem()

        let realPoints = Array.init points (fun _ -> rand.UniformV3dDirection())
        let realCameras = Array.init cameras (fun _ -> Camera3d.LookAt(rand.UniformV3dDirection() * 10.0, V3d.Zero, 2.0, V3d.OOI))

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


        Log.startTimed "solver"
        let subSol = solve 4 { problem = problem; subCameras = Set.ofList [0 .. cameras - 1] }

        let sol = 
            { 
                problem = problem
                points = subSol.subPoints |> Map.toSeq |> Seq.map snd |> Seq.toArray
                cameras = subSol.subCameras |> Map.toSeq |> Seq.map snd |> Seq.toArray
            }


        let sol = Bundler.improve sol

        let err = sol |> BundlerSolution.errorMetrics
        Log.start "error metrics"
        Log.line "cost:    %A" err.cost
        Log.line "average: %.4f%%" (100.0 * err.average)
        Log.line "stdev:   %.4f%%" (100.0 * err.stdev)
        Log.line "min:     %.4f%%" (100.0 * err.min)
        Log.line "max:     %.4f%%" (100.0 * err.max)
        Log.stop()
        Log.stop()

        let input = { problem = problem; points = realPoints; cameras = realCameras }

        input, sol

module BundlerSolution =
    open Aardvark.SceneGraph

    let sg (cameraColor : C4b) (pointSize : int) (pointColor : C4b) (s : BundlerSolution) =
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
            |> Sg.shader { 
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.constantColor (C4f pointColor)
                do! DefaultSurfaces.pointSprite
                do! DefaultSurfaces.pointSpriteFragment
            }

        Sg.ofList [ points; cameras ]

module FundamentalMatrix =
    open OpenCvSharp

    let inline coerce< ^a, ^b when (^a or ^b) : (static member op_Implicit : ^a -> ^b) > (a : ^a) : ^b = 
        ((^a or ^b) : (static member op_Implicit : ^a -> ^b) (a))

    let inline ip (a : ^a) = coerce< ^a, InputArray > a
    let inline op (a : ^a) = coerce< ^a, OutputArray > a

    type Mat with
        member x.M33 =
            M33d(
                x.Get(0, 0), x.Get(0, 1), x.Get(0, 2),
                x.Get(1, 0), x.Get(1, 1), x.Get(1, 2),
                x.Get(2, 0), x.Get(2, 1), x.Get(2, 2)
            )
        member x.M44 =
            M44d(
                x.Get(0, 0), x.Get(0, 1), x.Get(0, 2), x.Get(0, 3),
                x.Get(1, 0), x.Get(1, 1), x.Get(1, 2), x.Get(1, 3),
                x.Get(2, 0), x.Get(2, 1), x.Get(2, 2), x.Get(2, 3),
                x.Get(3, 0), x.Get(3, 1), x.Get(3, 2), x.Get(3, 3)
            )



    let compute (l : Map<int, V2d>) (r : Map<int, V2d>) =
        let correspondences = Map.intersect l r |> Map.toSeq |> Seq.map snd |> Seq.toArray
        let count = correspondences.Length
        if count < 7 then failwithf "cannot compute FundamentalMatrix on less than 8 correspondences"

        use p0 = new Mat(count, 1, OpenCvSharp.MatType.CV_64FC2)
        use p1 = new Mat(count, 1, OpenCvSharp.MatType.CV_64FC2)

        for i in 0 .. count - 1 do
            let (l,r) = correspondences.[i]
            p0.Set(i, Vec2d(l.X, l.Y))
            p1.Set(i, Vec2d(r.X, r.Y))

        use res = Cv2.FindFundamentalMat(ip p0, ip p1, FundamentalMatMethod.Ransac, 3.0, 0.99)

//        M33d(
//            res.Get(0,0), res.Get(0,1), res.Get(0,2),
//            res.Get(1,0), res.Get(1,1), res.Get(1,2),
//            res.Get(2,0), res.Get(2,1), res.Get(2,2)
//        )

        use h0 = new Mat(3,4, MatType.CV_64FC1)
        use h1 = new Mat(3,4, MatType.CV_64FC1)
        Cv2.StereoRectifyUncalibrated(ip p0, ip p1, InputArray.op_Implicit res, Size(2.0, 2.0), op h0, op h1) |> ignore

        let h0m = h0.M33
        let h1m = h1.M33

        h0m, h1m


let test2 () =
    let realPoints, realCameras, problem = BundlerTest.createProblem 2 50
    let c0 = problem.measurements.[0] //|> Map.filter (fun k _ -> k < 7)
    let c1 = problem.measurements.[1] //|> Map.filter (fun k _ -> k < 7)


    let input =
        BundlerSolution.sg C4b.Blue 20 C4b.Yellow {
            problem = problem
            points = realPoints
            cameras = realCameras |> Array.take 2
        }

    match Bundler.tryRegisterCameras c0 c1 with
        | Some(c1, pts) ->

//            let projected = pts |> Map.toSeq |> Seq.map snd |> Seq.map cam.Project |> Seq.toArray
//            let real = c1 |> Map.toSeq |> Seq.map snd |> Seq.toArray
//            let res = Array.map2 (fun l r -> Vec.length (l - r))  projected real |> Array.sum
//            Log.warn "err: %A" res

            let c0 = Camera3d(V3d.Zero, V3d.Zero, 1.0)

            let res = 
                BundlerSolution.sg (C4b(255uy, 0uy, 0uy, 127uy)) 10 C4b.Red {
                    problem = problem
                    points = pts |> Map.toSeq |> Seq.map snd |> Seq.toArray
                    cameras = [|c0; c1|]
                }
                |> Sg.transform (Camera3d.Delta(c0, realCameras.[0]))
                |> Sg.pass (RenderPass.after "asdasd" RenderPassOrder.Arbitrary RenderPass.main)
                |> Sg.depthTest (Mod.constant DepthTestMode.None)
                |> Sg.blendMode (Mod.constant BlendMode.Blend)
            Sg.ofList [input; res]

        | None ->
            Sg.ofList [input]

let testGlobal() =
    let input, sol = BundlerTest.syntheticCameras 128 50



    let trafo = PointCloud.trafo sol.points input.points
    let input =
        input 
            |> BundlerSolution.sg (C4b(0uy, 0uy, 255uy, 127uy)) 10 C4b.Yellow
            |> Sg.pass (RenderPass.after "asdasd" RenderPassOrder.Arbitrary RenderPass.main)
            |> Sg.depthTest (Mod.constant DepthTestMode.None)
            |> Sg.blendMode (Mod.constant BlendMode.Blend)
    let sol =
        sol
            |> BundlerSolution.transformed trafo
            |> BundlerSolution.sg C4b.Green 20 C4b.Red
        
    Sg.ofList [ input; sol ]

[<EntryPoint>]
let main argv =
    Aardvark.Init()

    use app = new OpenGlApplication()
    use win = app.CreateSimpleRenderWindow(8)

    let cameraView = CameraView.lookAt (V3d(6,6,6)) V3d.Zero V3d.OOI
    let cameraView = cameraView |> DefaultCameraController.control win.Mouse win.Keyboard win.Time
    let frustum = win.Sizes |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y))

    
    let sg = 
        testGlobal()
            |> Sg.uniform "ViewportSize" win.Sizes
            |> Sg.viewTrafo (cameraView |> Mod.map CameraView.viewTrafo)
            |> Sg.projTrafo (frustum |> Mod.map Frustum.projTrafo)

    let task = app.Runtime.CompileRender(win.FramebufferSignature, sg)
    win.RenderTask <- task

    win.Run()

    0 // return an integer exit code
