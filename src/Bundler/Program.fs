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
                let corr =
                    seq {
                        for i in 0 .. realPoints.Length - 1 do
                            if rand.UniformDouble() < 2.0 then
                                let jitter = rand.UniformV2dDirection() * rand.UniformDouble()* 0.005 * 3.0  // 3%
                                let p = jitter + realCameras.[ci].Project realPoints.[i] 
                                yield i, p
                    }

                Map.ofSeq corr
            )

        let input = 
            BundlerInput.preprocess {
                measurements = measurements
            }

        realPoints, realCameras, BundlerInput.toProblem input

    let private rand = RandomSystem()
    let rec solve (level : int) (k : int) (p : BundlerProblem) =
        try
            Log.startTimed "solve level %d" level
            if p.cameras.Count >= 2 * k then
            
                let l = p.cameras.RandomOrder() |> Seq.toArray
                let l, r = Array.splitAt (l.Length / 2) l


                let half = Set.ofArray l
                let rest = Set.ofArray r

                let l = solve (level + 1) k { p with cameras = half }
                let r = solve (level + 1) k { p with cameras = rest }

                let res = BundlerSolution.merge l r
                Bundler.improve res
            else
                Bundler.solve p
        finally
            Log.stop()



    let syntheticCameras (cameras : int) (points : int) =
        let realPoints, realCameras, problem = createProblem cameras points


        Log.startTimed "solver"
        let sol = Bundler.solve problem //solve 0 4 problem

        let err = sol |> BundlerSolution.errorMetrics
        Log.start "error metrics"
        Log.line "cost:    %A" err.cost
        Log.line "average: %.4f%%" (100.0 * err.average)
        Log.line "stdev:   %.4f%%" (100.0 * err.stdev)
        Log.line "min:     %.4f%%" (100.0 * err.min)
        Log.line "max:     %.4f%%" (100.0 * err.max)
        Log.stop()
        Log.stop()

        let input = { cost = 0.0; problem = problem; points = realPoints |> Seq.indexed |> Map.ofSeq; cameras = realCameras |> Seq.indexed |> Map.ofSeq }

        input, sol

    let testMatching_fileOut configs =
        let files = System.IO.Directory.GetFiles @"C:\bla\yolo" 
        let features = 
            files
                |> Array.map Akaze.ofFile

        for config in configs do
            Feature.toBundlerInput config files features |> ignore

    let kermit() =
        let files = System.IO.Directory.GetFiles @"C:\bla\yolo\k" 
        let features = 
            files
                |> Array.map Akaze.ofFile

        let config =
            {
                threshold = 0.8
                minTrackLength = 3
                reprDistance = 0.02
            }

        let problem = 
            Feature.toBundlerInput config files features
                |> BundlerInput.preprocess
                |> BundlerInput.toProblem

        if problem.cameras.Count >  0 then

            Log.startTimed "solver"
            let sol = Bundler.solve problem //solve 0 4 problem

            let err = sol |> BundlerSolution.errorMetrics
            Log.start "error metrics"
            Log.line "cost:    %A" err.cost
            Log.line "average: %.4f%%" (100.0 * err.average)
            Log.line "stdev:   %.4f%%" (100.0 * err.stdev)
            Log.line "min:     %.4f%%" (100.0 * err.min)
            Log.line "max:     %.4f%%" (100.0 * err.max)
            Log.stop()
            Log.stop()
            sol
        else
            { cost = 0.0; problem = problem; points = Map.empty; cameras = Map.empty }

module BundlerSolution =
    open Aardvark.SceneGraph

    let sg (cameraColor : C4b) (pointSize : int) (pointColor : C4b) (s : BundlerSolution) =
        let frustum = Box3d(-V3d(1.0, 1.0, 10000.0), V3d(1.0, 1.0, -2.0))
        let cameras = 
            s.cameras |> Map.toSeq |> Seq.map (fun (_,c) -> 
                Sg.wireBox' C4b.Green frustum
                    |> Sg.transform (c.ViewProjTrafo(100.0).Inverse)
            )
            |> Sg.ofSeq
            |> Sg.shader { 
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.constantColor (C4f cameraColor)
            }

        let points =
            IndexedGeometry(
                Mode = IndexedGeometryMode.PointList,
                IndexedAttributes =
                    SymDict.ofList [
                        DefaultSemantic.Positions, s.points |> Map.toSeq |> Seq.map snd |> Seq.map V3f |> Seq.toArray :> Array
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


let testGlobal() =
    let input, sol = BundlerTest.syntheticCameras 10 50

    let trafo = PointCloud.trafo2 sol.points input.points
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

let testKermit() =
    let sol = BundlerTest.kermit()
    sol |> BundlerSolution.sg C4b.Green 20 C4b.Red
        
    

open System.IO

[<EntryPoint>]
let main argv =
    Aardvark.Init()
    use app = new OpenGlApplication()
    use win = app.CreateSimpleRenderWindow(8)

//   let configs = 
//       [
//           for threshold in [0.6 .. 0.1 .. 0.9 ] do
//               for minTrackLength in [2 .. 4] do
//                   for reprDistance in [0.01 .. 0.015 .. 0.065] do
//                       yield {
//                           threshold = threshold
//                           minTrackLength = minTrackLength
//                           reprDistance = reprDistance
//                       }
//       ]
//
//   do BundlerTest.testMatching_fileOut configs
    let cameraView = CameraView.lookAt (V3d(6,6,6)) V3d.Zero V3d.OOI
    let cameraView = cameraView |> DefaultCameraController.control win.Mouse win.Keyboard win.Time
    let frustum = win.Sizes |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y))

    
    let sg = 
        testKermit()
            |> Sg.uniform "ViewportSize" win.Sizes
            |> Sg.viewTrafo (cameraView |> Mod.map CameraView.viewTrafo)
            |> Sg.projTrafo (frustum |> Mod.map Frustum.projTrafo)

    let task = app.Runtime.CompileRender(win.FramebufferSignature, sg)
    win.RenderTask <- task

    win.Run()

    0 // return an integer exit code
