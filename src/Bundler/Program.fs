open System
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Ceres
open Aardvark.SceneGraph
open Aardvark.Rendering.Text
open Aardvark.Application
open Aardvark.Application.WinForms
open FShade


module BundlerTest =

    let createProblem (cameras : int) (points : int) =
        let rand = RandomSystem()

        let realPoints = Array.init points (fun _ -> rand.UniformV3dDirection())
        let realCameras = Array.init cameras (fun _ -> Camera3d.LookAt(rand.UniformV3dDirection() * 5.0, V3d.Zero, 1.0, V3d.OOI))

        let colors = [1.0..2000.0] |> Seq.mapi (fun i _ -> Feature.rgbaFromHsva(6.0 * float i / float [1.0..2000.0].Length, 0.5, 1.0).ToC4b()) |> Seq.toArray

        let measurements =
            Array.init realCameras.Length (fun ci ->
                let pxSize = 500
                let pimg = PixImage<byte>(Col.Format.RGBA, V2i(pxSize,pxSize))
                let matrix = pimg.GetMatrix<C4b>()
                matrix.SetByCoord ( fun _ -> C4b.White ) |> ignore
                let corr =
                    [
                        for i in 0 .. realPoints.Length - 1 do
                            if rand.UniformDouble() < 2.0 then
                                let jitter = rand.UniformV2dDirection() * rand.UniformDouble()* 0.005 * 5.0  // 5%
                                let p = jitter + realCameras.[ci].Project realPoints.[i] 
                                
                                let cp = V2i (V2d.Half + V2d(0.5 * p.X + 0.5, 0.5 - 0.5 * p.Y) * V2d(float pxSize,float pxSize))

                                matrix.SetCross(cp, 5, (colors.[i]))

                                yield i, p
                    ] |> List.toSeq

                ci,Map.ofSeq corr,pimg
            ) 

        let imgs = measurements |> Array.map ( fun (_,_,x) -> x )

        let measurements = 
            measurements |> Array.map ( fun (a,b,_) -> a,b) |> Map.ofArray

        let input = 
            BundlerInput.preprocess {
                measurements = measurements
            }

        realPoints, realCameras, imgs, BundlerInput.toProblem input

    let private rand = RandomSystem()
//    let rec solve (level : int) (k : int) (p : BundlerProblem) =
//        try
//            Log.startTimed "solve level %d" level
//            if p.cameras.Count >= 2 * k then
//            
//                let l = p.cameras.RandomOrder() |> Seq.toArray
//                let l, r = Array.splitAt (l.Length / 2) l
//
//
//                let half = Set.ofArray l
//                let rest = Set.ofArray r
//
//                let l = solve (level + 1) k { p with cameras = half }
//                let r = solve (level + 1) k { p with cameras = rest }
//
//                let res = BundlerSolution.merge l r
//                Bundler.improve res
//            else
//                Bundler.solve true p
//        finally
//            Log.stop()



    let syntheticCameras (cameras : int) (points : int) =
        let realPoints, realCameras, realPimgs, problem = createProblem cameras points


        Log.startTimed "solver"
        let sol = Bundler.solve true problem //solve 0 4 problem

        let err = sol |> BundlerSolution.errorMetrics
        Log.start "error metrics"
        Log.line "cost:    %A" err.cost
        Log.line "average: %.4f%%" (100.0 * err.average)
        Log.line "stdev:   %.4f%%" (100.0 * err.stdev)
        Log.line "min:     %.4f%%" (100.0 * err.min)
        Log.line "max:     %.4f%%" (100.0 * err.max)
        Log.stop()
        Log.stop()

        let input = { cost = 0.0; problem = problem; points = realPoints |> Seq.indexed |> Map.ofSeq; cameras = realCameras |> Seq.map ( fun c -> c,false ) |> Seq.indexed |> Map.ofSeq }

        input, sol, realPimgs

        
    let haus() =
        let images = 
            System.IO.Directory.GetFiles @"C:\bla\yolo\k-sub"
                |> Array.map PixImage.Create
                |> Array.map (fun pi -> pi.AsPixImage<byte>())

        let features = 
            images |> Array.mapParallel Akaze.ofImage

        let config =
            {
                threshold = 0.65
                guidedThreshold = 0.8
                minTrackLength = 2
                distanceThreshold = 0.006
            }

        let problem = failwith ""
            //Feature.toBundlerInput config images features

        ()
    open System.IO
    let iterative() =
        let path = @"C:\bla\yolo\k-sub"
        let images = 
            System.IO.Directory.GetFiles path
                |> Array.map PixImage.Create
                |> Array.map (fun pi -> pi.AsPixImage<byte>())

        Log.startTimed "detecting features"
        let features = 
            images |> Array.mapParallel Akaze.ofImage

        features |> Array.iter (Array.length >> printfn "feature#: %A")

        let solution = 
            
            let config =
                {
                    threshold = 0.5
                    guidedThreshold = 0.3
                    minTrackLength = 2
                    distanceThreshold = 0.008
                }

            let matches, lines = Feature.matches config features.[0] features.[1]
            if matches.Length > 0 then
                Log.line "matched %d/%d: %d" 0 1 matches.Length

            let lImage = images.[0]
            let rImage = images.[1]
            let rand = RandomSystem()

            let pp (p : V2d) =
                let pp = V2i (V2d.Half + V2d(0.5 * p.X + 0.5, 0.5 - 0.5 * p.Y) * V2d rImage.Size)
                pp

            for (li,ri), line in Array.zip matches lines do
                            
                let col = rand.UniformC3f().ToC4b()

                let lf = features.[0].[li]
                let rf = features.[1].[ri]
                let setP f (file : PixImage<byte>) =
                    let p = f.ndc
                    let pp = V2i (V2d.Half + V2d(0.5 * p.X + 0.5, 0.5 - 0.5 * p.Y) * V2d file.Size)
                    let size = clamp 15 30 (ceil (f.size) |> int)

                    file.GetMatrix<C4b>().SetCross(pp, size,    col)
                    file.GetMatrix<C4b>().SetCircle(pp, size,   col)
                            



                setP lf lImage
                setP rf rImage

                let mutable line2d = Line2d()
                if Box2d(-V2d.II, V2d.II).Intersects(line, &line2d) then
                    let p0 = line2d.P0
                    let p1 = line2d.P1
                    rImage.GetMatrix<C4b>().SetLine(pp p0, pp p1, col)


                        
            let path = sprintf @"C:\bla\yolo\k-sub\out"

            if Directory.Exists path |> not then Directory.CreateDirectory path |> ignore

            lImage.SaveAsImage (Path.combine [path; sprintf "image%d.jpg" 0])
            rImage.SaveAsImage (Path.combine [path; sprintf "image%d.jpg" 1])


        System.Environment.Exit 0
        Log.stop()
//
//        let config =
//            {
//                threshold = 0.65
//                minTrackLength = 3
//                distanceThreshold = 0.003
//            }
//
//        Log.startTimed "solving iteratively"
//        
//        let solution = Feature.solveIteratively config (Some path) images features
//
//        Log.stop ()

        failwith ""
            

    let kermit() =
        let images = 
            System.IO.Directory.GetFiles @"C:\Users\schorsch\Desktop\bundling\kermit"
                |> Array.map PixImage.Create
                |> Array.map (fun pi -> pi.AsPixImage<byte>())

        Log.startTimed "detecting features"
        let features = 
            images |> Array.mapParallel Akaze.ofImage

        Log.stop()

        let config =
            {
                threshold = 0.65
                guidedThreshold = 0.8
                minTrackLength = 3
                distanceThreshold = 0.003
            }

        Log.startTimed "matching features"
        let problem : BundlerProblem = failwith ""
//            Feature.toBundlerInput config images features
//                |> BundlerInput.preprocess
//                |> BundlerInput.toProblem

        Log.stop()

        if problem.cameras.Count >  0 then

            Log.startTimed "solver"
            let sol = Bundler.solve true problem //solve 0 4 problem

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
    let input, sol, pimgs = BundlerTest.syntheticCameras 2 50

    use app = new OpenGlApplication()
    use win = app.CreateSimpleRenderWindow(8)

    let trafo = PointCloud.trafo2 sol.points input.points
    let input =
        SceneGraph.ofBundlerSolution (C4b(0uy, 0uy, 255uy, 127uy)) 10 C4b.Yellow input pimgs (Mod.constant Bam.Oida)
            |> Sg.pass (RenderPass.after "asdasd" RenderPassOrder.Arbitrary RenderPass.main)
            |> Sg.depthTest (Mod.constant DepthTestMode.None)
            |> Sg.blendMode (Mod.constant BlendMode.Blend)

    let s = sol |> BundlerSolution.transformed trafo
    let sol =
        SceneGraph.ofBundlerSolution C4b.Green 20 C4b.Red s pimgs (Mod.constant Bam.Oida)
        
    let stuff = Sg.ofList [ (* input; *) sol ]

    let b = Bam.Oida |> Mod.init

    let frustum = win.Sizes |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y))
    let cameraView = CameraView.lookAt (V3d(0.0, 2.0, 0.0)) V3d.Zero -V3d.OOI

    let lastSpace = Mod.init DateTime.Now
    lastSpace |> Mod.unsafeRegisterCallbackKeepDisposable ( fun _ -> printfn "Recentering camera." ) |> ignore

    let cameraView = 
        let im = Mod.custom ( fun a ->
                    
            lastSpace.GetValue a |> ignore
            cameraView |> DefaultCameraController.controlWithSpeed (Mod.init 2.5) win.Mouse win.Keyboard win.Time
        )

        let far = 1000.0
        let getCam i =
            try 
                (s.cameras.[i] |> fst).ViewProjTrafo far
            with _ -> Trafo3d.Identity

        adaptive {
            let! b = b
            match b with
            | Oida ->
                let! im = im
                let! cv = im
                let! f = frustum
                let f = f |> Frustum.projTrafo
                return cv |> CameraView.viewTrafo, f
            | Fix i ->
                return Trafo3d.Identity, (getCam i)
        }

    let sg = stuff  |> Sg.viewTrafo (cameraView |> Mod.map fst )
                    |> Sg.projTrafo (cameraView |> Mod.map snd )

    let task = app.Runtime.CompileRender(win.FramebufferSignature, sg)
    win.RenderTask <- task
    
    win.Keyboard.KeyDown(Keys.D1).Values.Add ( fun _ -> transact ( fun _ -> b.Value <- Fix 0))
    win.Keyboard.KeyDown(Keys.D2).Values.Add ( fun _ -> transact ( fun _ -> b.Value <- Fix 1))
    win.Keyboard.KeyDown(Keys.D3).Values.Add ( fun _ -> transact ( fun _ -> b.Value <- Fix 2))
    win.Keyboard.KeyDown(Keys.D4).Values.Add ( fun _ -> transact ( fun _ -> b.Value <- Fix 3))
    win.Keyboard.KeyDown(Keys.Space).Values.Add ( fun _ -> transact ( fun _ -> b.Value <- Oida))

    win.Run()

let testKermit() =
    let sol = BundlerTest.kermit()
    sol |> SceneGraph.ofBundlerSolution C4b.Green 20 C4b.Red
        
open System.IO
open MBrace.FsPickler

let gen (places : int) =
    
    let rand = RandomSystem()

    let ser = FsPickler.CreateBinarySerializer()
    [|
        for i in 0..1000000 do
            yield rand.UniformV3d(Box3d.Unit.Transformed(Trafo3d.Scale 2.0).Translated(-V3d.III))
    |] |> Array.map ( fun v -> V3d(Math.Round(v.X,places),Math.Round(v.Y,places),Math.Round(v.Z,places) ) )
       |> Array.distinct
       |> ser.Pickle
       |> File.writeAllBytes @"C:\blub\random"

[<EntryPoint>]
let main argv =
    Ag.initialize()
    Aardvark.Init()

    //testGlobal()
    //System.Environment.Exit 0
    
    let path = @"C:\blub\yolo"

    //PairViewer.app path
    //BundlerViewer.folder path

    Example.renderSponza @"D:\file\sponza_bun\sponzaVertices"
    BundlerViewer.sponza @"D:\file\sponza_bun\sponzaVertices"

    0 // return an integer exit code
