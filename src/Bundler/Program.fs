open System
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Ceres
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.WinForms
open FShade

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

                ci,Map.ofSeq corr
            ) |> Map.ofArray

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

        let problem = 
            Feature.toBundlerInput config images features

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
        let problem = 
            Feature.toBundlerInput config images features
                |> BundlerInput.preprocess
                |> BundlerInput.toProblem

        Log.stop()

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
    Ag.initialize()
    Aardvark.Init()

    let path = @"C:\bla\yolo\k-sub"




    use app = new OpenGlApplication()
    use win = app.CreateSimpleRenderWindow(8)


    let cameraView = CameraView.lookAt (V3d(0.0, 0.0, 2.0)) V3d.Zero V3d.OIO

    let cameraView = cameraView |> DefaultCameraController.controlWithSpeed (Mod.init 0.2) win.Mouse win.Keyboard win.Time
    let frustum = win.Sizes |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y))

    let plane (isLeft : bool) (img : PixImage<byte>) = 
        let sizeX = float img.Size.X
        let sizeY = float img.Size.Y
        let aspect = sizeY/sizeX   

        let trans = if isLeft then -1.0 else 1.0

        Sg.fullScreenQuad 
            |> Sg.transform (Trafo3d.Scale(1.0,aspect,1.0) * Trafo3d.Translation(trans,0.0,0.0))
            |> Sg.diffuseTexture' (PixTexture2d(PixImageMipMap(img),true))

    let images = 
        System.IO.Directory.GetFiles path
            |> Array.map PixImage.Create
            |> Array.map (fun pi -> pi.AsPixImage<byte>())

    let features =
        images |> Array.mapParallel Akaze.ofImage

    let lFtr = features.[0]
    let rFtr = features.[1]

    let config =
        {
            threshold = 0.8
            guidedThreshold = 0.3
            minTrackLength = 2
            distanceThreshold = 0.008
        }

    let mc = 
        Feature.matchCandidates config lFtr rFtr
    let m2d =
        mc |> Array.map ( fun (li,ri) ->
            let lf = lFtr.[li]
            let rf = rFtr.[ri]
            Match2d(lf.ndc, rf.ndc - lf.ndc, MatchProblem.o (rf.angle - lf.angle), li, ri)
        )


    let lImg = images.[0]
    let rImg = images.[1]

    let lAspect = float lImg.Size.Y / float lImg.Size.X
    let rAspect = float rImg.Size.Y / float rImg.Size.X

    let rand = RandomSystem()


    let matches (lampta : float) (sickma : float) =
        Log.startTimed "matching { lampta: %A; sickma: %A }" lampta sickma
        let fn = MatchProblem.likelihood lampta sickma m2d

        let m2g =
            m2d |> Array.filter (fun m -> 
                fn m >= 0.5
            )

        Log.line "found %d matches (%d candidates)" m2g.Length m2d.Length
        Log.stop()

        let good = 
            m2g |> Array.map (fun m -> m.Left, m.Right)

        let lines = 
            good |> Array.collect (fun (li, ri) -> 
                let lf = lFtr.[li]
                let rf = rFtr.[ri]
                let p0 = lf.ndc - V2d(1.0, 0.0)
                let p1 = rf.ndc + V2d(1.0, 0.0)
                [| V3d(p0.X, lAspect * p0.Y , 0.001); V3d(p1.X, rAspect * p1.Y, 0.001)|]
            )

        let colors = Array.init good.Length (ignore >> rand.UniformC3f >> C4b) |> Array.collect (fun v -> [| v; v |])

        lines, colors


    let currentLampta = Mod.init 35.0
    let currentSickma = Mod.init 0.5

    let result = Mod.map2 matches currentLampta currentSickma





    let lineSg = 
        Sg.draw IndexedGeometryMode.LineList
            |> Sg.vertexAttribute DefaultSemantic.Positions (Mod.map fst result)
            |> Sg.vertexAttribute DefaultSemantic.Colors (Mod.map snd result)
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
            }
    let sg = 
        [plane true lImg; plane false rImg; lineSg]
            |> Sg.ofList
            |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.diffuseTexture
                }
            |> Sg.viewTrafo (cameraView |> Mod.map CameraView.viewTrafo)
            |> Sg.projTrafo (frustum |> Mod.map Frustum.projTrafo)

    let task = app.Runtime.CompileRender(win.FramebufferSignature, sg)
    win.RenderTask <- task

    let runner =
        async {
            do! Async.SwitchToNewThread()
            while true do
                Console.Write("bundler# ")
                let line = Console.ReadLine()

                let rx = System.Text.RegularExpressions.Regex @"^[ \t]*(?<par>l|s)[ \t]*\=[ \t]*(?<value>[0-9]+(\.[0-9]+)?)$"
                let m = rx.Match line
                if m.Success then
                    let name = m.Groups.["par"].Value
                    let value = System.Double.Parse(m.Groups.["value"].Value, System.Globalization.CultureInfo.InvariantCulture)
                    match name with
                        | "l" -> transact(fun () -> currentLampta.Value <- value)
                        | _ -> transact(fun () -> currentSickma.Value <- value)

        }

    Async.Start runner


    win.Run()

    0 // return an integer exit code
