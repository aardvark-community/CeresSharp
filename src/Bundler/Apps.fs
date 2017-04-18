namespace Aardvark.Ceres

open System

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.Application

open Aardvark.Rendering.Text
open Aardvark.Application.WinForms

open FShade

module Shader =
    let compile (effects : seq<FShadeEffect>) =
        let e = FShade.Effect.compose effects
        FShadeSurface(e) :> ISurface 

    let camEffects () =
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

    let boxEffects (cameraColor : C4b) =
        [ 
            toEffect DefaultSurfaces.trafo
            toEffect (DefaultSurfaces.constantColor (C4f cameraColor))
        ]

    let pointEffects (pointColor : C4b) =
        [ 
            toEffect DefaultSurfaces.trafo
            toEffect (DefaultSurfaces.constantColor (C4f pointColor))
            toEffect DefaultSurfaces.pointSprite
            toEffect DefaultSurfaces.pointSpriteFragment
        ]

    let solutionSurface () =
        camEffects() |> compile |> Mod.constant

    let surfaceBox (cameraColor : C4b) =
        boxEffects cameraColor |> compile |> Mod.constant
        
    let surfacePoint (pointColor : C4b) =
        pointEffects pointColor |> compile |> Mod.constant

    let compileApp (win : SimpleRenderWindow) (effects : seq<FShadeEffect>) =
        let s = compile effects
        win.Runtime.PrepareSurface(win.FramebufferSignature,s) :> ISurface |> Mod.constant

    let surfaceApp (win : SimpleRenderWindow) =
        camEffects() |> compileApp win

    let surfaceBoxApp (cameraColor : C4b) (win : SimpleRenderWindow) =
        boxEffects cameraColor |> compileApp win

    let surfacePointApp (pointColor : C4b) (win : SimpleRenderWindow) =
        pointEffects pointColor |> compileApp win

module PairViewer =

    let fst' (a,b,c) = a
    let snd' (a,b,c) = b
    let trd' (a,b,c) = c

    type FeatureSelection =
    | Akaze
    | Brisk
    | Orb
    

    type stuffStats = 
        {
            startCount : int
            predCount : int
            superCount : int
            superWeightSum : V2d
            superAvg    : V2d
            superVar    : V2d
        }

    open Bundle

    let app path =

        use app = new OpenGlApplication()
        use win = app.CreateSimpleRenderWindow(8)


        let cameraView = CameraView.lookAt (V3d(0.0, 0.0, 2.0)) V3d.Zero V3d.OIO

        let lastSpace = Mod.init DateTime.Now
        lastSpace |> Mod.unsafeRegisterCallbackKeepDisposable ( fun _ -> printfn "Recentering camera." ) |> ignore
        let cameraView = 
            let im = Mod.custom ( fun a ->
                lastSpace.GetValue a |> ignore
                cameraView |> DefaultCameraController.controlWithSpeed (Mod.init 0.5) win.Mouse win.Keyboard win.Time
            )

            adaptive {
                let! im = im
                let! cv = im
                return cv
            }

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
    
        let currentImgs = Mod.init (images.[0], images.[1])
        let featureType = Mod.init Akaze

        let cfg : stuffConfig = 
            if System.IO.File.Exists (configfile()) then
                Log.line @"Loading config from C:\blub"
                let pickler = MBrace.FsPickler.FsPickler.CreateXmlSerializer(indent=true)
                File.readAllText (configfile()) |> pickler.UnPickleOfString
            else
                {
                    pLambda = 35.0
                    pSigma  = 0.5
                    pProb   = 0.5
                    aLambda = 20.0
                    aSigma  = 0.5
                    aThresh = 0.01
                }

//        let currentLampta   = Mod.init 35.0
//        let currentSickma   = Mod.init 0.5
//        let probability     = Mod.init 0.5
//        let aLambda         = Mod.init 20.0
//        let aSigma          = Mod.init 0.5
//        let aThreshold      = Mod.init 0.01
//        let useA            = Mod.init true

        let currentLampta   = Mod.init cfg.pLambda
        let currentSickma   = Mod.init cfg.pSigma
        let probability     = Mod.init cfg.pProb
        let aLambda         = Mod.init cfg.aLambda
        let aSigma          = Mod.init cfg.aSigma
        let aThreshold      = Mod.init cfg.aThresh
        let useA            = Mod.init true

        let config =
            {
                threshold = 0.9
                guidedThreshold = 0.3
                minTrackLength = 2
                distanceThreshold = 0.008
            }
        let result = Mod.custom ( fun a ->

                let (lImg,rImg) = currentImgs.GetValue a
                let useFeature = featureType.GetValue a

                let (lFtr,rFtr) = 
                    match useFeature with
                    | Akaze ->  lImg |> Akaze.ofImage, rImg |> Akaze.ofImage
                    | Orb ->    lImg |> Orb.ofImage,   rImg |> Orb.ofImage
                    | Brisk ->  lImg |> Brisk.ofImage, rImg |> Brisk.ofImage

                let mc = 
                    Feature.bruteforceMatch config.threshold lFtr rFtr

                let m2d =
                    mc |> Array.map ( fun (li,ri) ->
                        let lf = lFtr.[li]
                        let rf = rFtr.[ri]
                        Match2d(lf.ndc, rf.ndc - lf.ndc, MatchProblem.o (rf.angle - lf.angle), li, ri)
                    )


                let lAspect = float lImg.Size.Y / float lImg.Size.X
                let rAspect = float rImg.Size.Y / float rImg.Size.X

                let rand = RandomSystem()

                let averageByV2 (f : 'a -> V2d) (vs : 'a[]) =
                    (vs |> Array.fold ( fun vs v -> vs + f v ) V2d.OO) / (float vs.Length)
        
                let probability = probability.GetValue a
                let thresh = aThreshold.GetValue a

                let matches (lampta : float) (sickma : float) (affineLambda : float) (affineSigma : float) (useAffine : bool) =
                    Log.startTimed "matching { lampta: %A; sickma: %A }" lampta sickma
                    
                    let m2g = Match.probability lampta sickma probability m2d

                    Log.line "found %d matches (%d candidates)" m2g.Length m2d.Length
        
                    let (m2g,stats) =
                        if useAffine then
                            if m2g.Length >= 20 then 
                                let (qn,ex,ey,dx,dy) = MatchProblem.affineDistance affineLambda affineSigma m2g

                                let (supermatches,ws) = 
                                    m2g |> Array.map ( fun m ->
                                        let w = qn m
                                        m,w
                                    )   |> Array.unzip

                                let supermatches = supermatches |> Array.filteri ( fun i _ -> ws.[i].LengthSquared < thresh)

                                Log.line "(la=%A sa=%A) affine matches remaining: %A" affineLambda affineSigma supermatches.Length

                                let avg = ws |> averageByV2 id
                                let vari = ws |> averageByV2 ( fun w -> (w - avg) * (w - avg) )

                                Log.line "Weight stats: \n totalcount=%A \n thresh=%A \n belowThresh=%A \n average=%A \t (var=%A)\n" ws.Length thresh supermatches.Length avg vari

                                supermatches,    {
                                                    startCount = m2d.Length
                                                    predCount = m2g.Length
                                                    superCount = supermatches.Length
                                                    superWeightSum = V2d(ex,ey)
                                                    superAvg    = avg
                                                    superVar    = vari
                                                 }
                            else
                                Log.warn "not enough points (only %A, need 20) for supermatch" m2g.Length
                                m2g,{
                                        startCount = m2d.Length
                                        predCount = m2g.Length
                                        superCount = 0
                                        superWeightSum = V2d.OO
                                        superAvg    = V2d.OO
                                        superVar    = V2d.OO
                                        }
                        else
                            m2g,{
                                startCount = m2d.Length
                                predCount = m2g.Length
                                superCount = 0
                                superWeightSum = V2d.OO
                                superAvg    = V2d.OO
                                superVar    = V2d.OO
                                }
        
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

                    lines, colors, stats


                let l = currentLampta.GetValue a
                let s = currentSickma.GetValue a
                let al = aLambda.GetValue a
                let as' = aSigma.GetValue a
                let ua = useA.GetValue a

                matches l s al as' ua, lFtr.Length, rFtr.Length
             )

        let cnts = result |> Mod.map ( fun (_,c1,c2) -> V2i(c1,c2) )

        let configText =
            let cfg l s la sa t c =
                let t = match t with Brisk -> "Brisk" | Orb -> "Orb" | Akaze -> "Akaze"
                sprintf "\n
                  MotionPred:\tAffine:\t\n
                  λ = %A\t     λa = %A\t\n
                  σ = %A\t     σa = %A\t\n
                  \n
                  FeatureType: %A\n
                  Count: %A\n
                \n" l la s sa t c

            adaptive {
                let! l = currentLampta
                let! s = currentSickma
                let! la = aLambda
                let! sa = aSigma
                let! t = featureType
                let! c = cnts
                return cfg l s la sa t c
            }

        let xt = Mod.init 0.0
        let yt = Mod.init 0.0

        xt |> Mod.unsafeRegisterCallbackKeepDisposable ( printfn "X=%A" ) |> ignore
        yt |> Mod.unsafeRegisterCallbackKeepDisposable ( printfn "Y=%A" ) |> ignore

        win.Keyboard.Down.Values.Add( fun k ->
            match k with
            | Keys.Left     -> transact ( fun _ -> xt.Value <- xt.Value - 1.0)
            | Keys.Right    -> transact ( fun _ -> xt.Value <- xt.Value + 1.0)
            | Keys.Down     -> transact ( fun _ -> yt.Value <- yt.Value - 1.0)
            | Keys.Up       -> transact ( fun _ -> yt.Value <- yt.Value + 1.0)
            | Keys.Space    -> transact ( fun _ -> lastSpace.Value <- DateTime.Now)
            | _ -> ()
        )

        let stats (c : IMod<stuffStats>) =  
            adaptive {
                let! x = c
                let! p = probability
                let! ta = aThreshold
                return 
                    sprintf "\n
                        pProbability = %A\n
                        aThreshold = %A\n
                        \n
                        bfMatches = %A\n
                        afterPrediction = %A\n
                        afterAffinity = %A\n
                        \n
                        Ex,Ey = %A    \n
                        average = %A  \n
                        variance = %A \n\n
                    "p ta x.startCount x.predCount x.superCount x.superWeightSum x.superAvg x.superWeightSum
            }

        let statsSg = Mod.map (fst'>>trd') result 
                        |> stats       
                        |> Sg.text (Font.create "Consolas" FontStyle.Regular) C4b.White 
                        //|> Sg.trafo (Mod.map2 ( fun x y -> Trafo3d.Translation(x,y,0.0)) xt yt)
                        |> Sg.translate -19.0 -15.0 0.0
                        |> Sg.scale 0.05

        let configSg =
            Sg.text (Font.create "Consolas" FontStyle.Regular) C4b.White configText
                //|> Sg.trafo (Mod.map2 ( fun x y -> Trafo3d.Translation(x,y,0.0)) xt yt)
                |> Sg.translate -39.0 -15.0 0.0
                |> Sg.scale 0.05
    
        let textSg = [configSg; statsSg] |> Sg.ofList

        let lineSg = 
            Sg.draw IndexedGeometryMode.LineList
                |> Sg.vertexAttribute DefaultSemantic.Positions (Mod.map (fst'>>fst') result)
                |> Sg.vertexAttribute DefaultSemantic.Colors (Mod.map (fst'>>snd') result)
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.vertexColor
                }
        let sg = 
            aset {
                yield lineSg 
                yield textSg
                let! imgs = currentImgs
                let (lImg, rImg) = imgs
                yield plane true  lImg
                yield plane false rImg
            }   |> Sg.set
                |> Sg.shader {
                        do! DefaultSurfaces.trafo
                        do! DefaultSurfaces.diffuseTexture
                    }
                |> Sg.viewTrafo (cameraView |> Mod.map CameraView.viewTrafo)
                |> Sg.projTrafo (frustum |> Mod.map Frustum.projTrafo)

        let task = app.Runtime.CompileRender(win.FramebufferSignature, sg)
        win.RenderTask <- task

        let getImgs (ss : string[]) =
            ss |> Array.filter ( fun s -> 
                let e = (System.IO.Path.GetExtension s).ToLower()
                e = ".jpg" || e = ".png" || e = ".jpeg" || e = ".gif" || e = ".tiff"
            )

        let saveCurrent() =
            
            let gogo = 
                {
                    pLambda =  currentLampta.Value
                    pSigma  =  currentSickma.Value
                    pProb   =  probability.Value  
                    aLambda =  aLambda.Value      
                    aSigma  =  aSigma.Value       
                    aThresh =  aThreshold.Value   
                }

            let ser = MBrace.FsPickler.FsPickler.CreateXmlSerializer(indent=true)

            ser.PickleToString gogo |> File.writeAllText (configfile())

        let help =
            "
                \n
                Commands: \n
                l 30.0   - prediction lambda
                s 0.5    - prediction sigma
                p 0.5    - prediction > this probability
                la 25.0  - affine lambda
                sa 0.5   - affine sigma
                ta 0.01  - (affine transform - observation) < this threshold
                u [0|1]  - use affine no/yes
                i [path] - folder with exactly 2 images
                f [orb|brisk|akaze] - feature selection
                \n
            "

        let runner =
            async {
                do! Async.SwitchToNewThread()
                while true do
                    Console.Write("bundler# ")
                    let line = Console.ReadLine()
                    match line = "" with
                    | true -> printfn "\n"
                    | false -> 
                        let rx = System.Text.RegularExpressions.Regex @"^[ \t]*(?<par>[a-zA-Z]+)[ \t=]+(?<value>[0-9]+(\.[0-9]+)?)$"
                        let m = rx.Match line
                        if m.Success then
                            let name = m.Groups.["par"].Value
                            let value = m.Groups.["value"].Value
                            let value = System.Double.Parse(value, System.Globalization.CultureInfo.InvariantCulture)
                            match name with
                            | "l" -> transact(fun () -> currentLampta.Value <- value)
                            | "s" -> transact(fun () -> currentSickma.Value <- value)
                            | "la" -> transact(fun () -> aLambda.Value <- value)
                            | "sa" -> transact(fun () -> aSigma.Value <- value)
                            | "p" -> transact(fun () -> probability.Value <- value)
                            | "ta" -> transact(fun () -> aThreshold.Value <- value)
                            | "u" -> transact(fun () -> useA.Value <- (value > 0.5))
                            | _ -> Log.warn "Invalid numeric input: %A" line

                        else
                            let rxText = System.Text.RegularExpressions.Regex @"^[ \t]*(?<par>[a-zA-Z]+)[ \t=]+(?<value>[0-9a-zA-Z:\\]+)$" 
                            let m = rxText.Match line
                            if m.Success then
                                let name = m.Groups.["par"].Value
                                let value = m.Groups.["value"].Value
                                match name with
                                | "i" -> 
                                    match System.IO.Directory.Exists value with
                                    | false -> Log.warn "Directory does not exist: %A" value
                                    | true ->
                                        let imgs = System.IO.Directory.GetFiles value |> getImgs
                                        match imgs.Length >= 2 with
                                        | false -> Log.warn "Only found %A images (need exactly 2) in directory %A" imgs.Length value
                                        | true -> transact( fun _ -> currentImgs.Value <- (imgs.[0] |> PixImage<byte>, imgs.[1] |> PixImage<byte>) )
                                | "f" ->
                                    match value.ToLower() with
                                    | "orb" -> transact( fun _ -> featureType.Value <- Orb )
                                    | "brisk" -> transact( fun _ -> featureType.Value <- Brisk )
                                    | "akaze" -> transact( fun _ -> featureType.Value <- Akaze )
                                    | _ -> Log.warn "%A is not a valid feature type. Use one of these: brisk, orb, akaze" value
                                | "h" | "help" -> printfn "%A" help
                                | _ -> Log.warn "Invalid string input: %A" line
                            else
                                let rxCommand = System.Text.RegularExpressions.Regex @"^[ \t]*(?<par>[a-zA-Z]+)$" 
                                let m = rxCommand.Match line
                                if m.Success then
                                    let name = m.Groups.["par"].Value
                                    match name with
                                    | "h" | "help" -> printfn "%A" help
                                    | "save" -> printfn @"Saving current config in c:\blub"; saveCurrent()
                                    | _ -> Log.warn "Get help with \"help\". Invalid command: %A" line
                                else
                                    Log.warn "Regex didn't parse: %A" line
            }

        use cancel = new System.Threading.CancellationTokenSource()
        Async.Start(runner,cancel.Token)

        win.Run()

        cancel.Cancel()
        
type Bam =
    | Oida
    | Fix of int

module SceneGraph =
    open Aardvark.SceneGraph
    open FShade
    
    let blurb = RenderPass.after "blurb" RenderPassOrder.Arbitrary RenderPass.main
    let blerb = RenderPass.after "asd" RenderPassOrder.Arbitrary blurb

    let ofBundlerSolution (cameraColor : C4b) (pointSize : int) (pointColor : C4b) (s : BundlerSolution) (surfaceCams : IMod<ISurface>) (surfacePoints : IMod<ISurface>) (surfaceBox : IMod<ISurface>) (pix : PixImage<byte>[]) (b : IMod<Bam>) =
        let frustum = Box3d(-V3d(1.0, 1.0, 100.0), V3d(1.0, 1.0, -1.01))
        let cameras = 
            s.cameras |> Map.toList |> List.mapi (fun i (_,c) -> 
                let quad i = 
                    Sg.fullScreenQuad 
                        |> Sg.diffuseTexture' (PixTexture2d(PixImageMipMap [|pix.[i] :> PixImage|], true))
                        |> Sg.surface surfaceCams
                        |> Sg.blendMode (Mod.constant BlendMode.Blend)
                        |> Sg.pass blerb

                let wb =
                    Sg.wireBox' C4b.Green frustum
                        |> Sg.surface surfaceBox 
                
                let quadVisible i = 
                    b |> Mod.map (function Bam.Oida -> true | Fix ci -> i = ci)
                
                [wb;    (if pix.Length > 0 then
                            quad i |> Sg.onOff (quadVisible i)
                          else Sg.ofList []
                        ) 
                ]   |> Sg.ofList 
                    |> Sg.transform (c.cam.ViewProjTrafo(100.0).Inverse)

            )
            |> Sg.ofList

        let points =
            IndexedGeometry(
                Mode = IndexedGeometryMode.PointList,
                IndexedAttributes =
                    SymDict.ofList [
                        DefaultSemantic.Positions, s.points |> Map.toSeq |> Seq.map snd |> Seq.map ( fun sp -> V3f sp.point) |> Seq.toArray :> Array
                    ]
            )
            |> Sg.ofIndexedGeometry
            |> Sg.uniform "PointSize" (Mod.constant (float pointSize))
            |> Sg.surface surfacePoints

        Sg.ofList [ points; cameras ]
            |> Sg.pass blurb
            |> Sg.blendMode (Mod.constant BlendMode.Blend)


module BundlerViewer =

    open Aardvark.Ceres.Bundle
    
    let intersect (p : Ray3d) (f : DepthFun) : Option<V3d> =
        [
            for t in f.Triangle3Ds do
                let mutable intert = 0.0
                let mutable running = true
                if running then
                    if t.Intersects(p,&intert) then
                        yield p.GetPointOnRay intert
                        running <- false
        ] |> Seq.tryHead

    let approximate (p : V2d) (f : DepthFun) : Option<V2d*float> =
        Log.line "No triangle intersection, trying to get closest"
        let pD (e : Line2d) d0 d1 = 
            let mutable t = 0.0
            let p2d = e.GetClosestPointOn(p, &t)
            let d = (p - p2d).Length
            p2d, (d0 * t + d1 * (1.0-t)), d
        
        let lines = 
            [
                for (p0,p1,p2,d0,d1,d2) in f.Point2Ds do
                    let e0 = Line2d(p0,p1)
                    let e1 = Line2d(p1,p2)
                    let e2 = Line2d(p2,p0)
                    yield pD e0 d0 d1
                    yield pD e1 d1 d2
                    yield pD e2 d2 d0
            ]   |> List.sortBy ( fun (_,_,d) -> d )
                |> List.toArray

        lines
            |> Array.toList
            |> List.map ( fun (_,d,_) -> p,d )
            |> List.tryHead

    let transferPoint (fromCam : Camera3d * DepthFun) (toCam : Camera3d * DepthFun) (point : V2d) : Option<V3d*(V2d*float)> =
        let (fcam, ftris) = fromCam
        let (tcam, _) = toCam
        let pos = fcam.Unproject point (-1.0 - 0.0001)

        let ray = Ray3d(fcam.Position,(pos - fcam.Position).Normalized)
        let intersection = intersect ray ftris

        let p = 
            match intersection with
            | None -> 
                Log.line "No triangle intersection, trying for approximation"
                let pos = approximate point ftris
                match pos with
                | None ->
                    Log.warn "This has no intersection."
                    None
                | Some (pd,depth) ->
                    let u = fcam.Unproject pd depth
                    
                    (u, u |> tcam.ProjectWithDepth) |> Some

            | Some ii -> 
                let p = ii |> tcam.ProjectWithDepth 
                Some (ii, p)

        p

    let folder path =

        let scaleTrafo = Trafo3d.Scale (V3d.III * 5.0)
        
        use app = new OpenGlApplication()
        use win = app.CreateSimpleRenderWindow(8)
        let surface = Shader.solutionSurface()
        let surfaceBox = Shader.surfaceBox C4b.Red
        let surfacePoints = Shader.surfacePoint C4b.Green

        let (solution, tris, ssg, imgs, features) = Bundle.filesIn path
        
        let b = Bam.Oida |> Mod.init
        let blurg = Bam.Oida |> Mod.init
        let totalblurg = true |> Mod.init

        let klickPoints : cset<int * V2d> = CSet.empty

        let tttt (ndc : V2d) = 
            let q = Match.qq
            let mm = Match2d(ndc, V2d.OO, V4d.OOOO, 0, 1)
            let (qe,_,_) = q |> Option.get
            let pred = qe mm
            ndc + pred
            
        //let yolo = 
        //    let (ps,ds) =
        //        let filtered = 
        //            edg.weight 
        //                //|> Array.filteri ( fun i _ -> i < 10 )

        //        let mapped = 
        //            filtered
        //                |> Array.map ( fun (i0,i1) -> 
        //                    features.[edg.i0].[i0].ndc, 
        //                    features.[edg.i1].[i1].ndc) 

        //        let dec = 2
        //        let distincted =
        //            mapped
        //                |> Array.distinctBy ( fun (p,d) ->
        //                    V2d(Math.Round(p.X,dec),Math.Round(p.Y,dec)))
        //                |> Array.distinctBy ( fun (p,d) ->
        //                    Math.Round(d.X,dec))
        //                |> Array.distinctBy ( fun (p,d) ->
        //                    Math.Round(d.Y,dec))

        //        distincted
        //            |> Array.unzip
                    
        //    MatchProblem.prediction ds ps 0.5

        let klickPointsSg = 
            klickPoints 
            |> ASet.map ( fun (ci, ndc) ->
                    match solution with
                    | Some solution ->
                        let cam = solution.cameras.[ci].cam
                        let pos = cam.Unproject ndc (-1.0 - 0.0001)
    //                    Log.line "Unprojected to: %A" pos
    //                    let ray = Ray3d(cam.Position,(pos - cam.Position).Normalized)
    //                    
    //                    let intersection = intersect ray f

                        let f = tris.[ci]
                    
                        let otherpoints = 
                            [| 
                                let ocs = [ 0 .. solution.cameras.Count - 1 ] |> List.except [ci]
                                for other in ocs do
                                    let oc = solution.cameras.[other].cam
                                    let tro = tris.[other]
                                    yield other,transferPoint (cam,f) (oc,tro) ndc
                            |]

                        let ps = 
                            [|
                                yield pos
                                 
                                //yield oc.Unproject tttt (-oc.FocalLength - 0.0001)

                                for (oci,othercam) in otherpoints do
                                    match othercam with
                                    | None -> ()
                                    | Some (p3d, (ii,_)) -> 
                                        yield p3d

                                        let oc = solution.cameras.[oci].cam

                                        let oo = oc.Unproject ii (-1.0 - 0.0001)
                                        Log.line "Unprojected in other cam: %A" oo 
                                        yield oo
                                
                            |]

                        IndexedGeometryPrimitives.points ps [|C4b.DarkYellow; C4b.Yellow; C4b(250,200,130,255)|]
                        |> Sg.ofIndexedGeometry
                    | None ->
                        Log.error "No solution convergence."
                        Sg.ofList []
                )   |> Sg.set
                    |> Sg.uniform "PointSize" (Mod.constant (float 10))
                    |> Sg.shader { 
                        do! DefaultSurfaces.trafo
                        do! DefaultSurfaces.vertexColor
                        do! DefaultSurfaces.pointSprite
                        do! DefaultSurfaces.pointSpriteFragment
                    }


        let allPointsSg =
            let rand = RandomSystem()
            let (pos,col) = 
                [|
                    match solution with
                    | Some solution ->
                        let cam = solution.cameras.[0].cam
                        let oc =  solution.cameras.[1].cam
                        let t =  tris.[0]
                        let ot = tris.[1]
                   
                        //for (f,_) in edg.weight |> Array.map ( fun (i0,i1) -> features.[edg.i0].[i0], features.[edg.i1].[i1]) do
                        
                            //let ndc = f.ndc
                            //let ondc = tttt ndc
                            //let (_, ( ondc, _ )) = transferPoint (cam,t) (oc,ot) ndc |> Option.get
                    
                        //let s = 40.0
                        //for i in -s .. s do
                        //    for j in -s .. s do
                        //        let ndc = V2d(i / s, j / s)

                        for f in features.[0] do
                            ()
                                    //let ndc = f.ndc
                                    //let ondc = yolo ndc

                                    //let col = rand.UniformC3f().ToC4b()
                        
                                    //yield cam.Unproject ndc (-cam.FocalLength - 0.0001), col
                                    //yield oc.Unproject ondc (-oc.FocalLength - 0.0001), col
                    | None -> ()
                |] |> Array.unzip
            
            IndexedGeometryPrimitives.points pos col
                |> Sg.ofIndexedGeometry
                |> Sg.uniform "PointSize" (Mod.constant (float 6))
                |> Sg.shader { 
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.vertexColor
                    do! DefaultSurfaces.pointSprite
                    do! DefaultSurfaces.pointSpriteFragment
                }

        let sg : ISg = 

            let blurgs =
                ssg   |> List.map ( fun (s,ci) -> s |> Sg.onOff ( blurg |> Mod.map (function Bam.Oida -> true | Fix ind -> ind = ci) ) )
                      |> Sg.ofList
                      |> Sg.shader {
                        do! DefaultSurfaces.trafo
                        //do! DefaultSurfaces.diffuseTexture
                        do! DefaultSurfaces.vertexColor
                        do! DefaultSurfaces.simpleLighting
                      }
                      |> Sg.pass RenderPass.main
                      |> Sg.blendMode (Mod.constant BlendMode.Blend)
                      |> Sg.onOff totalblurg
            

            let stuff = 
                [
                    match solution with
                    | Some solution ->
                        yield SceneGraph.ofBundlerSolution C4b.Red 10 C4b.Green solution surface surfacePoints surfaceBox imgs b
                    | None -> Log.error "No solution."
                    yield blurgs
                    yield allPointsSg
                    yield klickPointsSg
                ] |> Sg.ofList

            let frustum = win.Sizes |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 10000.0 (float s.X / float s.Y))
            let cameraView = CameraView.lookAt (V3d(0.0, 2.0, 0.0)) V3d.Zero -V3d.OOI

            let lastSpace = Mod.init DateTime.Now
            lastSpace |> Mod.unsafeRegisterCallbackKeepDisposable ( fun _ -> printfn "Recentering camera." ) |> ignore

            let cameraView = 
                let im = Mod.custom ( fun a ->
                    
                    lastSpace.GetValue a |> ignore
                    cameraView |> DefaultCameraController.controlWithSpeed (Mod.init 2.5) win.Mouse win.Keyboard win.Time
                )

                let far = 1000.0
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
                        match solution with
                        | None -> return Trafo3d.Identity, Trafo3d.Identity
                        | Some solution -> 
                            let fs = solution.cameras.[i].cam.ViewProjTrafo far
                            return Trafo3d.Identity, fs
                }

            stuff |> Sg.viewTrafo (cameraView |> Mod.map fst )
                  |> Sg.projTrafo (cameraView |> Mod.map snd )

        win.Keyboard.KeyDown(Keys.D1).Values.Add ( fun _ -> if not <| (win.Keyboard.IsDown Keys.LeftCtrl).GetValue() then transact ( fun _ -> b.Value <- Fix 0))
        win.Keyboard.KeyDown(Keys.D2).Values.Add ( fun _ -> if not <| (win.Keyboard.IsDown Keys.LeftCtrl).GetValue() then transact ( fun _ -> b.Value <- Fix 1))
        win.Keyboard.KeyDown(Keys.D3).Values.Add ( fun _ -> if not <| (win.Keyboard.IsDown Keys.LeftCtrl).GetValue() then transact ( fun _ -> b.Value <- Fix 2))
        win.Keyboard.KeyDown(Keys.Space).Values.Add ( fun _ -> transact ( fun _ -> b.Value <- Oida))
        win.Keyboard.KeyDown(Keys.F).Values.Add 
            ( fun _ -> 
                match b.Value with
                | Bam.Oida -> ()
                | Fix i ->
                    //don't need to flip the y position since it's already wrong in the image
                    //flip x instead yolo
                    let ndc = (((win.Mouse.Position |> Mod.force).NormalizedPosition * V2d(2.0, 2.0)) - V2d.II) * V2d(-1.0, 1.0)
                    transact ( fun _ -> klickPoints |> CSet.add (i,ndc) |> ignore)
            )
        win.Keyboard.KeyDown(Keys.C).Values.Add ( fun _ -> transact ( fun _ -> CSet.clear klickPoints ))
        win.Keyboard.KeyDown(Keys.T).Values.Add ( fun _ -> transact ( fun _ -> totalblurg.Value <- not totalblurg.Value ))
        win.Keyboard.KeyDown(Keys.NumPad1).Values.Add( fun _ -> transact ( fun _ -> blurg.Value <- Fix 0) )
        win.Keyboard.KeyDown(Keys.NumPad2).Values.Add( fun _ -> transact ( fun _ -> blurg.Value <- Fix 1) )
        win.Keyboard.KeyDown(Keys.NumPad3).Values.Add( fun _ -> transact ( fun _ -> blurg.Value <- Fix 2) )
        win.Keyboard.KeyDown(Keys.NumPad0).Values.Add( fun _ -> transact ( fun _ -> blurg.Value <- Bam.Oida) )

        let task = app.Runtime.CompileRender(win.FramebufferSignature, sg)
        win.RenderTask <- task

        win.Run()

        printfn "Done."

    open System.IO
    let sponzaWithoutRender sponzaPath =
        
        use app = new OpenGlApplication()
        use win = app.CreateSimpleRenderWindow(8)

        let ser = MBrace.FsPickler.FsPickler.CreateBinarySerializer()

        let graphInput = sponzaPath 
                            |> File.readAllBytes 
                            |> ser.UnPickle
                            |> ModelSer.toGraphInput
        
        let graph = Feature.FeatureGraph.build graphInput

        let input = Feature.FeatureGraph.toBundlerInputSiegfried graph 2

        let problem = input |> BundlerInput.toProblem
        
        printfn "Done."

        Bundler.solve ignore problem
        
    open System.IO
    let sponza sponzaPath =
        
        use app = new OpenGlApplication()
        use win = app.CreateSimpleRenderWindow(8)

        let ser = MBrace.FsPickler.FsPickler.CreateBinarySerializer()

        let graphInput = sponzaPath 
                            |> File.readAllBytes 
                            |> ser.UnPickle
                            |> ModelSer.toGraphInput
        
        let graph = Feature.FeatureGraph.build graphInput

        let input = Feature.FeatureGraph.toBundlerInputSiegfried graph 2

        let problem = input |> BundlerInput.toProblem

        let solution = Mod.init None

        let renderSolution = Mod.init None

        let updateVis () = transact ( fun _ ->  renderSolution.Value <- solution.Value)
        
        let mutable visRunning = true
        async {
            do! Async.SwitchToNewThread()
            while visRunning do
                do! Async.Sleep 250
                updateVis()
        } |> Async.Start

        //does not work so good
        let resizeToUnit =
            let getBoxScale (fromBox : Box3d) (toBox : Box3d) : float =
                let fromSize = fromBox.Size
                let toSize = toBox.Size
                let factor = toSize / fromSize

                let mutable smallest = factor.X

                if factor.Y < smallest then
                    smallest <- factor.Y
                if factor.Z < smallest then
                    smallest <- factor.Z

                smallest
            let transformBox (sbox : Box3d) (tbox : Box3d) = Trafo3d.Translation(-sbox.Center) * Trafo3d.Scale(getBoxScale sbox tbox) * Trafo3d.Translation(tbox.Center)
            
            let sol = renderSolution |> Mod.force
            match sol with
            | None ->  Trafo3d.Identity |> Mod.constant
            | Some solution ->
                let pts = solution.points |> Map.toArray |> Array.map snd |> Array.map ( fun sp -> sp.point )
                let bb = Box3d(pts)
                let trafo = transformBox bb (Box3d(V3d(-1.0,-1.0,-1.0),V3d(1.0,1.0,1.0)))
                trafo |> Mod.constant

        async {
            let cacheSolution = false
            let mutable iterCt = -1
            let adorner sol =
                if iterCt = 0 then
                    printf "Enter iterCt: "
                    match Console.ReadLine() |> Int32.TryParse with
                    | (true,v) -> iterCt <- -v
                    | _ -> iterCt <- -10
                transact ( fun _ -> Mod.change solution (sol |> Some) )
                iterCt <- iterCt + 1

            let finalSol = 
                if not cacheSolution then 
                    Log.line "No caching involved."
                    Bundler.solve adorner problem
                else
                    let fn = @"D:\file\sponza_bun\cachedSol"
                    let ser = MBrace.FsPickler.FsPickler.CreateBinarySerializer()
                    if File.Exists fn then
                        Log.warn "Reading cached solution"
                        fn |> File.readAllBytes |> ser.UnPickle
                    else
                        let sol = Bundler.solve adorner problem
                        Log.warn "Writing solution to cache"
                        sol |> ser.Pickle |> File.writeAllBytes fn
                        sol
            match finalSol with
            | None -> ()
            | Some finalSol ->
                adorner finalSol

        } |> Async.Start

        let blurg = Mod.init Bam.Oida
        let blub = Mod.init Bam.Oida
        
        win.Keyboard.KeyDown(Keys.D1).Values.Add ( fun _ -> transact ( fun _ ->     blurg.Value <- Fix 0))
        win.Keyboard.KeyDown(Keys.D2).Values.Add ( fun _ -> transact ( fun _ ->     blurg.Value <- Fix 1))
        win.Keyboard.KeyDown(Keys.D3).Values.Add ( fun _ -> transact ( fun _ ->     blurg.Value <- Fix 2))
        win.Keyboard.KeyDown(Keys.D4).Values.Add ( fun _ -> transact ( fun _ ->     blurg.Value <- Fix 3))
        win.Keyboard.KeyDown(Keys.D5).Values.Add ( fun _ -> transact ( fun _ ->     blurg.Value <- Fix 4))
        win.Keyboard.KeyDown(Keys.D6).Values.Add ( fun _ -> transact ( fun _ ->     blurg.Value <- Fix 5))
        win.Keyboard.KeyDown(Keys.D7).Values.Add ( fun _ -> transact ( fun _ ->     blurg.Value <- Fix 6))
        win.Keyboard.KeyDown(Keys.D8).Values.Add ( fun _ -> transact ( fun _ ->     blurg.Value <- Fix 7))
        win.Keyboard.KeyDown(Keys.D9).Values.Add ( fun _ -> transact ( fun _ ->     blurg.Value <- Fix 8))
        win.Keyboard.KeyDown(Keys.D0).Values.Add ( fun _ -> transact ( fun _ ->     blurg.Value <- Fix 9))
        win.Keyboard.KeyDown(Keys.Space).Values.Add ( fun _ -> transact ( fun _ ->  blurg.Value <- Bam.Oida))

        win.Keyboard.KeyDown(Keys.NumPad1).Values.Add ( fun _ -> transact ( fun _ ->     blub.Value <- Fix 0))
        win.Keyboard.KeyDown(Keys.NumPad2).Values.Add ( fun _ -> transact ( fun _ ->     blub.Value <- Fix 1))
        win.Keyboard.KeyDown(Keys.NumPad3).Values.Add ( fun _ -> transact ( fun _ ->     blub.Value <- Fix 2))
        win.Keyboard.KeyDown(Keys.NumPad4).Values.Add ( fun _ -> transact ( fun _ ->     blub.Value <- Fix 3))
        win.Keyboard.KeyDown(Keys.NumPad5).Values.Add ( fun _ -> transact ( fun _ ->     blub.Value <- Fix 4))
        win.Keyboard.KeyDown(Keys.NumPad6).Values.Add ( fun _ -> transact ( fun _ ->     blub.Value <- Fix 5))
        win.Keyboard.KeyDown(Keys.NumPad7).Values.Add ( fun _ -> transact ( fun _ ->     blub.Value <- Fix 6))
        win.Keyboard.KeyDown(Keys.NumPad8).Values.Add ( fun _ -> transact ( fun _ ->     blub.Value <- Fix 7))
        win.Keyboard.KeyDown(Keys.NumPad9).Values.Add ( fun _ -> transact ( fun _ ->     blub.Value <- Fix 8))
        win.Keyboard.KeyDown(Keys.NumPad0).Values.Add ( fun _ -> transact ( fun _ ->     blub.Value <- Fix 9))
        win.Keyboard.KeyDown(Keys.C).Values.Add ( fun _ -> transact ( fun _ ->           blub.Value <- Bam.Oida))
        win.Keyboard.KeyDown(Keys.O).Values.Add updateVis 
        
        let proj = win.Sizes    |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 10000.0 (float s.X / float s.Y))
                                |> Mod.map Frustum.projTrafo

        let asdf = Mod.init (V3d(0.0, 50.0, 0.0))
        let view = 
            adaptive {
                let! asdf = asdf
                let! c =  CameraView.lookAt asdf V3d.Zero -V3d.OOI
                            |> DefaultCameraController.controlWithSpeed (Mod.init 2.5) win.Mouse win.Keyboard win.Time
                            |> Mod.map CameraView.viewTrafo
                return c
            }
    
        let far = 1000.0
        let camview solution i = 
            let cam = solution.cameras.[i].cam
            let ocam = cam.Transformed( resizeToUnit |> Mod.force )
            transact( fun _ -> Mod.change asdf ocam.Position )
            ocam.ViewProjTrafo far

        let rays solution i =
            let ocam = solution.cameras.[i].cam.Transformed( resizeToUnit |> Mod.force )
            [|
                for ti in 0 .. solution.problem.input.tracks.Length-1 do
                    let track = solution.problem.input.tracks.[ti]
                    match track |> Array.exists (fun (ci,_) -> ci = i) with
                    | true -> yield 
                                Line3d(solution.points.[ti].point, ocam.Position)
                    | false -> ()
            |]  |> Mod.constant
                |> Sg.lines (Mod.constant C4b.Yellow)
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.vertexColor
                   }
            

        let vp =
            adaptive {
                let! b = blurg
                match b with
                | Oida ->
                    let! cv = view
                    let! f = proj
                    return cv, f
                | Fix i ->
                    let! s = solution
                    match s with
                    | None -> return Trafo3d.Identity, Trafo3d.Identity
                    | Some solution ->
                        let fs = 
                            try 
                                camview solution i
                            with _ -> 
                                camview solution 0
                        return Trafo3d.Identity, fs
            }

        let (view,proj) = vp |> Mod.map fst, vp |> Mod.map snd

        let surface = Shader.surfaceApp win
        let surfacePoints = Shader.surfacePointApp C4b.Green win
        let surfaceBox = Shader.surfaceBoxApp C4b.Red win

        let sg2 =   
            adaptive {
                let! s = renderSolution
                match s with
                | None -> return Sg.ofList []
                | Some solution ->
                    let! b = blub
                    match b with
                    | Bam.Oida -> 
                        return Sg.ofList []
                    | Fix i ->
                        let isg =
                            try 
                                rays solution i
                            with _ ->
                                Sg.ofList []

                        return isg
                                
            }   |> Sg.dynamic

        let sg = 
            adaptive {
                let! s = renderSolution
                match s with
                | None -> return Sg.ofList []
                | Some solution ->
                     return SceneGraph.ofBundlerSolution C4b.Red 10 C4b.Green solution surface surfacePoints surfaceBox graphInput.images (Mod.constant Bam.Oida)
                            |> Sg.trafo resizeToUnit
            }       |> Sg.dynamic

        let sg = Sg.ofList [sg; sg2]
                    |> Sg.viewTrafo view
                    |> Sg.projTrafo proj

        let task = app.Runtime.CompileRender(win.FramebufferSignature, sg)
        win.RenderTask <- task

        win.Run()
           
        visRunning <- false
        
        printfn "Done."


       
    open System.IO
    let filesSuperEvilHack jpgsPath =
        
        use app = new OpenGlApplication()
        use win = app.CreateSimpleRenderWindow(8)

        let ser = MBrace.FsPickler.FsPickler.CreateBinarySerializer()

        

        let cfg : stuffConfig = 
            if System.IO.File.Exists (configfile()) then
                Log.line @"Loading config from C:\blub"
                let pickler = MBrace.FsPickler.FsPickler.CreateXmlSerializer(indent=true)
                File.readAllText (configfile()) |> pickler.UnPickleOfString
            else
                {
                    pLambda = 20.0
                    pSigma  = 0.6
                    pProb   = 0.5
                    aLambda = 20.0
                    aSigma  = 0.5
                    aThresh = 0.01
                }

        let bruteforceThreshold = 0.9

        let probabilityLambda =         cfg.pLambda
        let probabilitySigma =          cfg.pSigma
        let probabilityLowerThresh =    cfg.pProb

        let affineLambda =      cfg.aLambda
        let affineSigma =       cfg.aSigma
        let affineUpperThresh = cfg.aThresh

        let bruteforceMinCount = 10
        let probableMinCount =   10
        let affineMinCount =     10

        let bundlerMinTrackLength = 2
        let bundlerMaxFtrsPerCam = 60

        let pairMatch l r =
            
            Log.startTimed "Bruteforce matching"
            let bf = Match.bruteforce bruteforceThreshold l r |> Match2d.ofFeatures l r 
            Log.stop()
            
            if bf.Length >= bruteforceMinCount then
                Log.startTimed "Probability matching"
                let probable = bf |> Match.probability probabilityLambda probabilitySigma probabilityLowerThresh
                Log.stop ()

                if probable.Length >= probableMinCount then
                    Log.startTimed "Consistency matching"
                    let affine = probable |> Match.affine affineLambda affineSigma affineUpperThresh
                    Log.stop()

                    if affine.Length >= affineMinCount then

                        printfn "Found %A affine-consistent matches." affine.Length

                        affine |> Array.map ( fun m -> m.Left, m.Right )
                    else
                        printfn "Not enough affine matches (less than %A, only %A)." affineMinCount affine.Length
                        [||]
                else
                    printfn "Not enough probable matches (less than %A, only %A)." probableMinCount probable.Length
                    [||]
            else
                printfn "Not enough bruteforce matches (less than %A, only %A)." bruteforceMinCount bf.Length
                [||]

        let matcher (d : Feature[][]) li ri = pairMatch d.[li] d.[ri]

        let filenames = System.IO.Directory.GetFiles jpgsPath
        let images = 
            filenames
                |> Array.map PixImage.Create
                |> Array.map (fun pi -> pi.AsPixImage<byte>())

        let data = images |> Array.map Akaze.ofImage

        let graphInput = 
            {
                matcher = matcher
                images = images
                data = data
            }
        
        Log.line "did copy/paste stuff, now the real fun begins"

        //let graphInput = sponzaPath 
        //                    |> File.readAllBytes 
        //                    |> ser.UnPickle
        //                    |> ModelSer.toGraphInput
        
        let graph = Feature.FeatureGraph.build graphInput

        let input = Feature.FeatureGraph.toBundlerInputSiegfried graph 2

        let problem = input |> BundlerInput.toProblem

        let solution = Mod.init None

        let renderSolution = Mod.init None

        let updateVis () = transact ( fun _ ->  renderSolution.Value <- solution.Value)
        
        let mutable visRunning = true
        async {
            do! Async.SwitchToNewThread()
            while visRunning do
                do! Async.Sleep 250
                updateVis()
        } |> Async.Start

        //does not work so good
        let resizeToUnit =
            let getBoxScale (fromBox : Box3d) (toBox : Box3d) : float =
                let fromSize = fromBox.Size
                let toSize = toBox.Size
                let factor = toSize / fromSize

                let mutable smallest = factor.X

                if factor.Y < smallest then
                    smallest <- factor.Y
                if factor.Z < smallest then
                    smallest <- factor.Z

                smallest
            let transformBox (sbox : Box3d) (tbox : Box3d) = Trafo3d.Translation(-sbox.Center) * Trafo3d.Scale(getBoxScale sbox tbox) * Trafo3d.Translation(tbox.Center)
            
            let sol = renderSolution |> Mod.force
            match sol with
            | None ->  Trafo3d.Identity |> Mod.constant
            | Some solution ->
                let pts = solution.points |> Map.toArray |> Array.map snd |> Array.map ( fun sp -> sp.point )
                let bb = Box3d(pts)
                let trafo = transformBox bb (Box3d(V3d(-1.0,-1.0,-1.0),V3d(1.0,1.0,1.0)))
                trafo |> Mod.constant

        async {
            let cacheSolution = false
            let mutable iterCt = -1
            let adorner sol =
                if iterCt = 0 then
                    printf "Enter iterCt: "
                    match Console.ReadLine() |> Int32.TryParse with
                    | (true,v) -> iterCt <- -v
                    | _ -> iterCt <- -10
                transact ( fun _ -> Mod.change solution (sol |> Some) )
                iterCt <- iterCt + 1

            let finalSol = 
                if not cacheSolution then 
                    Log.line "No caching involved."
                    Bundler.solve adorner problem
                else
                    let fn = @"D:\file\sponza_bun\cachedSol"
                    let ser = MBrace.FsPickler.FsPickler.CreateBinarySerializer()
                    if File.Exists fn then
                        Log.warn "Reading cached solution"
                        fn |> File.readAllBytes |> ser.UnPickle
                    else
                        let sol = Bundler.solve adorner problem
                        Log.warn "Writing solution to cache"
                        sol |> ser.Pickle |> File.writeAllBytes fn
                        sol
            match finalSol with
            | None -> ()
            | Some finalSol ->
                adorner finalSol

        } |> Async.Start

        let blurg = Mod.init Bam.Oida
        let blub = Mod.init Bam.Oida
        
        win.Keyboard.KeyDown(Keys.D1).Values.Add ( fun _ -> transact ( fun _ ->     blurg.Value <- Fix 0))
        win.Keyboard.KeyDown(Keys.D2).Values.Add ( fun _ -> transact ( fun _ ->     blurg.Value <- Fix 1))
        win.Keyboard.KeyDown(Keys.D3).Values.Add ( fun _ -> transact ( fun _ ->     blurg.Value <- Fix 2))
        win.Keyboard.KeyDown(Keys.D4).Values.Add ( fun _ -> transact ( fun _ ->     blurg.Value <- Fix 3))
        win.Keyboard.KeyDown(Keys.D5).Values.Add ( fun _ -> transact ( fun _ ->     blurg.Value <- Fix 4))
        win.Keyboard.KeyDown(Keys.D6).Values.Add ( fun _ -> transact ( fun _ ->     blurg.Value <- Fix 5))
        win.Keyboard.KeyDown(Keys.D7).Values.Add ( fun _ -> transact ( fun _ ->     blurg.Value <- Fix 6))
        win.Keyboard.KeyDown(Keys.D8).Values.Add ( fun _ -> transact ( fun _ ->     blurg.Value <- Fix 7))
        win.Keyboard.KeyDown(Keys.D9).Values.Add ( fun _ -> transact ( fun _ ->     blurg.Value <- Fix 8))
        win.Keyboard.KeyDown(Keys.D0).Values.Add ( fun _ -> transact ( fun _ ->     blurg.Value <- Fix 9))
        win.Keyboard.KeyDown(Keys.Space).Values.Add ( fun _ -> transact ( fun _ ->  blurg.Value <- Bam.Oida))

        win.Keyboard.KeyDown(Keys.NumPad1).Values.Add ( fun _ -> transact ( fun _ ->     blub.Value <- Fix 0))
        win.Keyboard.KeyDown(Keys.NumPad2).Values.Add ( fun _ -> transact ( fun _ ->     blub.Value <- Fix 1))
        win.Keyboard.KeyDown(Keys.NumPad3).Values.Add ( fun _ -> transact ( fun _ ->     blub.Value <- Fix 2))
        win.Keyboard.KeyDown(Keys.NumPad4).Values.Add ( fun _ -> transact ( fun _ ->     blub.Value <- Fix 3))
        win.Keyboard.KeyDown(Keys.NumPad5).Values.Add ( fun _ -> transact ( fun _ ->     blub.Value <- Fix 4))
        win.Keyboard.KeyDown(Keys.NumPad6).Values.Add ( fun _ -> transact ( fun _ ->     blub.Value <- Fix 5))
        win.Keyboard.KeyDown(Keys.NumPad7).Values.Add ( fun _ -> transact ( fun _ ->     blub.Value <- Fix 6))
        win.Keyboard.KeyDown(Keys.NumPad8).Values.Add ( fun _ -> transact ( fun _ ->     blub.Value <- Fix 7))
        win.Keyboard.KeyDown(Keys.NumPad9).Values.Add ( fun _ -> transact ( fun _ ->     blub.Value <- Fix 8))
        win.Keyboard.KeyDown(Keys.NumPad0).Values.Add ( fun _ -> transact ( fun _ ->     blub.Value <- Fix 9))
        win.Keyboard.KeyDown(Keys.C).Values.Add ( fun _ -> transact ( fun _ ->           blub.Value <- Bam.Oida))
        win.Keyboard.KeyDown(Keys.O).Values.Add updateVis 
        
        let proj = win.Sizes    |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 10000.0 (float s.X / float s.Y))
                                |> Mod.map Frustum.projTrafo

        let asdf = Mod.init (V3d(0.0, 50.0, 0.0))
        let view = 
            adaptive {
                let! asdf = asdf
                let! c =  CameraView.lookAt asdf V3d.Zero -V3d.OOI
                            |> DefaultCameraController.controlWithSpeed (Mod.init 2.5) win.Mouse win.Keyboard win.Time
                            |> Mod.map CameraView.viewTrafo
                return c
            }
    
        let far = 1000.0
        let camview solution i = 
            let cam = solution.cameras.[i].cam
            let ocam = cam.Transformed( resizeToUnit |> Mod.force )
            transact( fun _ -> Mod.change asdf ocam.Position )
            ocam.ViewProjTrafo far

        let rays solution i =
            let ocam = solution.cameras.[i].cam.Transformed( resizeToUnit |> Mod.force )
            [|
                for ti in 0 .. solution.problem.input.tracks.Length-1 do
                    let track = solution.problem.input.tracks.[ti]
                    match track |> Array.exists (fun (ci,_) -> ci = i) with
                    | true -> yield 
                                Line3d(solution.points.[ti].point, ocam.Position)
                    | false -> ()
            |]  |> Mod.constant
                |> Sg.lines (Mod.constant C4b.Yellow)
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.vertexColor
                   }
            

        let vp =
            adaptive {
                let! b = blurg
                match b with
                | Oida ->
                    let! cv = view
                    let! f = proj
                    return cv, f
                | Fix i ->
                    let! s = solution
                    match s with
                    | None -> return Trafo3d.Identity, Trafo3d.Identity
                    | Some solution ->
                        let fs = 
                            try 
                                camview solution i
                            with _ -> 
                                camview solution 0
                        return Trafo3d.Identity, fs
            }

        let (view,proj) = vp |> Mod.map fst, vp |> Mod.map snd

        let surface = Shader.surfaceApp win
        let surfacePoints = Shader.surfacePointApp C4b.Green win
        let surfaceBox = Shader.surfaceBoxApp C4b.Red win

        let sg2 =   
            adaptive {
                let! s = renderSolution
                match s with
                | None -> return Sg.ofList []
                | Some solution ->
                    let! b = blub
                    match b with
                    | Bam.Oida -> 
                        return Sg.ofList []
                    | Fix i ->
                        let isg =
                            try 
                                rays solution i
                            with _ ->
                                Sg.ofList []

                        return isg
                                
            }   |> Sg.dynamic

        let sg = 
            adaptive {
                let! s = renderSolution
                match s with
                | None -> return Sg.ofList []
                | Some solution ->
                     return SceneGraph.ofBundlerSolution C4b.Red 10 C4b.Green solution surface surfacePoints surfaceBox graphInput.images (Mod.constant Bam.Oida)
                            |> Sg.trafo resizeToUnit
            }       |> Sg.dynamic

        let sg = Sg.ofList [sg; sg2]
                    |> Sg.viewTrafo view
                    |> Sg.projTrafo proj

        let task = app.Runtime.CompileRender(win.FramebufferSignature, sg)
        win.RenderTask <- task

        win.Run()
           
        visRunning <- false
        
        printfn "Done."


module Example =

    type RenderSponzaConfig = 
        {
            PercentObservations : float
            JitterNDC : float
            PercentFalse : float
        }

    open Aardvark.SceneGraph.IO
    open System.Collections.Generic


    let renderSponza (cfg : RenderSponzaConfig) outPath cams (points : Option<int>) =

        //euclid/inout/backend-paper/sponza_obj_copy
        Loader.Assimp.initialize()
        let scene = Loader.Assimp.loadFrom @"D:\file\sponza\sponza_NoMaterials_cm.obj" Assimp.PostProcessSteps.None
        
        //let scale = Trafo3d.Scale (V3d.III * (1.0/100.0))

        
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
                |> (match points with Some count -> Array.take count | None -> id)
                |> Array.unzip
           
        let center = V3d.OOO
        let r = 3000.0

        let mutable i = 0.0
        let step = (2.0 * Math.PI) / float cams

        //use app = new OpenGlApplication()
        //use win = app.CreateSimpleRenderWindow(8)

        //let proj = win.Sizes    |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 10000.0 (float s.X / float s.Y))
        //                        |> Mod.map Frustum.projTrafo

        //let view = CameraView.lookAt (V3d(0.0, r*1.0, 0.0)) V3d.Zero V3d.OOI
        //                        |> DefaultCameraController.controlWithSpeed (Mod.init 1000.0) win.Mouse win.Keyboard win.Time
        //                        |> Mod.map CameraView.viewTrafo

        //let sg = 
        //    IndexedGeometry(
        //        Mode = IndexedGeometryMode.PointList,
        //        IndexedAttributes = 
        //            SymDict.ofList [
        //                DefaultSemantic.Positions, sponzaVertices :> Array
        //                DefaultSemantic.Normals, sponzaNormals :> Array
        //            ]
        //        )       
        //            |> Sg.ofIndexedGeometry
        //            |> Sg.shader {
        //                do! DefaultSurfaces.trafo
        //                do! DefaultSurfaces.pointSprite
        //                do! DefaultSurfaces.pointSpriteFragment
        //                do! DefaultSurfaces.constantColor C4f.White
        //                do! DefaultSurfaces.simpleLighting
        //            }
        //            |> Sg.uniform "PointSize" (Mod.constant 5.0)
        //            |> Sg.viewTrafo view
        //            |> Sg.projTrafo proj

        //let task = app.Runtime.CompileRender(win.FramebufferSignature, sg)
        //win.RenderTask <- task

        //win.Run()

        //System.Environment.Exit 0








        Log.startTimed "Big Array"
        let ms =
            [|
            for _ in 0..cams-1 do
                yield   [|
                        for _ in 0..cams-1 do
                            yield   HashSet.empty
                        |]
            |]
        Log.stop()

        let fs =
            [|
                for _ in 0..cams-1 do
                    yield List<int*Feature>()
            |]
        
        let rand = RandomSystem()

//        let mutable ct = 0
        Log.startTimed "Fill small array"
        for c in 0 .. cams-1 do
            let x = r * sin i
            let y = r * cos i
            let cam = Camera3d.LookAt(center + V3d(x,y,0.0), center, 1.0, V3d.OOI)
            
            for pi in 0..sponzaVertices.Length-1 do

                if rand.UniformDouble() < cfg.PercentObservations then
                    let v = sponzaVertices.[pi]

                    let projected = cam.Project v
                    
                    let projected = projected + (rand.UniformV2dDirection() * (cfg.JitterNDC))

                    let f = {   
                                ndc         = projected
                                angle       = 0.0
                                size        = 1.0
                                response    = 1.0
                                descriptor  = FeatureDescriptor([||])
                            }
                    
                    if f.ndc.X < 1.0 && f.ndc.X > -1.0 && f.ndc.Y < 1.0 && f.ndc.Y > -1.0 then
                        
                        fs.[c].Add (pi,f)
                
            i <- i + step

        Log.stop ()

        Log.startTimed "Fill big array"

        for lc in 0 .. cams-1 do
            Log.startTimed "Cam %A of %A" lc (cams-1)
            for rc in 0 .. cams-1 do
                let lfs = fs.[lc] |> Seq.toArray
                let rfs = fs.[rc] |> Seq.toArray
                for lp in 0 .. lfs.Length-1 do
                    for rp in 0 .. rfs.Length-1 do
                        let (lfli,_) = lfs.[lp]
                        let (lfri,_) = rfs.[rp]
                        if lfli = lfri then
                            ms.[lc].[rc] |> HashSet.add (lp,rp) |> ignore
            Log.stop()

        Log.stop()

        let fs = fs |> Array.map ( fun gl -> gl.ToArray() |> Array.map snd )
        let s = { ms = ms; fs = fs }

        let ser = MBrace.FsPickler.FsPickler.CreateBinarySerializer()
        

        s |> ser.Pickle |> File.writeAllBytes outPath
        
    

    //let testManySponzas () =
        
    //    let sols =
    //        [
    //            let mutable ct = 0
    //            let range = [0.1 .. 0.1 .. 1.0]
    //            for percentObservations in range do
    //                ct <- ct+1
    //                Log.warn "Starting %A of %A" ct (range |> Seq.length)
    //                renderSponza percentObservations @"D:\file\sponza_bun\sponzaVertices" 5 (Some 50)
    //                yield ct, percentObservations, BundlerViewer.sponzaWithoutRender @"D:\file\sponza_bun\sponzaVertices"
    //        ]
        
    //    let ostr = 
    //        [
    //            yield sprintf "Test run: %A" (DateTime.Now.ToString(" HH:mm (YYYY-MM-dd) "))
    //            yield ""
    //            for (i,o,s) in sols do
    //                match s with
    //                | None -> 
    //                    yield sprintf "nr:%A\t\tpo:%A\t\NO CONVERGENCE!!" i o
    //                | Some s -> 
    //                    yield sprintf "nr:%A\t\tpo:%A\t\terr=%A" i o s.cost
    //            yield ""
    //        ] |> String.concat " \n"

    //    ostr |> File.writeAllText @"D:\file\out.txt"
        
    //    Log.line "%A" ostr