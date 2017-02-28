namespace Aardvark.Ceres

open System

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.Application

open Aardvark.Rendering.Text
open Aardvark.Application.WinForms

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

        let currentLampta = Mod.init 35.0
        let currentSickma = Mod.init 0.5
        let probability = Mod.init 0.5
        let aLambda = Mod.init 20.0
        let aSigma = Mod.init 0.5
        let aThreshold = Mod.init 0.01
        let useA = Mod.init true

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
                    Feature.matchCandidates config.threshold lFtr rFtr

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
                                let (qn,ex,ey) = MatchProblem.affineDistance affineLambda affineSigma m2g

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
                                    | _ -> Log.warn "Get help with \"help\". Invalid command: %A" line
                                else
                                    Log.warn "Regex didn't parse: %A" line
            }

        use cancel = new System.Threading.CancellationTokenSource()
        Async.Start(runner,cancel.Token)

        win.Run()

        cancel.Cancel()


