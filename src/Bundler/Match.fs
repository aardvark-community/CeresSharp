namespace Aardvark.Ceres

open System
open Aardvark.Base
open Aardvark.Base.Incremental

open TriangleNet.Geometry

open Aardvark.Base.Rendering
open Aardvark.Rendering
open Aardvark.SceneGraph

module Match =

    let bruteforce t l r = Feature.matchCandidates t l r
    
    let probability l s p ms =
        
        let fn = MatchProblem.likelihood l s ms

        ms |> Array.filter (fun m -> 
            fn m >= p
        )

    let affine l s t ms =
        
        let (qn,_,_) = MatchProblem.affineDistance l s ms

        ms |> Array.filter ( fun m ->
            (qn m).LengthSquared < t
        )

module Bundle =

    
    type stuffConfig =
        {
            pLambda : float
            pSigma  : float
            pProb   : float
            aLambda : float
            aSigma  : float
            aThresh : float
        }

    let configfile () = 
        let p = @"C:\blub"
        if System.IO.Directory.Exists p |> not then System.IO.Directory.CreateDirectory p |> ignore
        Path.combine [p; "config.config"]

    open MBrace.FsPickler
    open System.IO

    module private Cache =
        open System.Collections.Concurrent

        let patha = @"C:\blub\cached2"
        let filea = "matches"
        
        let fn = 
            if Directory.Exists patha |> not then Directory.CreateDirectory patha |> ignore
            Path.combine [patha;filea]

        let cache = 
            let ser = FsPickler.CreateBinarySerializer()
            match File.Exists fn with
            | false ->
                ConcurrentDictionary<string*string, (int*int)[]>()
            | true ->
                File.ReadAllBytes fn |> ser.UnPickle

        let save() = 
            let ser = FsPickler.CreateBinarySerializer()
            File.WriteAllBytes(fn,ser.Pickle cache)


        let cached k (vc : string*string -> (int*int)[]) = 
            cache.GetOrAdd(k,vc)

    open Cache

    let filesIn path =

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

        let allCamerasSameDistortion = true

        let cacheFeatureMatching    = false
        let cacheBundlerResult      = false


        Log.line "Reading images ... "
        let filenames = System.IO.Directory.GetFiles path
        let images = 
            filenames
                |> Array.map PixImage.Create
                |> Array.map (fun pi -> pi.AsPixImage<byte>())
        Log.line "Found %A images." images.Length

        
        Log.startTimed "Creating features"
        let data = images |> Array.map Akaze.ofImage
        Log.stop ()

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

        let all =
            let all = Array.zip filenames data
            [|
                for a in all do
                    for b in all do
                        yield a,b
            |]

        let cachedMatch l r =
            if cacheFeatureMatching then
                all
                    |> Array.choose ( fun ((limg, lf),(rimg, rf)) ->
                        if lf = l && rf = r then
                            cached (limg,rimg) ( fun _ -> pairMatch l r) |> Some
                        else
                            None
                    ) |> Array.exactlyOne
            else
                pairMatch l r



        if cacheFeatureMatching then printfn "Saving match cache."; save()



        Log.startTimed "Building feature graph"
        let mst = Feature.FeatureGraph.build cachedMatch images data
        Log.stop()





        Log.startTimed "BundlerInput generation"
        let input = Feature.FeatureGraph.toBundlerInput mst bundlerMinTrackLength bundlerMaxFtrsPerCam

        let problem = input |> BundlerInput.toProblem

        


        Log.stop()

        let ser = FsPickler.CreateBinarySerializer()

        let patha = @"C:\blub\cached2"
        let filea = "bundler"
        
        let fn = 
            if Directory.Exists patha |> not then Directory.CreateDirectory patha |> ignore
            Path.combine [patha;filea]

        let sol = 
            if cacheBundlerResult then
                if File.Exists fn then
                    printfn "Taking cached bundle result."
                    ser.UnPickle (File.ReadAllBytes fn)
                else
                    let solution = Bundler.solve (not allCamerasSameDistortion) problem
                    printfn "Saving bundle result cache."
                    ser.Pickle solution |> File.writeAllBytes fn
                    solution
            else
                Bundler.solve (not allCamerasSameDistortion) problem
        
        
        match sol.cameras |> Map.toList |> List.map snd |> List.tryFind ( fun (c,_) -> c.FocalLength.IsNaN() ) with Some c -> Log.warn "camera focal length is NaN !!!! abort!! %A" c; System.Environment.Exit 0 | None -> ()

        Log.startTimed "Tessellating for %A cam and %A points" sol.cameras.Count sol.points.Count
        
        let ps = 
            [
                for kvp in sol.cameras do
                    let ci = kvp.Key
                    let c = kvp.Value

                    let poly = TriangleNet.Geometry.Polygon(sol.points.Count)
                    for pw in sol.points do
                        let (p,d) = (c |> fst).ProjectWithDepth pw.Value
                        let vertex = TriangleNet.Geometry.Vertex(-p.X, -p.Y, 0, 1)
                        vertex.Attributes.[0] <- d
                        poly.Add vertex

                    Log.startTimed "Triangulation cam %A" ci
                    let constraintoptions = TriangleNet.Meshing.ConstraintOptions()
                    //constraintoptions.ConformingDelaunay <- true
                    //constraintoptions.Convex <- true

                    let qualityoptions = TriangleNet.Meshing.QualityOptions()
                    //qualityoptions.MinimumAngle <- 20.0
                    //qualityoptions.MaximumAngle <- 120.0

                    let mesh = (poly :> TriangleNet.Geometry.IPolygon).Triangulate( constraintoptions, qualityoptions )
                    
                    yield ci, mesh
                    Log.stop()
            ] |> Map.ofList

        Log.stop()

        let tris =
            [
                for kvp in sol.cameras do
                    let ci = kvp.Key
                    let c = kvp.Value

                    let mesh = ps.[ci]

                    let tris = 
                        [|
                            for tri in mesh.Triangles do    
                        
                                let v i = 
                                    let v = tri.GetVertex i
                                    let x = -v.X
                                    let y = -v.Y
                                    let z = v.Attributes.[0]
                                    
                                    (c |> fst).Unproject (V2d(x,y)) z

                                let t = Triangle3d(v 0, v 1, v 2)

                                yield t
                        |]
                    yield ci, tris
            ] |> Map.ofList

        let someSg =
            let mutable a = false
            [
                for kvp in sol.cameras (* |> Map.filter (constF (constF false)) *) do
                    let ci = kvp.Key
                    let c = kvp.Value

                    let mesh = ps.[ci]

                    let tris = 
                        [|
                            for tri in mesh.Triangles do    
                        
                                let v i = 
                                    let v = tri.GetVertex i
                                    let x = -v.X
                                    let y = -v.Y
                                    let z = v.Attributes.[0]
                                    
                                    (c |> fst).Unproject (V2d(x,y)) z

                                let tc i =
                                    let v = tri.GetVertex i
                                    let x = v.X
                                    let y = v.Y
                                    V2d(0.5 * x, 0.5 * y) + 0.5

                                let t = Triangle3d(v 0, v 1, v 2)
                                let n = (t.P0 - t.P1).Normalized.Cross( (t.P2 - t.P1).Normalized )
                                let tc = Triangle2d( tc 0, tc 1, tc 2 ) 

                                yield t,n,tc
                        |]

                    
                    yield 
                        Sg.draw IndexedGeometryMode.TriangleList
                            |> Sg.vertexAttribute' DefaultSemantic.Positions (tris |> Array.collect ( fun (t,_,_) -> [| t.P0 |> V3f; t.P1 |> V3f; t.P2 |> V3f |] ))
                            |> Sg.vertexAttribute' DefaultSemantic.DiffuseColorCoordinates (tris |> Array.collect ( fun (_,_,t) -> [| t.P0 |> V2f; t.P1 |> V2f; t.P2 |> V2f |] ))
                            |> Sg.vertexAttribute' DefaultSemantic.Normals (tris |> Array.collect ( fun (_,n,_) -> [| n |> V3f; n |> V3f; n |> V3f |] ))
                            |> Sg.vertexBufferValue DefaultSemantic.Colors (Mod.constant ( if a then (C4f(1.0,1.0,1.0,0.75)).ToV4f() else (C4f(1.0,0.0,0.0,0.75)).ToV4f()))
                            |> Sg.diffuseTexture' (PixTexture2d(PixImageMipMap [|images.[ci] :> PixImage|], true)), ci
                    a <- true
                            
            ] 

               

        sol, tris,someSg, images


