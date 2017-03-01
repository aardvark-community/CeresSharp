namespace Aardvark.Ceres

open System
open Aardvark.Base
open Aardvark.Base.Incremental

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

    open MBrace.FsPickler
    open System.IO

    module private Cache =
        open System.Collections.Concurrent

        let patha = @"C:\blub\cached2"
        let filea = "hallo"
        
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

        let bruteforceThreshold = 0.9

        let probabilityLambda = 14.0
        let probabilitySigma = 0.5
        let probabilityLowerThresh = 0.4

        let affineLambda = 1.0
        let affineSigma = 0.6
        let affineUpperThresh = 0.001

        let bruteforceMinCount = 20
        let probableMinCount =   20
        let affineMinCount =     20

        let cacheBundlerResult = false
        let cacheFeatureMatching = true


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



        Log.startTimed "Building feature graph"
        let mst = Feature.FeatureGraph.build cachedMatch images data
        Log.stop()

        if cacheFeatureMatching then printfn "Saving match cache."; save()

        Log.startTimed "BundlerInput generation"
        let input = Feature.FeatureGraph.toBundlerInput mst 3 120

        let problem = input |> BundlerInput.preprocess
                            |> BundlerInput.toProblem

        


        Log.stop()

        let ser = FsPickler.CreateBinarySerializer()

        let patha = @"C:\blub\cached2"
        let filea = "asfasdf"
        
        let fn = 
            if Directory.Exists patha |> not then Directory.CreateDirectory patha |> ignore
            Path.combine [patha;filea]

        if cacheBundlerResult then
            if File.Exists fn then
                printfn "Taking cached bundle result."
                ser.UnPickle (File.ReadAllBytes fn)
            else
                let solution = Bundler.solve problem
                printfn "Saving bundle result cache."
                ser.Pickle solution |> File.writeAllBytes fn
                solution
        else
            Bundler.solve problem