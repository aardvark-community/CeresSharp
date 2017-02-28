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

    module private Cache =
        open System.Collections.Concurrent

        let cache = 
            ConcurrentDictionary<Feature[]*Feature[], (int*int)[]>()

        let cached (v : 'a) = failwith ""
            
    
    let asd path =

        let bruteforceThreshold = 0.9

        let probabilityLambda = 35.0
        let probabilitySigma = 0.5
        let probabilityLowerThresh = 0.5

        let affineLambda = 20.0
        let affineSigma = 0.5
        let affineUpperThresh = 0.01

        let bruteforceMinCount = 20
        let probableMinCount =   20
        let affineMinCount =     20

        Log.line "Reading images ... "
        let images = 
            System.IO.Directory.GetFiles path
                |> Array.map PixImage.Create
                |> Array.map (fun pi -> pi.AsPixImage<byte>())
        Log.line "Found %A images." images.Length

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

        Log.startTimed "Creating features"
        let data = images |> Array.map Akaze.ofImage
        Log.stop ()

        Log.startTimed "Building feature graph"
        let mst = Feature.FeatureGraph.build pairMatch images data
        Log.stop()

        ()
        ()