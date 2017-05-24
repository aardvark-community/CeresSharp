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

    //let createProblem (cameras : int) (points : int) =
    //    let rand = RandomSystem()

    //    let realPoints = Array.init points (fun _ -> rand.UniformV3dDirection())
    //    let realCameras = Array.init cameras (fun _ -> Camera3d.LookAt(rand.UniformV3dDirection() * 5.0, V3d.Zero, 1.0, V3d.OOI))

    //    let colors = [1.0..2000.0] |> Seq.mapi (fun i _ -> Feature.rgbaFromHsva(6.0 * float i / float [1.0..2000.0].Length, 0.5, 1.0).ToC4b()) |> Seq.toArray

    //    let measurements =
    //        Array.init realCameras.Length (fun ci ->
    //            let pxSize = 500
    //            let pimg = PixImage<byte>(Col.Format.RGBA, V2i(pxSize,pxSize))
    //            let matrix = pimg.GetMatrix<C4b>()
    //            matrix.SetByCoord ( fun _ -> C4b.White ) |> ignore
    //            let corr =
    //                [
    //                    for i in 0 .. realPoints.Length - 1 do
    //                        if rand.UniformDouble() < 2.0 then
    //                            let jitter = rand.UniformV2dDirection() * rand.UniformDouble()* 0.005 * 5.0  // 5%
    //                            let p = jitter + realCameras.[ci].Project realPoints.[i] 
                                
    //                            let cp = V2i (V2d.Half + V2d(0.5 * p.X + 0.5, 0.5 - 0.5 * p.Y) * V2d(float pxSize,float pxSize))

    //                            matrix.SetCross(cp, 5, (colors.[i]))

    //                            yield i, p
    //                ] |> List.toSeq

    //            ci,Map.ofSeq corr,pimg
    //        ) 

    //    let imgs = measurements |> Array.map ( fun (_,_,x) -> x )

    //    let measurements = 
    //        measurements |> Array.map ( fun (a,b,_) -> a,b) |> Map.ofArray

    //    let input = 
    //        BundlerInput.preprocess {
    //            measurements = measurements
    //            tracks = failwith "implement me"
    //        }

    //    realPoints, realCameras, imgs, BundlerInput.toProblem input

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



    //let syntheticCameras (cameras : int) (points : int) =
    //    let realPoints, realCameras, realPimgs, problem = createProblem cameras points


    //    Log.startTimed "solver"
    //    let sol = Bundler.solve ignore problem //solve 0 4 problem
    //    match sol with
    //    | Some sol -> 
    //        let err = sol |> BundlerSolution.errorMetrics
    //        Log.start "error metrics"
    //        Log.line "cost:    %A" err.cost
    //        Log.line "average: %.4f%%" (100.0 * err.average)
    //        Log.line "stdev:   %.4f%%" (100.0 * err.stdev)
    //        Log.line "min:     %.4f%%" (100.0 * err.min)
    //        Log.line "max:     %.4f%%" (100.0 * err.max)
    //        Log.stop()
    //        Log.stop()

    //        let input = { cost = 0.0; problem = problem; points = realPoints |> Seq.map ( fun p -> { point = p; isFixed = false } ) |> Seq.indexed |> Map.ofSeq; cameras = realCameras |> Seq.map ( fun c -> { cam = c; isFixed = false } ) |> Seq.indexed |> Map.ofSeq }

    //        input, sol, realPimgs
    //    | None -> 
    //        failwith "No convergence."



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
    
open Aardvark.Reconstruction

type Value() =
    member x.GetSomething i = 1

type Bla< ^a, ^b when ^a : (member GetSomething : int -> ^b) > =
    {
        s : ^a
    }

    static member inline get< ^a,^b when ^a : (member GetSomething : int -> ^b)> ( bla : Bla< ^a,^b > ) ( i : int ) =
        let y = ( ^a : (member GetSomething : int -> ^b ) bla.s,i )
        y

let x = Value()

let bla = { s = x }

let y = Bla.get<Value,int> bla 123



//let x = [1.0;2.0;3.0;4.0]

//let y = { s = x }
   
//let z = 
//    y.s

[<EntryPoint>]
let main argv =
    Ag.initialize()
    Aardvark.Init()

    //testGlobal()
    //System.Environment.Exit 0
    
    //let path = @"C:\blub\yolo"

//    PairViewer.app @"C:\blub\yolo"
    BundlerViewer.folder @"C:\blub\yolo1"

    let sponzacfg =
        {
            Example.PercentObservations = 0.3
            Example.JitterNDC           = 1.0 / 500.0
            Example.PercentFalse        = 0.4
        }

    //Example.renderSponza sponzacfg @"D:\file\sponza_bun\sponzaVertices" 10 (Some 1000)
    //BundlerViewer.sponza @"D:\file\sponza_bun\sponzaVertices"

//    BundlerViewer.filesSuperEvilHack @"C:\blub\yolo"
//    BundlerViewer.sponzaWithoutRender @"D:\file\sponza_bun\sponzaVertices" |> ignore

//    Example.testManySponzas()

    





    0 // return an integer exit code
