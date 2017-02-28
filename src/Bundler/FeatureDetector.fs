namespace Aardvark.Ceres

open System
open System.IO
open Aardvark.Base

#nowarn "9"

type FeatureDescriptor internal(raw : float[]) =
    let dim = raw.Length
    let scale = sqrt (float dim)
    let vector = raw |> Array.map (fun v -> v / scale)

    member x.Dimension = dim
    member x.Data = vector
    member x.RawData = raw

    static member Distance(l : FeatureDescriptor, r : FeatureDescriptor) =
        if l.Dimension <> r.Dimension then
            failwithf "cannot compare features with different dimensions: %A vs %A" l.Dimension r.Dimension

        let l = l.Data
        let r = r.Data
        let mutable res = 0.0
        for i in 0 .. l.Length - 1 do
            let v = l.[i] - r.[i]
            res <- res + v * v
        sqrt res

//sift
///%f1= [x ,y, scale, rotation, tilt(t), other_rot (phi)];


type Feature =
    {
        ndc         : V2d
        angle       : float
        size        : float
        response    : float
        descriptor  : FeatureDescriptor
    }



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Match2d =
    
    let ofFeatures (l : Feature[]) (r : Feature[]) (ms : (int*int)[]) = 
        ms |> Array.map ( fun (li,ri) -> 
            Match2d(l.[li].ndc, r.[ri].ndc - l.[li].ndc, MatchProblem.o (r.[ri].angle - l.[li].angle), li, ri) 
        )
        

[<AutoOpen>]
module private CVHelpers =
    open OpenCvSharp
    open OpenCvSharp.XFeatures2D
    open Microsoft.FSharp.NativeInterop

    [<AbstractClass>]
    type PixImageVisitor<'r>() =
        static let table =
            LookupTable.lookupTable [
                typeof<int8>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<int8>(unbox img, 127y))
                typeof<uint8>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<uint8>(unbox img, 255uy))
                typeof<int16>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<int16>(unbox img, Int16.MaxValue))
                typeof<uint16>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<uint16>(unbox img, UInt16.MaxValue))
                typeof<int32>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<int32>(unbox img, Int32.MaxValue))
                typeof<uint32>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<uint32>(unbox img, UInt32.MaxValue))
                typeof<int64>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<int64>(unbox img, Int64.MaxValue))
                typeof<uint64>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<uint64>(unbox img, UInt64.MaxValue))
                typeof<float16>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<float16>(unbox img, float16(Float32 = 1.0f)))
                typeof<float32>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<float32>(unbox img, 1.0f))
                typeof<float>, (fun (self : PixImageVisitor<'r>, img : PixImage) -> self.Visit<float>(unbox img, 1.0))
            ]
        abstract member Visit<'a when 'a : unmanaged> : PixImage<'a> * 'a -> 'r

        


        interface IPixImageVisitor<'r> with
            member x.Visit<'a>(img : PixImage<'a>) =
                table (typeof<'a>) (x, img)

    let matTypes =
        LookupTable.lookupTable [
            PixFormat.ByteBGR, MatType.CV_8UC3
            PixFormat.ByteBGRA, MatType.CV_8UC4
            PixFormat.ByteBGRP, MatType.CV_8UC4
            PixFormat.ByteBW, MatType.CV_8UC1
            PixFormat.ByteGray, MatType.CV_8UC1
            PixFormat.ByteRGB, MatType.CV_8UC3
            PixFormat.ByteRGBA, MatType.CV_8UC4
            PixFormat.ByteRGBP, MatType.CV_8UC4

            PixFormat.UShortBGR, MatType.CV_16UC3
            PixFormat.UShortBGRA, MatType.CV_16UC4
            PixFormat.UShortBGRP, MatType.CV_16UC4
            PixFormat.UShortGray, MatType.CV_16UC1
            PixFormat.UShortRGB, MatType.CV_16UC3
            PixFormat.UShortRGBA, MatType.CV_16UC4
            PixFormat.UShortRGBP, MatType.CV_16UC4

            PixFormat.FloatBGR, MatType.CV_32FC3
            PixFormat.FloatBGRA, MatType.CV_32FC4
            PixFormat.FloatBGRP, MatType.CV_32FC4
            PixFormat.FloatGray, MatType.CV_32FC1
            PixFormat.FloatRGB, MatType.CV_32FC3
            PixFormat.FloatRGBA, MatType.CV_32FC4
            PixFormat.FloatRGBP, MatType.CV_32FC4
        ]

    type VolumeInfo with
        member x.Transformed(t : ImageTrafo) =
            let sx = x.SX
            let sy = x.SY
            let sz = x.SZ
            let dx = x.DX
            let dy = x.DY
            let dz = x.DZ
            match t with
                | ImageTrafo.Rot0 -> x
                | ImageTrafo.Rot90 -> x.SubVolume(sx - 1L, 0L, 0L, sy, sx, sz, dy, -dx, dz)
                | ImageTrafo.Rot180 -> x.SubVolume(sx - 1L, sy - 1L, 0L, sx, sy, sz, -dx, -dy, dz)
                | ImageTrafo.Rot270 -> x.SubVolume(0L, sy - 1L, 0L, sy, sx, sz, -dy, dx, dz)
                | ImageTrafo.MirrorX -> x.SubVolume(sx - 1L, 0L, 0L, sx, sy, sz, -dx, dy, dz)
                | ImageTrafo.Transpose -> x.SubVolume(0L, 0L, 0L, sy, sx, sz, dy, dx, dz)
                | ImageTrafo.MirrorY -> x.SubVolume(0L, sy - 1L, 0L, sx, sy, sz, dx, -dy, dz)
                | ImageTrafo.Transverse -> x.SubVolume(sx - 1L, sy - 1L, 0L, sy, sx, sz, -dy, -dx, dz)
                | _ -> failwithf "invalid ImageTrafo"

    type PixImage with
        member x.ToMat(trafo : ImageTrafo) =
            let x = x.Transformed(trafo)
            let input = new Mat(x.Size.Y, x.Size.X, matTypes x.PixFormat)

            x.Visit 
                { new PixImageVisitor<int>() with
                    member x.Visit(img : PixImage<'a>, emptyVal : 'a) =
                        let img = 
                            match img.Format with
                                | Col.Format.RGB -> img.ToFormat(Col.Format.BGR)
                                | Col.Format.RGBA -> img.ToFormat(Col.Format.BGRA)
                                | Col.Format.RGBP -> img.ToFormat(Col.Format.BGRP)
                                | _ -> img


                        let srcInfo = img.Volume.Info
                            

                        let dstInfo =
                            VolumeInfo(
                                0L,
                                V3l(srcInfo.SX, srcInfo.SY, srcInfo.SZ),
                                V3l(srcInfo.SZ, srcInfo.SX * srcInfo.SZ, 1L)
                            )

                        let dst = NativeVolume<'a>(NativePtr.ofNativeInt input.Data, dstInfo)
                        NativeVolume.using img.Volume (fun src ->
                            NativeVolume.copy src dst
                        )   
                        0
                } |> ignore

            input

        member x.ToMat() =
            x.ToMat(ImageTrafo.Rot0)

module Akaze =
    open OpenCvSharp
    open OpenCvSharp.XFeatures2D
    open Microsoft.FSharp.NativeInterop

    let akaze = AKAZE.Create()


    let private ofMat (input : Mat) =
        let features = akaze.Detect(input)

        let mutable features = features
        use descriptors = new Mat()
        akaze.Compute(InputArray.op_Implicit input, &features, OutputArray.op_Implicit descriptors)

        features |> Array.mapi (fun i f ->
            let desc = Array.create descriptors.Cols 0uy
            descriptors.Row.[i].GetArray(0, 0, desc)

            let x = float f.Pt.X / float input.Cols
            let y = float f.Pt.Y / float input.Rows

            {
                ndc = V2d(2.0 * x - 1.0, 1.0 - 2.0 * y)
                angle = float f.Angle * Constant.RadiansPerDegree
                size = float f.Size
                response = float f.Response
                descriptor = FeatureDescriptor (desc |> Array.map (fun v -> float v / 255.0))
            }
        )

    let ofFile (file : string) =
        use input = new Mat(file, ImreadModes.Color)
        ofMat input

    let ofImage (img : PixImage) =
        use input = img.ToMat()
        ofMat input


module Orb =
    open OpenCvSharp
    open OpenCvSharp.XFeatures2D
    open Microsoft.FSharp.NativeInterop
    
    let orb = ORB.Create()

    let private ofMat (input : Mat) =
        let features = orb.Detect(input)
        

        let mutable features = features
        use descriptors = new Mat()
        orb.Compute(InputArray.op_Implicit input, &features, OutputArray.op_Implicit descriptors)

        features |> Array.mapi (fun i f ->
            let desc = Array.create descriptors.Cols 0uy
            descriptors.Row.[i].GetArray(0, 0, desc)

            let x = float f.Pt.X / float input.Cols
            let y = float f.Pt.Y / float input.Rows

            {
                ndc = V2d(2.0 * x - 1.0, 1.0 - 2.0 * y)
                angle = float f.Angle
                size = float f.Size
                response = float f.Response
                descriptor = FeatureDescriptor (desc |> Array.map (fun v -> float v / 255.0))
            }
        )

    let ofFile (file : string) =
        use input = new Mat(file)
        ofMat input

    let ofImage (img : PixImage) =
        use input = img.ToMat()
        ofMat input

module Brisk =
    open OpenCvSharp
    open OpenCvSharp.XFeatures2D
    open Microsoft.FSharp.NativeInterop
    
    let brisk = BRISK.Create()

    let private ofMat (input : Mat) =
        let features = brisk.Detect(input)
        

        let mutable features = features
        use descriptors = new Mat()
        brisk.Compute(InputArray.op_Implicit input, &features, OutputArray.op_Implicit descriptors)

        features |> Array.mapi (fun i f ->
            let desc = Array.create descriptors.Cols 0uy
            descriptors.Row.[i].GetArray(0, 0, desc)

            let x = float f.Pt.X / float input.Cols
            let y = float f.Pt.Y / float input.Rows

            {
                ndc = V2d(2.0 * x - 1.0, 1.0 - 2.0 * y)
                angle = float f.Angle
                size = float f.Size
                response = float f.Response
                descriptor = FeatureDescriptor (desc |> Array.map (fun v -> float v / 255.0))
            }
        )

    let ofFile (file : string) =
        use input = new Mat(file)
        ofMat input

    let ofImage (img : PixImage) =
        use input = img.ToMat()
        ofMat input


type MatchingConfig =
    {
        threshold : float
        guidedThreshold : float
        minTrackLength : int
        distanceThreshold : float
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Feature =
    open System.Collections.Generic
    open OpenCvSharp
    open OpenCvSharp.XFeatures2D
    open Microsoft.FSharp.NativeInterop
    open CeresSharp

    let private fundamentalMat (config : MatchingConfig) (l : V2d[]) (r : V2d[]) =

//        let la = l |> Array.map (fun l -> V3d(l, 0.0))
//        let ra = r |> Array.map (fun r -> V3d(r, 0.0))
//
//
//        let trafo = PointCloud.trafo la ra
//        trafo.Forward
        use lMat = new Mat(1, l.Length, MatType.CV_64FC2)
        use rMat = new Mat(1, r.Length, MatType.CV_64FC2)
        for i in 0 .. l.Length - 1 do
            let lp = Vec2d(l.[i].X, l.[i].Y)
            let rp = Vec2d(r.[i].X, r.[i].Y)
            lMat.Set(0, i, lp)
            rMat.Set(0, i, rp)

        use res = Cv2.FindFundamentalMat(InputArray.op_Implicit rMat, InputArray.op_Implicit lMat, FundamentalMatMethod.LMedS, config.distanceThreshold, 0.999)
   
        M33d(
            res.Get(0, 0), res.Get(0, 1), res.Get(0, 2),
            res.Get(1, 0), res.Get(1, 1), res.Get(1, 2),
            res.Get(2, 0), res.Get(2, 1), res.Get(2, 2)
        )

    let private homography (l : V2d[]) (r : V2d[]) =
        use lMat = new Mat(1, l.Length, MatType.CV_64FC2)
        use rMat = new Mat(1, r.Length, MatType.CV_64FC2)
        for i in 0 .. l.Length - 1 do
            let lp = Vec2d(l.[i].X, l.[i].Y)
            let rp = Vec2d(r.[i].X, r.[i].Y)
            lMat.Set(0, i, lp)
            rMat.Set(0, i, rp)

        use mask = new Mat()
        use res = Cv2.FindHomography(InputArray.op_Implicit lMat, InputArray.op_Implicit rMat, HomographyMethods.Ransac, 0.002, OutputArray.op_Implicit mask)
   
        

        let maskData : byte[] = Array.zeroCreate mask.Rows
        mask.GetArray(0,0, maskData)

        let used =
            maskData 
                |> Seq.indexed
                |> Seq.choose (fun (i, v) -> if v <> 0uy then Some i else None)
                |> Set.ofSeq

        let homography = 
            M33d(
                res.Get(0, 0), res.Get(0, 1), res.Get(0, 2),
                res.Get(1, 0), res.Get(1, 1), res.Get(1, 2),
                res.Get(2, 0), res.Get(2, 1), res.Get(2, 2)
            )

        homography, used

    let private stereoRectify (l : V2d[]) (r : V2d[]) =
        use lMat = new Mat(1, l.Length, MatType.CV_64FC2)
        use rMat = new Mat(1, r.Length, MatType.CV_64FC2)
        for i in 0 .. l.Length - 1 do
            let lp = Vec2d(l.[i].X, l.[i].Y)
            let rp = Vec2d(r.[i].X, r.[i].Y)
            lMat.Set(0, i, lp)
            rMat.Set(0, i, rp)
            
        use mask = new Mat()
        use fMat = Cv2.FindFundamentalMat(InputArray.op_Implicit lMat, InputArray.op_Implicit rMat, FundamentalMatMethod.Ransac, 0.002, 0.99, OutputArray.op_Implicit mask)



        let maskData : byte[] = Array.zeroCreate mask.Rows
        mask.GetArray(0,0, maskData)
        let used =
            maskData 
                |> Seq.indexed
                |> Seq.choose (fun (i, v) -> if v <> 0uy then Some i else None)
                |> Seq.toArray

        use lMat = new Mat(1, used.Length, MatType.CV_64FC2)
        use rMat = new Mat(1, used.Length, MatType.CV_64FC2)
        let mutable i = 0
        for pi in used do
            let lp = Vec2d(l.[pi].X, l.[pi].Y)
            let rp = Vec2d(r.[pi].X, r.[pi].Y)
            lMat.Set(0, i, lp)
            rMat.Set(0, i, rp)
            i <- i + 1

        use h1 = new Mat(4, 4, MatType.CV_64FC1)
        use h2 = new Mat(4, 4, MatType.CV_64FC1)
        let worked = Cv2.StereoRectifyUncalibrated(InputArray.op_Implicit lMat, InputArray.op_Implicit rMat, InputArray.op_Implicit fMat, Size(2.0, 2.0), OutputArray.op_Implicit h1, OutputArray.op_Implicit h2)
        Log.warn "worked: %A" worked

        let h1 = 
            M44d(
                h1.Get(0, 0), h1.Get(0, 1), 0.0, h1.Get(0, 2),
                h1.Get(1, 0), h1.Get(1, 1), 0.0, h1.Get(1, 2),
                0.0,          0.0,          1.0, 0.0,
                h1.Get(2, 0), h1.Get(2, 1), 0.0, h1.Get(2, 2)
            )

        let h2 = 
            M44d(
                h2.Get(0, 0), h2.Get(0, 1), 0.0, h2.Get(0, 2),
                h2.Get(1, 0), h2.Get(1, 1), 0.0, h2.Get(1, 2),
                0.0,          0.0,          1.0, 0.0,
                h2.Get(2, 0), h2.Get(2, 1), 0.0, h2.Get(2, 2)
            )

        h1, h2

    let matchCandidates (t : float) (l : Feature[]) (r : Feature[]) =
        use matcher = new BFMatcher(normType = NormTypes.Hamming)

        use lMat = new Mat(l.Length, l.[0].descriptor.Dimension, MatType.CV_8UC1)
        for i in 0 .. l.Length - 1 do
            let arr = l.[i].descriptor.RawData |> Array.map (fun v -> uint8 (v * 255.0))
            lMat.Row.[i].SetArray(0, 0, arr)
        matcher.Add [lMat]

        use rMat = new Mat(r.Length, r.[0].descriptor.Dimension, MatType.CV_8UC1)
        for i in 0 .. r.Length - 1 do
            let arr = r.[i].descriptor.RawData |> Array.map (fun v -> uint8 (v * 255.0))
            rMat.Row.[i].SetArray(0, 0, arr)

        let matches = matcher.KnnMatch(rMat, 2)

        let reallyGoodMatches =
            matches |> Seq.choose (fun m ->
                let m0 = m.[0]
                let m1 = m.[1]

                if float m0.Distance < t * float m1.Distance then
                    Some (m0.TrainIdx, m0.QueryIdx)
                else
                    None

            ) |> Seq.toArray

        reallyGoodMatches

    let matchesOld (config : MatchingConfig) (l : Feature[]) (r : Feature[]) =
        let reallyGoodMatches = matchCandidates config.threshold l r

        if reallyGoodMatches.Length >= 10 then
            Log.warn "found %d anchor matches" reallyGoodMatches.Length

            Array.toList reallyGoodMatches

//            let lPoints = reallyGoodMatches |> Array.map (fun (li,_) -> l.[li].ndc)
//            let rPoints = reallyGoodMatches |> Array.map (fun (_,ri) -> r.[ri].ndc)
//            let hom, used = homography lPoints rPoints
//
//
//
//
//            let final = 
//                used 
//                    |> Seq.map (fun i -> reallyGoodMatches.[i])
//                    |> Seq.toArray
//                matches |> Seq.choose (fun m ->
//            
//                    let m0 = m.[0]
//                    let m1 = m.[1]
//                
//                    let lPoint = l.[m0.TrainIdx].ndc
//                    let rPoint = r.[m0.QueryIdx].ndc
//
//                    let rExpected = hom.TransformPosProj lPoint
//
//
////                    let n = fMat * V3d(lPoint, 1.0) |> Vec.normalize
////                    let distance = Vec.dot (V3d(rPoint, 1.0)) n |> abs
//                    let distance = rExpected - rPoint |> Vec.length
//                    if m0.Distance < 96.0f && distance < 0.01 then
//                        Some (m0.TrainIdx, m0.QueryIdx)
//                    else
//                        None
//
//                ) |> Seq.toArray
//
//            if final.Length >= 20 then
//
//                Log.warn "made %d out of it" final.Length
//                Array.toList final
//            else 
//                Log.warn "found no matches"
//                []

        else
            Log.warn "found no matches"
            []

    open Aardvark.Ceres

    let matchesNotSoOld (config : MatchingConfig) (l : Feature[]) (r : Feature[]) =
        let reallyGoodMatches = matchCandidates config.threshold l r

        let distThreshold = config.distanceThreshold
        if reallyGoodMatches.Length >= 12 then
            let randomSubsetSolution() = 
                let randomMatches =
                    reallyGoodMatches.RandomOrder()
                        |> Seq.take 12
                        |> Seq.toArray
                

                let m0 = randomMatches |> Seq.mapi (fun i (li, _) -> i, l.[li].ndc) |> Map.ofSeq
                let m1 = randomMatches |> Seq.mapi (fun i (_, ri) -> i, r.[ri].ndc) |> Map.ofSeq

                Bundler.solveSimple 3 {
                    input = { measurements = [| 0, m0; 1, m1 |] |> Map.ofArray }
                    cameras = Set.ofList [0; 1]
                }

            let countInliers (threshold : float) (sol : BundlerSolution) =
                let c0 = sol.cameras.[0]
                let c1 = sol.cameras.[1]

                let mutable inliers = 0
                for li, ri in reallyGoodMatches do
                    let lPoint = l.[li].ndc
                    let rPoint = r.[ri].ndc
                    let lRay = c0.GetRay(lPoint)
                    let rRay = c1.GetRay(rPoint)
                    
                    let pt = lRay.GetMiddlePoint(rRay)
                    let lTest = c0.Project pt
                    let rTest = c1.Project pt

                    let lRes = lTest - lPoint |> Vec.length
                    let rRes = rTest - rPoint |> Vec.length

                    if lRes < threshold && rRes < threshold then
                        inliers <- inliers + 1

                    
                    ()


                inliers

            let allInliners (threshold : float) (sol : BundlerSolution) =
                let c0 = sol.cameras.[0]
                let c1 = sol.cameras.[1]

                let mutable inliers = 0
                reallyGoodMatches |> Array.filter (fun (li, ri) ->
                    let lPoint = l.[li].ndc
                    let rPoint = r.[ri].ndc
                    let lRay = c0.GetRay(lPoint)
                    let rRay = c1.GetRay(rPoint)
                    
                    let pt = lRay.GetMiddlePoint(rRay)
                    let lTest = c0.Project pt
                    let rTest = c1.Project pt

                    let lRes = lTest - lPoint |> Vec.length
                    let rRes = rTest - rPoint |> Vec.length

                    lRes < threshold && rRes < threshold
                   )


            let mutable maxScore = -1
            let mutable bestSolution = Unchecked.defaultof<_>
            for i in 1 .. 10 do
                match randomSubsetSolution() with
                    | Some sol -> 
                        let score = countInliers distThreshold sol
                        if score > maxScore then
                            maxScore <- score
                            bestSolution <- sol
                    | _ ->
                        ()





            if maxScore >= 12 then
                allInliners distThreshold bestSolution
            else
                [||]
        else
            [||]

    let matches (config : MatchingConfig) (l : Feature[]) (r : Feature[]) =
        let reallyGoodMatches = matchCandidates config.threshold l r

        if reallyGoodMatches.Length >= 10 then

            let lPoints = reallyGoodMatches |> Array.map (fun (li,_) -> l.[li].ndc)
            let rPoints = reallyGoodMatches |> Array.map (fun (_,ri) -> r.[ri].ndc)
            let F = fundamentalMat config lPoints rPoints
            let F = F.Transposed

//            let lI = reallyGoodMatches |> Array.map fst
//            let rI = reallyGoodMatches |> Array.map snd

            let resultList = List()
            let lines = List()

            let getLine li =
                let line = F * V3d(l.[li].ndc,1.0)
                Plane2d(line.XY, line.Z)

            for li in 0 .. l.Length-1 do
                let x = l.[li]

                let line = F * V3d(x.ndc,1.0)
                let p = Plane2d(line.XY, -line.Z)


                // n . p - d = 0
                // (n * f . p) - d * f = 0
                //let line = line / line.XY.Length

                let candidates = 
                    r |> Array.choosei (
                        fun ri r -> 
                            let dist = p.Normalized.Height r.ndc |> abs //Vec.dot line (V3d(r.ndc,1.0)) |> abs
                            if dist < config.distanceThreshold then
                                let featureDist = FeatureDescriptor.Distance (x.descriptor,r.descriptor)
                                if featureDist < 0.5 then
                                    Some (ri,featureDist)
                                else
                                    None
                            else
                                None
                    ) |> Array.sortBy snd
//                 
                if candidates.Length = 1 then
                    let (fi,fd) = candidates.[0]
                    if fd < 0.15 then
                        lines.Add(p)
                        resultList.Add(li,fi)

                if candidates.Length >= 2 then
                    let (fi,fd) = candidates.[0]
                    let (_,sd) = candidates.[1]
                    if fd < (config.guidedThreshold) * sd then
                        let p = Plane2d(line.XY, -line.Z)
                        lines.Add(p)
                        resultList.Add(li,fi)

            Log.warn "rgm=%A -> funMat=%A" reallyGoodMatches.Length resultList.Count
            

            resultList.ToArray(), lines.ToArray()
            //reallyGoodMatches, reallyGoodMatches |> Array.map (fst >> getLine)
        else
            Log.warn "found not enough matches (only %A)" reallyGoodMatches.Length
            [||], [||]


    let estimateHomography (config : MatchingConfig) (l : Feature[]) (r : Feature[]) =
        let reallyGoodMatches = matchCandidates config.threshold l r

        if reallyGoodMatches.Length >= 10 then
            let lPoints = reallyGoodMatches |> Array.map (fun (li,_) -> l.[li].ndc)
            let rPoints = reallyGoodMatches |> Array.map (fun (_,ri) -> r.[ri].ndc)
            let hom, used = homography lPoints rPoints
            let cnt = used.Count

            if cnt >= 8 then
                Log.warn "found %d matches" cnt
                Some(hom, cnt)
            else 
                Log.warn "found no matches"
                None

        else
            Log.warn "found no matches"
            None


    [<CustomEquality; NoComparison>]
    type FeatureNode =
        {
            image           : int
            featureIndex    : int
            feature         : Feature
            corresponding   : Dict<int, HashSet<FeatureNode>>
        }

        override x.GetHashCode() = x.featureIndex
        override x.Equals o =
            match o with
                | :? FeatureNode as o -> x.featureIndex = o.featureIndex
                | _ -> false

        member x.Add(image : int, f : FeatureNode) =
            let set = x.corresponding.GetOrCreate(image, fun _ -> HashSet())
            set.Add f |> ignore

        member x.Remove(image : int, f : FeatureNode) =
            match x.corresponding.TryGetValue image with
                | (true, set) ->
                    if set.Remove f then
                        if set.Count = 0 then
                            x.corresponding.Remove image |> ignore
                | _ -> ()
        member x.Clear(image : int) =
            x.corresponding.Remove image |> ignore

    type FeatureGraph =
        {
            images : PixImage<byte>[]
            data : Feature[][]
            features : Dict<int, HashSet<FeatureNode>>
            edges : Edge<(int*int)[]>[]
        }

    let private rgbaFromHsva (h : float, s : float, v : float) =
        let c = v * s
        let hh = (h * 6.0) % 6.0
        let x = c * (1.0 - abs(hh % 2.0 - 1.0))
        let a = 1.0
        if (0.0 <= h && h < 1.0) then
            C4f(c, x, 0.0, a)
        else if (1.0 <= h && h < 2.0) then
            C4f(x, c, 0.0, a)
        else if (2.0 <= h && h < 3.0) then
            C4f(0.0, c, x, a)
        else if (3.0 <= h && h < 4.0) then
            C4f(0.0, x, c, a)
        else if (4.0 <= h && h < 5.0) then
            C4f(x, 0.0, c, a)
        else if (5.0 <= h && h < 6.0) then
            C4f(c, 0.0, x, a)
        else 
            C4f(0.0, 0.0, 0.0, a)

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module FeatureGraph =
        
        let build (matcher : Feature[] -> Feature[] -> (int*int)[]) (images : PixImage<byte>[]) (data : Feature[][]) =
            let nodes = Dict<int * int, FeatureNode>()
            let features = Dict()

            let getNode (image : int) (featureIndex : int) =
                nodes.GetOrCreate((image, featureIndex), fun (image, featureIndex) ->
                    let result =
                        {
                            image           = image
                            featureIndex    = featureIndex
                            feature         = data.[image].[featureIndex]
                            corresponding   = Dict()
                        }

                    let set = features.GetOrCreate(image, fun _ -> HashSet())
                    set.Add result |> ignore

                    result
                )

            let edges = List<Edge<array<int * int>>>()

            let allPairs = 
                [|
                    for lImg in 0 .. data.Length - 1  do
                        for rImg in lImg + 1 .. data.Length - 1 do
                            yield lImg, rImg
                |]

            let edges = 
                allPairs |> Array.choose (fun (lImg, rImg) ->
                    let lFeatures = data.[lImg]
                    let rFeatures = data.[rImg]

                    Log.line "Now matching %d/%d" lImg rImg
                    let matches = matcher lFeatures rFeatures
                    if matches.Length > 0 then
                        Log.line "matched %d/%d: %d" lImg rImg matches.Length
                        Some { i0 = lImg; i1 = rImg; weight = matches }
                    else
                        None
                )
                    
            let spanningTree = 
                edges
                    |> Array.toList
                    |> Graph.ofEdges
                    |> Graph.minimumSpanningTree (fun l r -> compare r.Length l.Length)

            Log.start "using"
            for e in spanningTree do
                let lImg = e.i0
                let rImg = e.i1
                Log.line "(%d, %d): %d" lImg rImg e.weight.Length
                for (lFeature, rFeature) in e.weight do
                    let lNode = getNode lImg lFeature
                    let rNode = getNode rImg rFeature
                    lNode.Add(rImg, rNode)
                    rNode.Add(lImg, lNode)
            Log.stop()

            { data = data; images = images; features = features; edges = spanningTree }


        let toBundlerInput (g : FeatureGraph) (minTrackLength : int) =

            let features = g.features
            
            let takeNode() =
                let (KeyValue(image, fs)) = features |> Seq.head
                let f = fs |> Seq.head
                fs.Remove f |> ignore
                if fs.Count = 0 then features.Remove image |> ignore
                f


            let paths = List<Set<int> * array<FeatureNode>>()
            while features.Count > 0 do
                let list = List<FeatureNode>()
                let start = takeNode()
                list.Add start

                let rec traverse (visitedImages : Set<int>) (path : list<FeatureNode>) (start : FeatureNode) =
//                    let mutable bad = false
//                    for (dstImg, dstNode) in start.corresponding |> Dict.toArray do
//                        // remove all connections that go to an image that was already
//                        // visited and represent a non-chosen feature
//                        if Set.contains dstImg visitedImages then
//                            start.Clear dstImg
//                            bad <- true

                    let next = 
                        start.corresponding |> Dict.toSeq |> Seq.tryPick (fun (ii,other) -> 
                           if other.Count >= 1 then Some (ii, Seq.head other)
                           else None
                        )


                    match next with
                        | Some(dstImg, dstNode) ->
                            // kill the connection
                            dstNode.Remove(start.image, start)
                            start.Remove(dstImg, dstNode)

                            traverse (Set.add dstImg visitedImages) (dstNode :: path) dstNode

                        | None ->
                            visitedImages, path

                let usedImages, path = traverse (Set.singleton start.image) [start] start
                let path = List.toArray path

                if path.Length >= minTrackLength then
                    paths.Add(usedImages, path)
                    let str = path |> Array.map (fun f -> sprintf "(%d, %d)" f.image f.featureIndex) |> String.concat " -> "
                    Log.warn "found path: %s" str


            let measurements = Array.create g.data.Length Map.empty
            let ff = Array.create g.data.Length Map.empty

            let targetCounts = Array.create g.data.Length 40

            paths.QuickSortDescending(fun (_,p) -> p.Length)

            for pi in 0 .. paths.Count - 1 do
                let (used, path) = paths.[pi]
                let usePath = used |> Set.exists (fun u -> targetCounts.[u] > 0)
                if usePath then
                    for node in path do
                        let image = node.image
                        let pos = node.feature.ndc
                        measurements.[image] <- Map.add pi pos measurements.[image]
                        ff.[image] <- Map.add pi node.feature ff.[image]

                    for u in used do
                        targetCounts.[u] <- targetCounts.[u] - 1 
                        
                        


            let rand = RandomSystem()
            let colors = paths |> Seq.mapi (fun i _ -> rgbaFromHsva(6.0 * float i / float paths.Count, 1.0, 1.0).ToC4b()) |> Seq.toArray

            for i in 0 .. g.images.Length - 1 do
                let file = g.images.[i].ToPixImage<byte>()
                let ff = ff.[i]
                
                for (pi,f) in Map.toSeq ff do
                    let p = f.ndc
                    let pp = V2i (V2d.Half + V2d(0.5 * p.X + 0.5, 0.5 - 0.5 * p.Y) * V2d file.Size)
                    let size = clamp 15 30 (ceil (f.size) |> int)

                    file.GetMatrix<C4b>().SetCross(pp, size, colors.[pi])
                    file.GetMatrix<C4b>().SetCircle(pp, size, colors.[pi])
                    

                let path = sprintf @"C:\blub\yolo\out\mtl%A" minTrackLength

                if Directory.Exists path |> not then Directory.CreateDirectory path |> ignore

                file.SaveAsImage (Path.combine [path; sprintf "image%d.jpg" i])


            { measurements = measurements |> Array.mapi ( fun i x -> i,x) |> Map.ofArray }



//    let toBundlerInput (config : MatchingConfig) (images : PixImage<byte>[]) (data : Feature[][]) =
//        let graph = FeatureGraph.build config images data
//        FeatureGraph.toBundlerInput graph


    let iterate (g : FeatureGraph) (debugPath : Option<string>) =
        let debugOutput (measureMap : Map<int,Map<int,V2d>>) cam (run : Option<int>) =
            match debugPath with
            | None -> ()
            | Some path ->
                
                let rand = RandomSystem()
                let colors = [1.0..2000.0] |> Seq.mapi (fun i _ -> rgbaFromHsva(6.0 * float i / float [1.0..2000.0].Length, 1.0, 1.0).ToC4b()) |> Seq.toArray

                let outputCam idx =
                    let file = g.images.[idx]
                    let map = measureMap.[idx]
                    let mutable fNumber = 0
                    for (fi,v2) in map |> Map.toArray do
                        let f = g.data.[idx].[fi]
                        let p = f.ndc
                        let pp = V2i (V2d.Half + V2d(0.5 * p.X + 0.5, 0.5 - 0.5 * p.Y) * V2d file.Size)
                        let size = clamp 15 30 (ceil (f.size) |> int)

                        file.GetMatrix<C4b>().SetCross(pp, size, colors.[fNumber])
                        file.GetMatrix<C4b>().SetCircle(pp, size, colors.[fNumber])
                        fNumber <- fNumber+1
                        
                    let path = sprintf @"%s\t" path
                    //let path = sprintf @"%s\%A" path g.config.distanceThreshold

                    if Directory.Exists path |> not then Directory.CreateDirectory path |> ignore

                    let fn =    
                        match run with
                        | None ->     sprintf "image%d.jpg" idx
                        | Some run -> sprintf "%A_image%d.jpg" run idx

                    file.SaveAsImage (Path.combine [path; fn])

                outputCam cam

        let getMeasurement cam idx =
            g.data.[cam].[idx].ndc

        //assumes these edges are sorted most->least matches
        let processNewEdge (b : Edge<(int * int)[]>) =
            let cam0 = b.i0
            let cam1 = b.i1

            let bBundlerInput = 

                let measurements =
                    [|
                        cam0,Map.ofArray [| for (p0,_) in b.weight do yield p0, getMeasurement cam0 p0 |]
                        cam1,Map.ofArray [| for (_,p1) in b.weight do yield p1, getMeasurement cam1 p1 |]
                    |] |> Map.ofArray

                { measurements = measurements }

            let preprocessed = 
                bBundlerInput 
                    |> BundlerInput.preprocess

            let measureMap =
                [| cam0,preprocessed.measurements.[cam0] ; cam1,preprocessed.measurements.[cam1] |] |> Map.ofArray

            debugOutput measureMap cam0 (Some 0)
            debugOutput measureMap cam1 (Some 0)

            let solution = 
                preprocessed
                |> BundlerInput.toProblem
                |> Bundler.solve

            let involvedCameras = [|cam0; cam1|]

            solution,involvedCameras

        let mutable runs = 1
        let addEdgeToExisting (solution : BundlerSolution) (solvedCameras : int[]) (b : Edge<(int*int)[]>) =
            let ((newCam,newWeights),(knownCam,knownWeights)) =
                if solvedCameras |> Array.contains b.i0 then 
                    (b.i1, b.weight |> Array.map snd),(b.i0, b.weight |> Array.map fst)
                else 
                    (b.i0, b.weight |> Array.map fst),(b.i1, b.weight |> Array.map snd)

            let (bBundlerInput, measureMap) = 

                let measurements =
                    [|
                        knownCam,Map.ofArray [| for p in knownWeights    do yield p, getMeasurement knownCam p |]
                        newCam,Map.ofArray [| for p in newWeights      do yield p, getMeasurement newCam   p |]
                    |] |> Map.ofArray

                let measureMap =
                    [| knownCam,measurements.[knownCam] ; newCam,measurements.[newCam] |] |> Map.ofArray

                { measurements = measurements }, measureMap

            //preprocess to make measurement set consistent for these two cameras
            let preprocessed = 
                bBundlerInput 
                    |> BundlerInput.preprocess
                    
            debugOutput measureMap knownCam (Some runs)
            debugOutput measureMap newCam   (Some runs)
            runs <- runs+1

            let solution = 
                //after preprocessing, throw away the already known camera measurements and only keep the unknown one
                { measurements = [| newCam, preprocessed.measurements.[newCam] |] |> Map.ofArray }
                |> BundlerInput.toProblem
                |> Bundler.solveTowardsKnown solution   //this combines the old and new solutions. all measurements are in there once



            let involvedCameras =   [| 
                                        yield! solvedCameras
                                        yield newCam
                                    |]

            solution,involvedCameras

        match g.edges.Length > 0 with
            | true ->
                Log.line "Processing first edge using cams (%A,%A)" g.edges.[0].i0 g.edges.[0].i1
                let (baseSolution,involvedCams) = processNewEdge g.edges.[0]
                
                let mutable solution = (baseSolution,involvedCams)
                for i in 1..g.edges.Length-1 do
                    let candidate = g.edges.[i]
                    Log.line "Adding new edge KNOWN=%A to NEW=%A" candidate.i0 candidate.i1

                    let (sol, cams) = addEdgeToExisting (solution |> fst) (solution |> snd) candidate

                    solution <- (sol,cams)

                    let err = sol |> BundlerSolution.errorMetrics
                    Log.start "error metrics"
                    Log.line "cost:    %A" err.cost
                    Log.line "average: %.4f%%" (100.0 * err.average)
                    Log.line "stdev:   %.4f%%" (100.0 * err.stdev)
                    Log.line "min:     %.4f%%" (100.0 * err.min)
                    Log.line "max:     %.4f%%" (100.0 * err.max)
                    Log.stop()

                Some (solution |> fst)
            | false -> 
                Log.warn "Match graph is empty. no solutions."
                None

//    let solveIteratively (config : MatchingConfig) (outputPath : Option<String>) (images : PixImage<byte>[]) (data : Feature[][]) =
//        let graph = FeatureGraph.build config images data
//        iterate graph outputPath


