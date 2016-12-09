namespace Aardvark.Ceres

open System
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
        res

type Feature =
    {
        ndc         : V2d
        angle       : float
        size        : float
        response    : float
        descriptor  : FeatureDescriptor
    }

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
                angle = float f.Angle
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

        //features.QuickSortDescending(fun f -> f.Response)

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
        minTrackLength : int
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Feature =
    open System.Collections.Generic
    open OpenCvSharp
    open OpenCvSharp.XFeatures2D
    open Microsoft.FSharp.NativeInterop
    
    let private homography (l : V2d[]) (r : V2d[]) =

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


        use res = Cv2.FindHomography(InputArray.op_Implicit lMat, InputArray.op_Implicit rMat, HomographyMethods.Ransac, 0.001)
        M33d(
            res.Get(0, 0), res.Get(0, 1), res.Get(0, 2),
            res.Get(1, 0), res.Get(1, 1), res.Get(1, 2),
            res.Get(2, 0), res.Get(2, 1), res.Get(2, 2)
        )


    let matches (config : MatchingConfig) (l : Feature[]) (r : Feature[]) =
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

                if float m0.Distance < config.threshold * float m1.Distance then
                    Some (m0.TrainIdx, m0.QueryIdx)
                else
                    None

            ) |> Seq.toArray


        if reallyGoodMatches.Length >= 12 then
            Log.warn "found %d anchor matches" reallyGoodMatches.Length

            let lPoints = reallyGoodMatches |> Array.map (fun (li,_) -> l.[li].ndc)
            let rPoints = reallyGoodMatches |> Array.map (fun (_,ri) -> r.[ri].ndc)
            let hom = homography lPoints rPoints

            let final = 
                matches |> Seq.choose (fun m ->
            
                    let m0 = m.[0]
                    let m1 = m.[1]
                
                    let lPoint = l.[m0.TrainIdx].ndc
                    let rPoint = r.[m0.QueryIdx].ndc
                    let rExpected = hom.TransformPosProj(lPoint)
                    let distance = Vec.length (rPoint - rExpected)
                    if distance < 0.02 then
                        Some (m0.TrainIdx, m0.QueryIdx)
                    else
                        None

                ) |> Seq.toArray

            if final.Length >= 32 then

                Log.warn "made %d out of it" final.Length
                Array.toList final
            else 
                Log.warn "found no matches"
                []

        else
            Log.warn "found no matches"
            []


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
            files : string[]
            config : MatchingConfig
            data : Feature[][]
            features : Dict<int, HashSet<FeatureNode>>
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module FeatureGraph =
        let build (config : MatchingConfig) (files : string[]) (data : Feature[][]) =
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

            for lImg in 0 .. data.Length - 1  do
                for rImg in lImg + 1 .. data.Length - 1 do
                    let lFeatures = data.[lImg]
                    let rFeatures = data.[rImg]
                    let matches = matches config lFeatures rFeatures

                    for (lFeature, rFeature) in matches do
                        let lNode = getNode lImg lFeature
                        let rNode = getNode rImg rFeature
                        lNode.Add(rImg, rNode)
                        rNode.Add(lImg, lNode)

                       


                    ()

            { data = data; config = config; files = files; features = features }



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

        let toBundlerInput (g : FeatureGraph) =
            let features = g.features
            
            let takeNode() =
                let (KeyValue(image, fs)) = features |> Seq.head
                let f = fs |> Seq.head
                fs.Remove f |> ignore
                if fs.Count = 0 then features.Remove image |> ignore
                f


            let paths = List<list<FeatureNode>>()
            while features.Count > 0 do
                let list = List<FeatureNode>()
                let start = takeNode()
                list.Add start

                let rec traverse (visitedImages : Set<int>) (path : list<FeatureNode>) (start : FeatureNode) =
                    let mutable bad = false
                    for (dstImg, dstNode) in start.corresponding |> Dict.toArray do
                        // remove all connections that go to an image that was already
                        // visited and represent a non-chosen feature
                        if Set.contains dstImg visitedImages then
                            start.Clear dstImg
                            bad <- true

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
                            if bad then [] 
                            else path

                let path = traverse (Set.singleton start.image) [start] start

                if List.length path >= g.config.minTrackLength then
                    paths.Add path
                    let str = path |> List.map (fun f -> sprintf "(%d, %d)" f.image f.featureIndex) |> String.concat " -> "
                    Log.warn "found path: %s" str


            let measurements = Array.create g.data.Length Map.empty
            let ff = Array.create g.data.Length Map.empty


            for pi in 0 .. paths.Count - 1 do
                for node in paths.[pi] do
                    let image = node.image
                    let pos = node.feature.ndc
                    measurements.[image] <- Map.add pi pos measurements.[image]
                    ff.[image] <- Map.add pi node.feature ff.[image]


            let rand = RandomSystem()
            let colors = paths |> Seq.mapi (fun i _ -> rgbaFromHsva(6.0 * float i / float paths.Count, 1.0, 1.0).ToC4b()) |> Seq.toArray

            for i in 0 .. g.files.Length - 1 do
                let file = PixImage.Create(g.files.[i]).ToPixImage<byte>()
                let ff = ff.[i]
                
                for (pi,f) in Map.toSeq ff do
                    let p = f.ndc
                    let pp = V2i (V2d.Half + V2d(0.5 * p.X + 0.5, 0.5 - 0.5 * p.Y) * V2d file.Size)
                    let size = (ceil (f.size) |> int)

                    file.GetMatrix<C4b>().SetCross(pp, size, colors.[pi])
                    file.GetMatrix<C4b>().SetCircle(pp, size, colors.[pi])
                    

                file.SaveAsImage (Path.combine [@"C:\Users\schorsch\Desktop\test"; sprintf "image%d.jpg" i])


            { measurements = measurements }


    let toBundlerInput (config : MatchingConfig) (files : string[]) (data : Feature[][]) =
        let graph = FeatureGraph.build config files data
        FeatureGraph.toBundlerInput graph



