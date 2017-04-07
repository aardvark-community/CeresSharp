namespace Aardvark.Ceres

open System
open System.IO
open Aardvark.Base

#nowarn "9"

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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix);AutoOpen>]
module Feature =
    open System.Collections.Generic
    open OpenCvSharp
    open OpenCvSharp.XFeatures2D
    open Microsoft.FSharp.NativeInterop
    open CeresSharp

    let bruteforceMatch (t : float) (l : Feature[]) (r : Feature[]) =
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

    [<AutoOpen>]
    module private Utils =
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
    
    
    let rgbaFromHsva (h : float, s : float, v : float) =
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
    
    type FeatureGraphInput =
        {
            matcher : Feature[][] -> int -> int -> (int*int)[]
            images  : PixImage<byte>[]
            data    : Feature[][]
        }
    
    type ModelSer =
        {
            ms : HashSet<int32*int32>[][]
            fs : Feature[][]
        }
        
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ModelSer =

        let toGraphInput (ms : ModelSer) =
            {
                images = Array.init (ms.fs |> Array.length) ( fun _ -> PixImage<byte>(Col.Format.RGBA, V2i(400,400)) )
                data   = ms.fs
                matcher = (fun _ (lcam : int) (rcam : int) -> ms.ms.[lcam].[rcam] |> HashSet.toArray)
            }
            
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module FeatureGraph =
        
        let build (input : FeatureGraphInput) =
            let nodes = Dict<int * int, FeatureNode>()
            let features = Dict()

            let getNode (image : int) (featureIndex : int) =
                nodes.GetOrCreate((image, featureIndex), fun (image, featureIndex) ->
                    let result =
                        {
                            image           = image
                            featureIndex    = featureIndex
                            feature         = input.data.[image].[featureIndex]
                            corresponding   = Dict()
                        }

                    let set = features.GetOrCreate(image, fun _ -> HashSet())
                    set.Add result |> ignore

                    result
                )

            let edges = List<Edge<array<int * int>>>()

            let allPairs = 
                [|
                    for lImg in 0 .. input.data.Length - 1  do
                        for rImg in lImg + 1 .. input.data.Length - 1 do
                            yield lImg, rImg
                |]

            let edges = 
                allPairs |> Array.choose (fun (lImg, rImg) ->
                    Log.line "Now matching %d/%d" lImg rImg
                    let matches = input.matcher input.data lImg rImg
                    if matches.Length > 0 then
                        Log.line "matched %d/%d: %d" lImg rImg matches.Length
                        Some { i0 = lImg; i1 = rImg; weight = matches }
                    else
                        None
                )
                    
            let spanningTree, spanningEdges = 
                edges
                    |> Array.toList
                    |> Graph.ofEdges
                    |> Graph.minimumSpanningTree (fun l r -> compare r.Length l.Length)

            Log.start "using"
            for e in spanningEdges do
                let lImg = e.i0
                let rImg = e.i1
                Log.line "(%d, %d): %d" lImg rImg e.weight.Length
                for (lFeature, rFeature) in e.weight do
                    let lNode = getNode lImg lFeature
                    let rNode = getNode rImg rFeature
                    lNode.Add(rImg, rNode)
                    rNode.Add(lImg, lNode)
            Log.stop()

            { data = input.data; images = input.images; features = features; edges = spanningEdges; tree = spanningTree }

        let toBundlerInputSiegfried (g : FeatureGraph) (minTrackLength : int) =
            
            let used = Dict<int*FeatureNode, bool>()
            let setUsed ci (f : FeatureNode) = used |> Dict.set (ci,f) true

            let neighboursOf (f : FeatureNode) = (f.corresponding |> Dict.toList)

            let rec traverseNeighboursAndFindPath (path : list<FeatureNode>) (neighbours : list<int*HashSet<FeatureNode>>) =
                match neighbours with                                   //check corresponding neighbouring images
                | [] -> path                                            //no more neighbours, we're finished
                | (corImg, cor)::remainingNeighbours ->                 //has neighbours, take the first one

                    let rec findUnusedMatch (cor : list<FeatureNode>) =
                        match cor with
                        | [] -> None
                        | corFtr::remainingFtrs ->
                            if used.[corImg,corFtr] then
                                findUnusedMatch remainingFtrs
                            else
                                setUsed corImg corFtr
                                Some corFtr

                    match findUnusedMatch (cor |> HashSet.toList) with  //check if this neighbour has an unused feature match
                    | None ->                                           //doesn't have unused feature, try the next neighbour
                        traverseNeighboursAndFindPath (path) remainingNeighbours
                    | Some m ->                                                     //has an unused feature. this is part of the result.
                        let subPath = traverseNeighboursAndFindPath [] (neighboursOf m)                                 //first, collect all subresults from the neighbours
                        traverseNeighboursAndFindPath ([[m]; subPath; path] |> List.concat) remainingNeighbours         //then, append them to the current and continue with next neighbour
                        
            let result = List<list<FeatureNode>>()

            for KeyValue(ci,features) in g.features do for f in features do used |> Dict.add (ci,f) false
            
            let mutable tooShort = 0
            Log.line "Paths (img:ftr): "
            for KeyValue(ci,features) in g.features do
                
                for f in features do
                                    
                    if not used.[ci,f] then
                        setUsed ci f
                        let path = traverseNeighboursAndFindPath [f] (neighboursOf f)
                        if path |> List.length >= minTrackLength then
                            
                            //status string print
                            [ "Path: "; sprintf "start=%A " ci; [ for p in path do yield (sprintf " (%A:%A) " p.image p.featureIndex) ] |> String.concat "->"  ] |> String.concat " " |> Log.line "%s"

                            result.Add path
                        else
                            tooShort <- tooShort+1
                    else
                        ()
            
            let result = result |> Seq.toList

            let debugoutput1() =
                Log.line "Found %A paths within %A FeatureNodes (%A imgs)."     result.Length    used.Count  g.images.Length
                Log.line "MinTrackLength:\t\t%A"                                minTrackLength
                Log.line "Too short tracks:\t\t%A"                              tooShort
                Log.line "Used FeatureNodes:\t\t%A"                             (used |> Dict.toList |> List.sumBy ( fun (_,b) -> if b then 1 else 0))
                Log.line "Unused FeatureNodes:\t%A"                             (used |> Dict.toList |> List.sumBy ( fun (_,b) -> if b then 0 else 1))
            
                let resultByCount =
                    result  |> List.fold (fun (counts:Dict<int,int>) (path) -> 
                                            let key = path.Length
                                            let ct = counts.GetOrCreate(key, fun _ -> 0)
                                            counts |> Dict.set key (ct + 1) 
                                            counts) (Dict<int,int>())
            
                Log.line "Number of valid tracks:"
                for KeyValue(len,ct) in resultByCount do
                    Log.line "length: %A  count: %A" len ct
            debugoutput1()
            

            let measurements = Dictionary<_,_>()
            for ci in 0 .. g.images.Length - 1 do
                measurements.Add(ci,Dictionary<_,_>())

                for fn in result |> List.map ( fun fs -> fs |> List.filter ( fun f -> f.image = ci ) ) |> List.concat do
                    let pi = fn.featureIndex
                    let p = fn.feature.ndc
                    measurements.[ci].Add(pi,p)
                    
            let measurements =
                [|
                    for KeyValue(ci, ms) in measurements do
                        if ms.Count < 8 then
                            Log.warn "ms count less than 8 (only %d) bye bye camera %d" ms.Count ci
                        else
                            yield ci, ms
                            
                |] |> Dictionary.ofArray

            let result = 
                result |> List.choose ( fun track -> 
                    let filtered = track |> List.filter ( fun n -> measurements.ContainsKey n.image )
                    if List.length filtered < minTrackLength then
                        None
                    else
                        Some filtered
                )

            let debugoutput2() =
                let colors = result |> Seq.concat |> Seq.mapi (fun i _ -> rgbaFromHsva(6.0 * float i / float result.Length, 1.0, 1.0).ToC4b()) |> Seq.toArray
                for i in 0 .. g.images.Length - 1 do
                    let file = g.images.[i].ToPixImage<byte>()
                    for fn in result |> List.map ( fun fs -> fs |> List.filter ( fun f -> f.image = i ) ) |> List.concat do
                        let f = fn.feature
                        let pi = fn.featureIndex
                        let p = f.ndc

                        let pp = V2i (V2d.Half + V2d(0.5 * p.X + 0.5, 0.5 - 0.5 * p.Y) * V2d file.Size)
                        let size = clamp 5 10 (ceil (f.size / 3.0) |> int)
                    
                        file.GetMatrix<C4b>().SetCross(pp, size, colors.[pi])
                        file.GetMatrix<C4b>().SetCircle(pp, size, colors.[pi])
                    
                    let path = sprintf @"D:\file\pix\mtl%A" minTrackLength

                    if Directory.Exists path |> not then Directory.CreateDirectory path |> ignore

                    file.SaveAsImage (Path.combine [path; sprintf "image%d.jpg" i])
            debugoutput2()

            let flat = measurements |> Seq.map ( fun kvp -> int kvp.Key, kvp.Value |> Seq.map ( fun ikvp -> ikvp.Key, ikvp.Value ) |> Map.ofSeq ) |> Map.ofSeq
            
            let tracks = result |> List.map ( fun nodes -> nodes |> List.map ( fun fn -> fn.image, fn.featureIndex ) |> List.sortBy fst |> List.toArray ) |> List.toArray

            { graph = g; measurements = flat; tracks = tracks }
                        