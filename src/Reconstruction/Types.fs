namespace Aardvark.Reconstruction

open System
open Aardvark.Base
open System.Collections.Generic

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
            
    let o (rotation : float) =
        V4d(0.0, sin(rotation), 0.0, cos(rotation))

[<Struct; CustomComparison; CustomEquality>]
type CameraId(id : int) =
    static let mutable current = 0
    static member New = CameraId(System.Threading.Interlocked.Increment(&current))

    member x.Id = id

    override x.ToString() = sprintf "C%A" id
    override x.GetHashCode() = id
    override x.Equals o =
        match o with
            | :? CameraId as o -> id = o.Id
            | _ -> false

    interface System.IComparable with
        member x.CompareTo o =
            match o with
                | :? CameraId as o -> compare id o.Id
                | _ -> failwith ""

[<Struct; CustomComparison; CustomEquality>]
type TrackId(id : int) =
    static let mutable current = 0
    static member New = TrackId(System.Threading.Interlocked.Increment(&current))

    member x.Id = id

    override x.ToString() = sprintf "T%A" id
    override x.GetHashCode() = id
    override x.Equals o =
        match o with
            | :? TrackId as o -> id = o.Id
            | _ -> false

    interface System.IComparable with
        member x.CompareTo o =
            match o with
                | :? TrackId as o -> compare id o.Id
                | _ -> failwith ""

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
   
type Feature =
    {
        ndc         : V2d
        angle       : float
        size        : float
        response    : float
        descriptor  : FeatureDescriptor
    }

[<CustomEquality; NoComparison>]
type FeatureNode =
    {
        feature         : Feature
        corresponding   : Dict<CameraId, HashSet<FeatureNode>>
    }
    override x.GetHashCode() = x.feature.GetHashCode()
    override x.Equals o =
        match o with
            | :? FeatureNode as o -> x.feature = o.feature
            | _ -> false
    

    member x.Add(image : CameraId, f : FeatureNode) =
        let set = x.corresponding.GetOrCreate(image, fun _ -> HashSet())
        set.Add f |> ignore

    static member ofFeature ( ftr : Feature ) =
        if ftr.ndc.X > 1.0 || ftr.ndc.Y > 1.0 || ftr.ndc.X < -1.0 || ftr.ndc.Y < -1.0 then Log.error "[Features] Found a feature that is outside of image boundaries: ndc=%A" ftr.ndc
        {
            feature = ftr //{ ftr with ndc = ftr.ndc.YX * (V2d(1.0,-1.0)) }
            corresponding = Dict()
        }

[<Struct>]
type Match2d(pos : V2d, vel : V2d, o : V4d) =
    member x.LengthSquared =
        pos.LengthSquared + vel.LengthSquared + o.LengthSquared

    member x.Length = 
        sqrt x.LengthSquared

    member x.Pos = pos
    member x.Vel = vel
    member x.O = o

    static member (-)(l : Match2d,r : Match2d) =
        Match2d( l.Pos-r.Pos, l.Vel-r.Vel, l.O-r.O )

    static member Dot(l : Match2d,r : Match2d) =
        Vec.dot l.Pos r.Pos + Vec.dot l.Vel r.Vel + Vec.dot l.O r.O

    static member ofFeatures ( l : FeatureNode, r : FeatureNode ) =
        Match2d( l.feature.ndc, r.feature.ndc - l.feature.ndc, o (r.feature.angle - l.feature.angle) ) 


[<AutoOpen>]
module Geometry =

    type Camera3d =
        struct
            val mutable public Position         : V3d
            val mutable public AngleAxis        : V3d

            member x.ProjectWithDepth(p : V3d) =
                let view = AngleAxis.RotatePoint(x.AngleAxis, p - x.Position)
                let ndc = view.XY / view.Z

                ndc, view.Z

            member x.Project(p : V3d) =
                let view = AngleAxis.RotatePoint(x.AngleAxis, p - x.Position)
                let ndc = view.XY / view.Z

                ndc

            member x.ViewAndProjTrafo (far : float) =
                let frustum = { left = -1.0; right = 1.0; top = 1.0; bottom = -1.0; near = 1.0; far = far }

                Trafo3d.Translation(-x.Position) *
                AngleAxis.Trafo(x.AngleAxis),
                Frustum.projTrafo frustum

            member x.ViewProjTrafo (far : float) =
                let (v,p) = x.ViewAndProjTrafo far
                v * p

            member x.Transformed (t : Trafo3d) =
                let up = AngleAxis.RotatePoint(-x.AngleAxis, V3d.OIO)
                let fw = AngleAxis.RotatePoint(-x.AngleAxis, -V3d.OOI)
                let p =  x.Position         |> t.Forward.TransformPosProj
                let pu = x.Position + up    |> t.Forward.TransformPosProj
                let pf = x.Position + fw    |> t.Forward.TransformPosProj

                let u = pu - p
                let s = Vec.length u

                let mutable res = Camera3d.LookAt(p, pf, (pf - p).Length / s, u / s)
                res

            member x.Unproject (pt : V2d) (depth : float) =
                let ndc = V2d(pt.X, pt.Y)
                let dir = AngleAxis.RotatePoint(-x.AngleAxis, V3d(ndc, 1.0) * depth)
                x.Position + dir

            member x.GetRay (pt : V2d) =
                let point = x.Unproject pt 1.0
                let dir = (point - x.Position) |> Vec.normalize
                Ray3d(x.Position, dir)

            static member LookAt(eye : V3d, center : V3d, f : float, sky : V3d) : Camera3d =
                let forward = Vec.normalize (center - eye)
                let left = Vec.cross sky forward |> Vec.normalize
                let up = Vec.cross forward left |> Vec.normalize

                let rot = M44d.FromBasis(-left, up, -forward, V3d.Zero).UpperLeftM33() |> Rot3d.FromM33d
                let mutable axis = V3d.Zero
                let mutable angle = 0.0
                rot.ToAxisAngle(&axis, &angle)
                let aa = axis * -angle
                let res = Camera3d(eye, aa)

                let test = res.Project center
                res

            static member Delta(src : Camera3d, dst : Camera3d) =
                let src = src.ViewProjTrafo 100.0
                let dst = dst.ViewProjTrafo 100.0
                src * dst.Inverse

            new(pos, angleAxis) = 
                { Position = pos; AngleAxis = angleAxis}
        end

type Camera3s(pos : V3s, aa : V3s) =
    member x.Position = pos
    member x.AngleAxis = aa

    member x.Project(p : V3s) =
        let view = AngleAxis.RotatePoint(x.AngleAxis, p - x.Position)
        let ndc = view.XY / view.Z

        ndc

    member x.ProjectWithDepth(p : V3s) =
        let view = AngleAxis.RotatePoint(x.AngleAxis, p - x.Position)
        let ndc = view.XY / view.Z

        ndc, view.Z

    static member Read(offset : int, v : Camera3d) =
        let p   = V3s(scalar.Variable(offset + 0, v.Position.X), scalar.Variable(offset + 1, v.Position.Y), scalar.Variable(offset + 2, v.Position.Z))
        let aa  = V3s(scalar.Variable(offset + 3, v.AngleAxis.X), scalar.Variable(offset + 4, v.AngleAxis.Y), scalar.Variable(offset + 5, v.AngleAxis.Z))
        Camera3s(p, aa)

    static member Value(cam : Camera3s) =
        Camera3d(cam.Position.Value, cam.AngleAxis.Value)

module V3s =
    
    let getProjectedBy (cam : Camera3d) (p : V3s) =   
        let view = AngleAxis.RotatePoint(cam.AngleAxis, p - cam.Position)
        let ndc = view.XY / view.Z

        ndc

    let getProjectedByWithDepth (cam : Camera3d) (p : V3s) =   
        let view = AngleAxis.RotatePoint(cam.AngleAxis, p - cam.Position)
        let ndc = view.XY / view.Z

        ndc, view.Z
        
type BundlerProblem =
    {
        tracks : MapExt<TrackId, MapExt<CameraId, V2d>>
    }

type BundlerState =
    {
        cameras     : MapExt<CameraId, Camera3d>
        points      : MapExt<TrackId,  V3d>
    }

type Bundled = BundlerProblem * BundlerState
        
type Edges = RoseTree<int>*list<Edge<MapExt<TrackId,V2d*V2d>>>

module Edges =

    let ignore ((b,_) : Bundled * Edges) = b

    let get (res : ref<Edges>) ((b,e) : Bundled * Edges) =
        res := e
        b
    
    let toTracks (edges : list<Edge<MapExt<TrackId,V2d*V2d>>>) : MapExt<TrackId, MapExt<CameraId, V2d>> =
        
        let mutable res = MapExt.empty

        for e in edges do
            let l = CameraId(e.i0)
            let r = CameraId(e.i1)
            
            for KeyValue(tid, (lo, ro)) in e.weight do

                let addBoth (m : MapExt<_,_>) =
                    let mutable obs = m
                    obs <- obs.Add (l,lo)
                    obs <- obs.Add (r,ro)
                    obs

                res <- res |> MapExt.alter tid ( fun m ->
                            match m with
                            | None -> addBoth MapExt.empty |> Some
                            | Some obs -> addBoth obs |> Some
                       )
        res

module Tracks =

    let toMeasurements (tracks : MapExt<TrackId, MapExt<CameraId, V2d>>) : MapExt<CameraId, MapExt<TrackId, V2d>> =
        
        let mutable measures : MapExt<CameraId, MapExt<TrackId, V2d>> = MapExt.empty

        for (id, m) in MapExt.toSeq tracks do
            for (ci, measure) in MapExt.toSeq m do
                measures <- measures |> MapExt.alter ci ( fun old ->
                                let old = Option.defaultValue MapExt.empty old
                                Some (old |> MapExt.add id measure)
                            )
        measures
            
    let toEdgesAllWithAll (tracks : MapExt<TrackId, MapExt<CameraId, V2d>>) : list<Edge<MapExt<TrackId,V2d*V2d>>> =
        let edges = Dict<CameraId * CameraId, ref<MapExt<TrackId,V2d * V2d>>>()
            
        for KeyValue(tid, track) in tracks do
            for KeyValue(ci,pi) in track do
                for KeyValue(cj,pj) in track do
                    if ci < cj then
                        let m = edges.GetOrCreate((ci,cj), (fun _ -> ref MapExt.empty))
                        m := !m |> MapExt.add tid (pi,pj)
            
        edges   |> Dict.toList 
                |> List.map ( fun ((ci, cj),m) -> 
                    { i0 = ci.Id; i1 = cj.Id; weight = !m } 
                )

module Measurements =
    
    let toTracks ( ms : MapExt<CameraId, MapExt<TrackId, V2d>> ) : MapExt<TrackId, MapExt<CameraId, V2d>> =
        
        let mutable ts : MapExt<TrackId, MapExt<CameraId, V2d>> = MapExt.empty

        for (cid, os) in ms |> MapExt.toSeq do
            for (tid, m) in os |> MapExt.toSeq do 
                ts <- ts |> MapExt.alter tid ( fun old ->
                        let old = Option.defaultValue MapExt.empty old
                        Some (old |> MapExt.add cid m)
                      )

        ts