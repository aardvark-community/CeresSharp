namespace Aardvark.Reconstruction

open System
open Aardvark.Base



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

            member x.ViewProjTrafo (far : float) =
                let frustum = { left = -1.0; right = 1.0; top = 1.0; bottom = -1.0; near = 1.0; far = far }

                Trafo3d.Translation(-x.Position) *
                AngleAxis.Trafo(x.AngleAxis) *
                Frustum.projTrafo frustum

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


[<Struct; CustomComparison; CustomEquality>]
type CameraId(id : int) =
    static let mutable current = 0
    static member New = CameraId(System.Threading.Interlocked.Increment(&current))

    member x.Id = id

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
                

