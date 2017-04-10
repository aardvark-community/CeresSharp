namespace Aardvark.Reconstruction

open System
open System.Threading
open System.Collections.Generic

open Aardvark.Base
open Aardvark.Base.Monads.State


[<AutoOpen>]
module Undefined =
    let undefined<'a> : 'a = failwith "undefined"

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






[<Struct; CustomComparison; CustomEquality>]
type CameraId private(id : int) =
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
type TrackId private(id : int) =
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


type Track = MapExt<CameraId, V2d>


type BundlerProblem =
    {
        tracks : MapExt<TrackId, MapExt<CameraId, V2d>>
    }
    
type BundlerState =
    {
        cameras     : MapExt<CameraId, Camera3d>
        points      : MapExt<TrackId, V3d>
    }

type Bundled<'a> = State<BundlerState, 'a>

module BundlerState =
    let setPoint (t : TrackId) (pt : V3d) =
        State.modify (fun s -> { s with points = MapExt.add t pt s.points })


module BundlerProblem =
    open Aardvark.Base.MultimethodTest

    let private minTracksPerCamera = 5

    let empty = { tracks = MapExt.empty }

    let removeCamera (id : CameraId) (sol : BundlerProblem) =
        { 
            tracks =
                sol.tracks |> MapExt.choose (fun k pt ->
                    let r = MapExt.remove id pt
                    if MapExt.count r > 0 then
                        Some r
                    else
                        None
                )
        }

    let removeTrack (id : TrackId) (sol : BundlerProblem) =
        { tracks = MapExt.remove id sol.tracks }

    let addMeasurement (tid : TrackId) (cam : CameraId) (position : V2d) (sol : BundlerProblem) =
        { tracks =
            sol.tracks
                |> MapExt.alter tid (fun pt ->
                    match pt with
                        | Some pt -> 
                            Some ( MapExt.add cam position pt )
                        | None ->
                            Some ( MapExt.ofList [cam, position] )
                )
        }


    //let filter (predicate : Point -> bool) (input : BundlerProblem) =
    //    { input with points = MapExt.filter ( fun _ p -> predicate p ) input.points }

    //let map (mapping : Point -> Point) (input : BundlerSolution) =
    //    { input with 
    //        points =
    //            input.points |> MapExt.choose (fun k pt ->
    //                let r = mapping pt
    //                if MapExt.count r.track > 1 then
    //                    Some r
    //                else
    //                    None
    //            )
    //    }

    //let filterTracks (predicate : V3d -> list<Camera3d * V2d> -> bool) (input : BundlerSolution) =
    //    input |> filter (fun pt ->
    //        pt.track 
    //            |> MapExt.toSeq 
    //            |> Seq.map (fun (ci, m) -> input.cameras.[ci], m) 
    //            |> Seq.toList
    //            |> predicate pt.position
    //    )

    //let removeOffscreenPoints (input : BundlerSolution) =
    //    input |> map ( fun p -> 
    //        let newTrack = 
    //            p.track 
    //                |> MapExt.filter ( fun ci _ -> 
    //                    let (_,d) = input.cameras.[ci].ProjectWithDepth p.position
    //                    d < 0.0
    //                )
    //        { p with track = newTrack }
    //    )

    //let removeRayOutliers (maxDist : float) (input : BundlerSolution) =
    //    input |> filterTracks ( fun pos camsAndTracks ->
    //        camsAndTracks |> List.forall ( fun (c, ndc) ->
    //            c.GetRay(ndc).GetMinimalDistanceTo(pos) <= maxDist
    //        )
    //    )


    //let rec 


    let ofMeasurements (measurements : MapExt<CameraId, MapExt<TrackId, V2d>>) : BundlerProblem =
        
        // get all the tracks
        let mutable tracks : MapExt<TrackId, MapExt<CameraId, V2d>> = MapExt.empty

        for (ci, measurements) in MapExt.toSeq measurements do
            for (id, m) in MapExt.toSeq measurements do
                tracks <- 
                    tracks |> MapExt.alter id (fun old ->
                        let old = Option.defaultValue MapExt.empty old
                        Some (old |> MapExt.add ci m)
                    )
                    
        { tracks = tracks }
   
    let miniCV (p : BundlerProblem) : Bundled<BundlerProblem> =
        state {
            do! BundlerState.setPoint TrackId.New V3d.Zero

            let edges = Dict<CameraId * CameraId, List<V2d * V2d>>()
           
            for KeyValue(_, track) in p.tracks do
                for KeyValue(ci,pi) in track do
                    for KeyValue(cj,pj) in track do
                        if ci < cj then
                            let list = edges.GetOrCreate((ci,cj), (fun _ -> List()))
                            list.Add (pi,pj)
            
            let edges = edges |> Dict.toList |> List.choose ( fun ((ci, cj),l) -> Some { i0 = ci.Id; i1 = cj.Id; weight = l } )
                






            return p
        }


//type BundlerProblem =
//    {
//        input           : BundlerInput
//        cameras         : Set<int>
//    }

//type BundlerSolution =
//    {
//        cost        : float
//        problem     : BundlerProblem
//        points      : Map<int, SolverPoint>
//        cameras     : Map<int, SolverCamera>
//    }

//type BundlerError =
//    {
//        cost        : float
//        max         : float
//        min         : float
//        average     : float
//        stdev       : float
//    }