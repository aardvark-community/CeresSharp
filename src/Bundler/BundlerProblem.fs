﻿namespace Aardvark.Ceres

open System
open Aardvark.Base

type Camera3d =
    struct
        val mutable public Position         : V3d
        val mutable public AngleAxis        : V3d
        val mutable public SqrtFocalLength  : float
        val mutable public Distortion       : V2d
            
        member x.FocalLength = 0.01 + x.SqrtFocalLength * x.SqrtFocalLength

        member x.Project(p : V3d) =
            let view = AngleAxis.RotatePoint(x.AngleAxis, p - x.Position)
            let ndc = view.XY / view.Z

            let distortion = 
                let r2 = ndc.LengthSquared
                1.0 + r2 * (x.Distortion.X + x.Distortion.Y * r2)

            x.FocalLength * distortion * ndc

        member x.ViewProjTrafo (far : float) =
            let frustum = { left = -1.0; right = 1.0; top = 1.0; bottom = -1.0; near = x.FocalLength; far = far }

            Trafo3d.Translation(-x.Position) *
            AngleAxis.Trafo(x.AngleAxis) *
            Frustum.projTrafo frustum

        member x.Transformed (t : Trafo3d) =
            let up = AngleAxis.RotatePoint(-x.AngleAxis, V3d.OIO)
            let fw = AngleAxis.RotatePoint(-x.AngleAxis, -V3d.OOI * x.FocalLength)
            let p =  x.Position         |> t.Forward.TransformPosProj
            let pu = x.Position + up    |> t.Forward.TransformPosProj
            let pf = x.Position + fw    |> t.Forward.TransformPosProj

            let u = pu - p
            let s = Vec.length u

            let mutable res = Camera3d.LookAt(p, pf, (pf - p).Length / s, u / s)
            res.Distortion <- x.Distortion
            res

        member x.GetRay (pt : V2d) =
            let ndc = V3d(pt.X, pt.Y, x.FocalLength)
            let dir = AngleAxis.RotatePoint(-x.AngleAxis, ndc) |> Vec.normalize
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
            let res = Camera3d(eye, aa, f, V2d.Zero)

            let test = res.Project center
            res

        static member Delta(src : Camera3d, dst : Camera3d) =
            let src = src.ViewProjTrafo 100.0
            let dst = dst.ViewProjTrafo 100.0
            src * dst.Inverse




        new(pos, angleAxis, f, d) = { Position = pos; AngleAxis = angleAxis; SqrtFocalLength = sqrt (f - 0.01); Distortion = d }
    end

type Camera3s(pos : V3s, aa : V3s, sf : scalar, d : V2s) =
    let f = 0.01 + sf * sf
    member x.Position = pos
    member x.AngleAxis = aa
    member x.SqrtFocalLength = sf
    member x.FocalLength = f
    member x.Distortion = d

    member x.ProjectNoDistortion(p : V3s) =
        let view = AngleAxis.RotatePoint(x.AngleAxis, p - x.Position)
        let ndc = view.XY / view.Z
        x.FocalLength * ndc           
         
    member x.Project(p : V3s) =
        let view = AngleAxis.RotatePoint(x.AngleAxis, p - x.Position)
        let ndc = view.XY / view.Z

        let distortion = 
            let r2 = ndc.LengthSquared
            scalar 1.0 + r2 * (x.Distortion.X + x.Distortion.Y * r2)

        x.FocalLength * distortion * ndc

    static member Read(offset : int, v : Camera3d) =
        let p   = V3s(scalar.Variable(offset + 0, v.Position.X), scalar.Variable(offset + 1, v.Position.Y), scalar.Variable(offset + 2, v.Position.Z))
        let aa  = V3s(scalar.Variable(offset + 3, v.AngleAxis.X), scalar.Variable(offset + 4, v.AngleAxis.Y), scalar.Variable(offset + 5, v.AngleAxis.Z))
        let sf  = scalar.Variable(offset + 6, v.SqrtFocalLength)
        let d  = V2s(scalar.Variable(offset + 7, v.SqrtFocalLength), scalar.Variable(offset + 8, v.SqrtFocalLength))
        Camera3s(p, aa, sf, d)

[<Struct>]
type Match2d(pos : V2d, vel : V2d, o : V4d, li : int, ri : int) =
    
    member x.Left = li
    member x.Right = ri

    member x.LengthSquared =
        pos.LengthSquared + vel.LengthSquared + o.LengthSquared

    member x.Length = 
        sqrt x.LengthSquared

    member x.Pos = pos
    member x.Vel = vel
    member x.O = o

    static member (-)(l : Match2d,r : Match2d) =
        Match2d(l.Pos-r.Pos, l.Vel-r.Vel, l.O-r.O, -1, -1)

    static member Dot(l : Match2d,r : Match2d) =
        Vec.dot l.Pos r.Pos + Vec.dot l.Vel r.Vel + Vec.dot l.O r.O
        

module MatchProblem =       
    open CeresSharp

    let private random = RandomSystem()

    let inline sum (n : int) (f : int -> 'a) =
        let mutable res = LanguagePrimitives.GenericZero
        for i in 0 .. n - 1 do
            res <- res + f i
        res

    let inline sum' (z : 'a) (n : int) (f : int -> 'a) =
        let mutable res = z
        for i in 0 .. n - 1 do
            res <- res + f i
        res

    let o (rotation : float) =
        V4d(0.0, sin(rotation), 0.0, cos(rotation))


    let delta = 0.5
    let gamma = 1.0
    let lambda = 0.5
    let sigma = gamma * gamma

    let huber (a : scalar) =   //huber
        if abs a.Value <= delta then
            0.5 * a * a
        else
            delta * (abs a - 0.5 * delta)

    let likelihood ( ms : Match2d[] ) =
        use p = new Problem()

        let w = p.AddParameterBlock(Array.init ms.Length (ignore >> random.UniformDouble))

        let N = ms.Length
        let G =
            Array2D.init N N 
                ( fun i j -> 
                    exp (- (ms.[i] - ms.[j]).LengthSquared / sigma ) )


        p.AddCostFunction ( 1, w, fun w ->
            let f m =   
                sum N (fun i ->
                    w.[i] * exp (- (m - ms.[i]).LengthSquared / sigma )
                )

            let wGw = 
                sum N (fun i ->
                    w.[i] *
                    sum N (fun j ->
                        G.[i,j] * w.[j]
                    )
                )

            [| sum N (fun i -> huber(1.0 - f ms.[i])) + lambda * wGw |]
        )

        let cost = p.Solve(CeresOptions(1000, CeresSolverType.SparseSchur, true, 1.0E-3, 1.0E-3, 1.0E-3))
        let w = w.Result
        let f m =   
            sum N (fun i ->
                w.[i] * exp (- (m - ms.[i]).LengthSquared / sigma )
            )

        f


    let private affineComponent (c : int) (G : float[,]) (ps : V4d[]) =
        use px = new Problem()
        let N = ps.Length
        
        let wx = px.AddParameterBlock(Array.init N (ignore >> random.UniformV3d))
        let Hx = px.AddParameterBlock [| V3d.Zero |]
        
        let g (v : V4d) =
            exp (-v.LengthSquared / sigma)

        px.AddCostFunction(1, wx, Hx, fun w H ->
            let H = H.[0]
            let f (p : V4d) = H + sum N (fun j -> w.[j] * g(p - ps.[j]))
            let q (p : V4d) = Vec.dot (f p) (V3s p.XYI)


            let E = sum N (fun j -> huber(ps.[j].[c] + ps.[j].[c+2] - q(ps.[j])))

            let wGw = 
                sum N (fun i ->
                    w.[i] *
                    sum N (fun j ->
                        G.[i,j] * w.[j]
                    )
                )

            [| E + lambda * (wGw.X + wGw.Y + wGw.Z) |]
        )

        px.Solve(CeresOptions(1000, CeresSolverType.SparseSchur, true, 1.0E-3, 1.0E-3, 1.0E-3)) |> ignore
        
        let H = Hx.Result.[0]
        let w = wx.Result
        let f (p : V4d) = H + sum' V3d.Zero N (fun j -> w.[j] * g(p - ps.[j]))
        let q (p : V4d) = Vec.dot (f p) p.XYI
        q


    let affine ( ms : Match2d[] ) =
        
        let ps = ms |> Array.map (fun m -> V4d(m.Pos.X, m.Pos.Y, m.Vel.X, m.Vel.Y))
        let N = ms.Length
        let ms = ()

        let g (v : V4d) = exp (-v.LengthSquared / sigma)

        let G = Array2D.init N N ( fun i j -> g(ps.[i] - ps.[j]))

        let qx = affineComponent 0 G ps
        let qy = affineComponent 1 G ps

        let q (p : V4d) = V2d(qx p, qy p)
        q


type BundlerInput =
    {
        measurements : Map<int,Map<int, V2d>>
    }

type BundlerProblem =
    {
        input           : BundlerInput
        cameras         : Set<int>
    }

type BundlerSolution =
    {
        cost        : float
        problem     : BundlerProblem
        points      : Map<int, V3d>
        cameras     : Map<int, Camera3d>
    }

type BundlerError =
    {
        cost        : float
        max         : float
        min         : float
        average     : float
        stdev       : float
    }



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BundlerProblem =

    let inline input (p : BundlerProblem) = p.input
    let inline cameras (p : BundlerProblem) = p.cameras
  
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BundlerSolution =
    let inline problem (s : BundlerSolution) = s.problem
    let inline points (s : BundlerSolution) = s.points
    let inline cameras (s : BundlerSolution) = s.cameras
 
    let errorMetrics (s : BundlerSolution) =
        let errors =
            s.problem.cameras
                |> Seq.collect (fun ci ->
                    let measurements = s.problem.input.measurements.[ci]
                    let cam = s.cameras.[ci]

                    measurements 
                        |> Map.toSeq 
                        |> Seq.map (fun (pi, m) -> 
                            let obs = cam.Project s.points.[pi]
                            let v = obs - m
                            v, Vec.length (0.5 * v) // 0.5 because [-1,1]
                        ) 
                    )
                |> Seq.toArray

        let mutable sumSq = 0.0
        let mutable sum = 0.0
        let mutable emin = Double.PositiveInfinity
        let mutable emax = Double.NegativeInfinity
        for (v,l) in errors do
            sumSq <- sumSq + v.X * v.X + v.Y * v.Y
            sum <- sum + l
            emin <- min emin l
            emax <- max emax l


        let average = sum / float errors.Length
        let variance = Array.sumBy (fun (_,e) -> (e - average) * (e - average)) errors / float (errors.Length - 1)


        {
            cost        = 0.5 * sumSq
            max         = emax
            min         = emin
            average     = average
            stdev       = sqrt variance
        }


    let private rand = RandomSystem()

    let private withCost (s : BundlerSolution) =
        let m = errorMetrics s
        { s with cost = m.cost }


    let transformed (trafo : Trafo3d) (s : BundlerSolution) =
        let fw = trafo.Forward
        { s with 
            points = s.points |> Map.map (fun _ p -> fw.TransformPosProj p)
            cameras = s.cameras |> Map.map (fun _ c -> c.Transformed trafo)
        }

    let merge (l : BundlerSolution) (r : BundlerSolution) =
        if l.problem.input <> r.problem.input then failwith "cannot merge SubSolutions for different inputs"

        let overlapping = Map.intersect l.points r.points
        if overlapping.Count < 8 then failwith "cannot merge SubSolutions with less than 8 shared points"

        let lPoints, rPoints = overlapping |> Map.toSeq |> Seq.map snd |> Seq.toArray |> Array.unzip

        let trafo = PointCloud.trafo rPoints lPoints

        let r = transformed trafo r

        withCost {
            cost        = 0.0
            problem     = { l.problem with cameras = Set.union l.problem.cameras r.problem.cameras }
            points      = Map.union r.points l.points
            cameras     = Map.union r.cameras l.cameras
        }

    let random (p : BundlerProblem) =
        let cameras = p.cameras |> Seq.map (fun ci -> ci, Camera3d.LookAt(rand.UniformV3dDirection() * 10.0, V3d.Zero, 1.0, V3d.OOI)) |> Map.ofSeq
        let points = p.cameras |> Seq.collect (fun ci -> p.input.measurements.[ci] |> Map.toSeq |> Seq.map fst) |> Set.ofSeq |> Seq.map (fun pi -> pi, rand.UniformV3dDirection()) |> Map.ofSeq
        withCost {
            cost = 0.0
            problem = p
            cameras = cameras
            points = points
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BundlerError =
    let inline cost (s : BundlerError) = s.cost
    let inline max (s : BundlerError) = s.max
    let inline min (s : BundlerError) = s.min
    let inline average (s : BundlerError) = s.average
    let inline stdev (s : BundlerError) = s.stdev

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BundlerInput =
    
    let preprocess (input : BundlerInput) =
        let counts = Dict<int, ref<int>>()

        // count how often each point is referenced
        for kvp in input.measurements do
            let m = kvp.Value
            for kvp in m do
                let r = counts.GetOrCreate(kvp.Key, fun _ -> ref 0)
                r := !r + 1

        // points that are visible from only one camera are useless
        let valid = 
            counts 
                |> Dict.toSeq 
                |> Seq.choose (fun (pi,r) -> if !r >= 2 then Some pi else None) 
                |> HashSet.ofSeq
        
        // remove invalid points from all measurements
        let measurements =
            input.measurements |> Map.map (fun _ m ->
                m |> Map.filter (fun pi _ -> valid.Contains pi)
            )

        Log.line "reduced %A -> %A (visible from 2 cams)" counts.Count (measurements |> Map.toArray |> Array.map snd).[0].Count

        // prune them from the input
        { input with measurements = measurements }

    let toProblem (i : BundlerInput) =
        // cameras that see less than 8 points are not stable
        let cameras = 
            i.measurements |> Map.filter (fun i m -> 
                if m.Count < 8 then
                    Log.warn "Camera %A has less than 8 points (%A) and is discarded." i m.Count
                    false
                else
                    true
            )

        if cameras.Count = 0 then Log.error "No stable cameras found, solution impossible."

        {
            input = i
            cameras = cameras |> Map.toArray |> Array.map fst |> Set.ofArray 
        }

