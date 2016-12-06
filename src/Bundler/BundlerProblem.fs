namespace Aardvark.Ceres

open System
open Aardvark.Base

type Camera3d =
    struct
        val mutable public Position         : V3d
        val mutable public AngleAxis        : V3d
        val mutable public SqrtFocalLength  : float
            
        member x.FocalLength = 0.01 + x.SqrtFocalLength * x.SqrtFocalLength

        member x.Project(p : V3d) =
            let view = AngleAxis.RotatePoint(x.AngleAxis, p - x.Position)
            x.FocalLength * view.XY / view.Z

        member x.ViewProjTrafo (far : float) =
            let frustum = { left = -1.0; right = 1.0; top = 1.0; bottom = -1.0; near = x.FocalLength; far = far }

            Trafo3d.Translation(-x.Position) *
            AngleAxis.Trafo(x.AngleAxis) *
            Frustum.projTrafo frustum

        static member LookAt(eye : V3d, center : V3d, f : float, sky : V3d) =
            let forward = Vec.normalize (center - eye)
            let left = Vec.cross sky forward |> Vec.normalize
            let up = Vec.cross forward left |> Vec.normalize

            let rot = M44d.FromBasis(-left, up, -forward, V3d.Zero).UpperLeftM33() |> Rot3d.FromM33d
            let mutable axis = V3d.Zero
            let mutable angle = 0.0
            rot.ToAxisAngle(&axis, &angle)
            let aa = axis * -angle
            let res = Camera3d(eye, aa, f)

            let test = res.Project center
            if not (Fun.IsTiny test.X) || not (Fun.IsTiny test.Y) then
                Log.warn "invalid lookAt: project(lookAt) = %A" test


            res


        new(pos, angleAxis, f) = { Position = pos; AngleAxis = angleAxis; SqrtFocalLength = sqrt (f - 0.01) }
    end

type Camera3s(pos : V3s, aa : V3s, sf : scalar) =
    let f = 0.01 + sf * sf
    member x.Position = pos
    member x.AngleAxis = aa
    member x.SqrtFocalLength = sf
    member x.FocalLength = f

            
    member x.Project(p : V3s) =
        let view = AngleAxis.RotatePoint(x.AngleAxis, p - x.Position)
        x.FocalLength * view.XY / view.Z

    static member Read(offset : int, v : Camera3d) =
        let p   = V3s(scalar.Variable(offset + 0, v.Position.X), scalar.Variable(offset + 1, v.Position.Y), scalar.Variable(offset + 2, v.Position.Z))
        let aa  = V3s(scalar.Variable(offset + 3, v.AngleAxis.X), scalar.Variable(offset + 4, v.AngleAxis.Y), scalar.Variable(offset + 5, v.AngleAxis.Z))
        let sf  = scalar.Variable(offset + 6, v.SqrtFocalLength)
        Camera3s(p, aa, sf)


type BundlerProblem =
    {
        realPoints : int
        realCameras : int
        measurements : array<Map<int, V2d>>
    }

type BundlerSolution =
    {
        problem : BundlerProblem
        points  : V3d[]
        cameras : Camera3d[]
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

    let inline realPoints (p : BundlerProblem) = p.realPoints
    let inline realCameras (p : BundlerProblem) = p.realCameras
    let inline measurements (p : BundlerProblem) = p.measurements

    let errorMetrics (s : BundlerSolution) (p : BundlerProblem) =
        let errors =
            p.measurements 
                |> Seq.mapi (fun ci measurements ->
                    let cam = s.cameras.[ci]

                    measurements 
                        |> Map.toSeq 
                        |> Seq.map (fun (pi, m) -> 
                            let obs = cam.Project s.points.[pi]
                            let v = obs - m
                            v, Vec.length (0.5 * v) // 0.5 because [-1,1]
                        ) 
                    )
                |> Seq.concat
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
      
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BundlerSolution =
    let inline problem (s : BundlerSolution) = s.problem
    let inline points (s : BundlerSolution) = s.points
    let inline cameras (s : BundlerSolution) = s.cameras
       
    let errorMetrics (s : BundlerSolution) =
        let errors =
            s.problem.measurements 
                |> Seq.mapi (fun ci measurements ->
                    let cam = s.cameras.[ci]

                    measurements 
                        |> Map.toSeq 
                        |> Seq.map (fun (pi, m) -> 
                            let obs = cam.Project s.points.[pi]
                            let v = obs - m
                            v, Vec.length (0.5 * v) // 0.5 because [-1,1]
                        ) 
                    )
                |> Seq.concat
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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BundlerError =
    let inline cost (s : BundlerError) = s.cost
    let inline max (s : BundlerError) = s.max
    let inline min (s : BundlerError) = s.min
    let inline average (s : BundlerError) = s.average
    let inline stdev (s : BundlerError) = s.stdev

