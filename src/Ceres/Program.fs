namespace Ceres

open CeresSharp
open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Aardvark.Base

#nowarn "9"

module Entry =

    type AngleAxis private() =
        static member RotatePoint(aa : V3d, p : V3d) =
            let theta2 = aa.LengthSquared
            if not (Fun.IsTiny theta2) then
                let theta = sqrt theta2
                let costheta = cos theta
                let sintheta = sin theta
                let thetainverse = 1.0 / theta

                let w = aa * thetainverse

                let wCrossP = Vec.cross w p
                let tmp = (Vec.dot w p) * (1.0 - costheta)


                (p * costheta) + (wCrossP * sintheta) + (w * tmp)

            else
                let wCrossP = Vec.cross aa p
                p + wCrossP

        static member RotatePoint(aa : V3s, p : V3s) =
            let theta2 = aa.LengthSquared
            if not (Fun.IsTiny theta2.Value) then
                let theta = sqrt theta2
                let costheta = cos theta
                let sintheta = sin theta
                let thetainverse = 1.0 / theta

                let w = aa * thetainverse

                let wCrossP = Vec.cross w p
                let tmp = (Vec.dot w p) * (1.0 - costheta)


                (p * costheta) + (wCrossP * sintheta) + (w * tmp)

            else
                let wCrossP = Vec.cross aa p
                p + wCrossP



    type Camera3d =
        struct
            val mutable public Position     : V3d
            val mutable public AngleAxis    : V3d
            val mutable public SqrtFocalLength  : float
            
            member x.FocalLength = 0.01 + x.SqrtFocalLength * x.SqrtFocalLength

            member x.Project(p : V3d) =
                let view = AngleAxis.RotatePoint(x.AngleAxis, p - x.Position)
                x.FocalLength * view.XY / view.Z

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


    type Problem with   

        member x.AddParameterBlock(data : float[]) =
            let block = new ParameterBlock<float, scalar>(data, fun i v -> scalar.Variable(i,v))
            block :> IParameterBlock<float, scalar>   

        member x.AddParameterBlock(data : V2d[]) =
            let read (offset : int) (v : V2d) =
                V2s(scalar.Variable(offset, v.X), scalar.Variable(offset + 1, v.Y))
            let block = new ParameterBlock<V2d, V2s>(data, read)
            block :> IParameterBlock<_, _>   
            
        member x.AddParameterBlock(data : V3d[]) =
            let read (offset : int) (v : V3d) =
                V3s(scalar.Variable(offset, v.X), scalar.Variable(offset + 1, v.Y), scalar.Variable(offset + 2, v.Z))
            let block = new ParameterBlock<V3d, V3s>(data, read)
            block :> IParameterBlock<_, _>   

        member x.AddParameterBlock(data : V4d[]) =
            let read (offset : int) (v : V4d) =
                V4s(scalar.Variable(offset, v.X), scalar.Variable(offset + 1, v.Y), scalar.Variable(offset + 2, v.Z), scalar.Variable(offset + 3, v.W))
            let block = new ParameterBlock<V4d, V4s>(data, read)
            block :> IParameterBlock<_, _>   
            
        member inline x.AddParameterBlock< ^a, ^b when ^a : unmanaged and ^b : (static member Read : int * ^a -> ^b) > (data : ^a[]) : IParameterBlock< ^a, ^b > =
            let read (offset : int) (v : ^a) = (^b : (static member Read : int * ^a -> ^b) (offset, v))
            let block = new ParameterBlock< ^a, ^b >(data, read)
            block :> IParameterBlock<_, _>

        member x.AddCostFunction(residualCount : int, p0 : IParameterBlock<'b>, f : 'b[] -> scalar[]) =
            let c0 = p0.DoubleCount
            let evaluate (parameters : nativeptr<nativeptr<float>>, residuals : nativeptr<float>, jacobians : nativeptr<nativeptr<float>>) =
                let parameters = NativePtr.read parameters
                let jacobians = if NativePtr.isNull jacobians then NativePtr.zero else NativePtr.read jacobians
                if NativePtr.isNull jacobians then
                    let args = p0.Read(0, parameters)
                    let res = f args
                    for i in 0 .. residualCount - 1 do
                        NativePtr.set residuals i res.[i].Value
                else
                    let args = p0.Read(0, parameters)
                    let res = f args
                    for i in 0 .. residualCount - 1 do
                        NativePtr.set residuals i res.[i].Value

                        for (pi, j) in Map.toSeq res.[i].Jacobian do
                            let index = i * c0 + pi
                            NativePtr.set jacobians index j

                1

            x.AddCostFunction([| c0 |], residualCount, evaluate, id, [p0.Pointer])

        member x.AddCostFunction(residualCount : int, p0 : IParameterBlock<'b>, p1 : IParameterBlock<'c>, f : 'b[] -> 'c[] -> scalar[]) =
            let c0 = p0.DoubleCount
            let c1 = p1.DoubleCount
            let evaluate (parameters : nativeptr<nativeptr<float>>, residuals : nativeptr<float>, jacobians : nativeptr<nativeptr<float>>) =
                if NativePtr.isNull jacobians then
                    let a0 = p0.Read(0, NativePtr.get parameters 0)
                    let a1 = p1.Read(c0, NativePtr.get parameters 1)
                    let res = f a0 a1
                    for i in 0 .. residualCount - 1 do
                        NativePtr.set residuals i res.[i].Value
                else
                    let a0 = p0.Read(0, NativePtr.get parameters 0)
                    let a1 = p1.Read(c0, NativePtr.get parameters 1)

                    let res = f a0 a1
                    for i in 0 .. residualCount - 1 do
                        NativePtr.set residuals i res.[i].Value

                        let d0, d1 = Map.partition (fun k v -> k < c0) res.[i].Jacobian
                       

                        let j = NativePtr.get jacobians 0
                        if not (NativePtr.isNull j) then
                            for pi in 0 .. c0 - 1 do
                                let value = 
                                    match Map.tryFind pi d0 with
                                        | Some v -> v
                                        | None -> 0.0

                                let index = i * c0 + pi
                                NativePtr.set j index value
                                
                        let j = NativePtr.get jacobians 1
                        if not (NativePtr.isNull j) then
                            for pi in 0 .. c1 - 1 do
                                let value = 
                                    match Map.tryFind (pi + c0) d1 with
                                        | Some v -> v
                                        | None -> 0.0

                                let index = i * c1 + pi
                                NativePtr.set j index value

                1

            x.AddCostFunction([|c0; c1|], residualCount, evaluate, id, [p0.Pointer; p1.Pointer])


    type BundleAdjustmentProblem =
        {
            realPoints : int
            realCameras : int
            measurements : array<Map<int, V2d>>
        }

    type BundleAdjustmentSolution =
        {
            points  : V3d[]
            cameras : Camera3d[]
        }

    type BundleAdjustmentError =
        {
            cost        : float
            max         : float
            min         : float
            average     : float
            stdev       : float
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module BundleAdjustmentProblem =
        let relativeError (s : BundleAdjustmentSolution) (p : BundleAdjustmentProblem) =
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
                cost        = sumSq
                max         = emax
                min         = emin
                average     = average
                stdev       = sqrt variance
            }

    let private rand = RandomSystem()

    let rec bundle (input : BundleAdjustmentProblem) =
        use p = new Problem()
        let guessedPoints   = Array.init input.realPoints (fun _ -> rand.UniformV3dDirection())
        let guessedCameras  = Array.init input.realCameras (fun _ -> Camera3d.LookAt(rand.UniformV3dDirection() * 10.0, V3d.Zero, 1.0, V3d.OOI))
        let worldPoints     = p.AddParameterBlock guessedPoints
        let camBlocks       = guessedCameras |> Array.map (fun c -> p.AddParameterBlock<Camera3d, Camera3s>([|c|]))

        for ci in 0 .. guessedCameras.Length - 1 do
            let measurements = input.measurements.[ci] |> Seq.toArray
            let residuals = 2 * measurements.Length
            let res = Array.zeroCreate residuals

            p.AddCostFunction(residuals, worldPoints, camBlocks.[ci], fun world cam ->
                let cam = cam.[0]
                let mutable oi = 0
                for kvp in measurements do
                    let obs = cam.Project world.[kvp.Key]
                    let r = obs - kvp.Value
                    res.[oi + 0] <- r.X 
                    res.[oi + 1] <- r.Y 
                    oi <- oi + 2

                res
            )
        
        if p.Solve(CeresOptions(150, CeresSolverType.SparseSchur, true, 1.0E-4, 1.0E-4, 1.0E-4)) then
            let points = worldPoints.Result
            let cameras = camBlocks |> Array.map (fun b -> b.Result.[0])
            { cameras = cameras; points = points }
        else
            Log.warn "retry"
            bundle input

    let cameraTest() =
        let rand = RandomSystem()

        let realPoints = Array.init 50 (fun _ -> rand.UniformV3dDirection())
        let realCameras = Array.init 6 (fun _ -> Camera3d.LookAt(rand.UniformV3dDirection() * 4.0, V3d.Zero, 1.0, V3d.OOI))

        let measurements =
            Array.init realCameras.Length (fun ci ->
                Seq.init realPoints.Length (fun i -> i, realCameras.[ci].Project realPoints.[i]) |> Map.ofSeq
            )

        let problem =
            {
                realPoints = realPoints.Length
                realCameras = realCameras.Length
                measurements = measurements
            }

        let sol = bundle problem

        let err = problem |> BundleAdjustmentProblem.relativeError sol
        Log.start "test"
        
        Log.line "cost:    %A" err.cost
        Log.line "average: %.4f%%" (100.0 * err.average)
        Log.line "stdev:   %.4f%%" (100.0 * err.stdev)
        Log.line "min:     %.4f%%" (100.0 * err.min)
        Log.line "max:     %.4f%%" (100.0 * err.max)
        Log.stop()

        sol

    [<EntryPoint>]
    let main argv =
        let sol = cameraTest()

        Environment.Exit 0

        use p = new Problem()

        let b = p.AddParameterBlock [| 1.0 |]
        let c = p.AddParameterBlock [| 1.0 |]

        p.AddCostFunction(2, b, c, fun b c ->
            let x = b.[0]
            let y = c.[0]

            [|
                x - y
                x * x + y * y - 1.0
            |]
        )

        p.Solve(CeresOptions(100, CeresSolverType.DenseSchur, true, 1.0E-16, 1.0E-16, 1.0E-16)) |> ignore

        let b = b.Result 
        let c = c.Result 

        printfn "b = %A" b
        printfn "c = %A" c


        0 
