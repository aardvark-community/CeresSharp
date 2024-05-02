namespace CeresSharp.Raw

open System
open System.Threading
open System.Security
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Collections.Generic
open Microsoft.FSharp.NativeInterop
open Aardvark.Base

#nowarn "9"
#nowarn "51"


type CeresSolverType =
    | DenseNormalCholesky = 0
    | DenseQr = 1
    | SparseNormalCholesky = 2
    | DenseSchur = 3
    | SparseSchur = 4
    | IterativeSchur = 5
    | CGNR = 6

[<StructLayout(LayoutKind.Sequential)>]
type CeresOptions =
    struct
        val mutable public MaxIterations : int
        val mutable public SolverType : CeresSolverType
        val mutable public PrintProgress : int
        val mutable public GradientTolerance : double
        val mutable public FunctionTolerance : double
        val mutable public ParameterTolerance : double

        new(maxIter, solverType, print, gradTol, fTol, pTol) = { MaxIterations = maxIter; SolverType = solverType; PrintProgress = (if print then 1 else 0); GradientTolerance = gradTol; FunctionTolerance = fTol; ParameterTolerance = pTol }

    end

type CeresLossFunctionKind =
    | Trivial = 0
    | Huber = 1
    | SoftLOne = 2
    | Cauchy = 3
    | ArcTan = 4
    | Tolerant = 5

[<StructLayout(LayoutKind.Sequential)>]
type CeresLossFunction =
    struct
        val mutable public Kind : CeresLossFunctionKind
        val mutable public P0 : float
        val mutable public P1 : float

        new (k, p0, p1) = { Kind = k; P0 = p0; P1 = p1}
        new (k, p0) = { Kind = k; P0 = p0; P1 = 0.0}
        new (k) = { Kind = k; P0 = 0.0; P1 = 0.0}


        static member Trivial = CeresLossFunction(CeresLossFunctionKind.Trivial)
        static member Huber(a : float) = CeresLossFunction(CeresLossFunctionKind.Huber, a)
        static member SoftLOne(a : float) = CeresLossFunction(CeresLossFunctionKind.SoftLOne, a)
        static member Cauchy(a : float) = CeresLossFunction(CeresLossFunctionKind.Cauchy, a)
        static member ArcTan(a : float) = CeresLossFunction(CeresLossFunctionKind.ArcTan, a)
        static member Tolerant(a : float, b : float) = CeresLossFunction(CeresLossFunctionKind.Tolerant, a, b)
    end


[<StructLayout(LayoutKind.Sequential)>]
type CeresProblem =
    struct
        val mutable public Handle : nativeint

        new(h) = { Handle = h }
    end

[<StructLayout(LayoutKind.Sequential)>]
type CeresCostFunction =
    struct
        val mutable public Handle : nativeint
    end
    
[<StructLayout(LayoutKind.Sequential)>]
type CeresLossFunctionHandle =
    struct
        val mutable public Handle : nativeint
    end

type CeresCostFunctionDelegate = delegate of nativeptr<nativeptr<float>> * nativeptr<float> * nativeptr<nativeptr<float>> -> int

[<Struct; StructLayout(LayoutKind.Sequential)>]
type CeresCamera3d private (rx : float, ry : float, rz : float, tx : float, ty : float, tz : float) =

    static member FromEuclidean3d (e : Euclidean3d) =
        let aa = e.Rot.ToAngleAxis()
        CeresCamera3d(
            aa.X, aa.Y, aa.Z,
            e.Trans.X, e.Trans.Y, e.Trans.Z
        )
        
    member x.ToEuclidean3d() =
        Euclidean3d(
            Rot3d.FromAngleAxis (V3d(rx, ry, rz)),
            V3d(tx, ty, tz)
        )

[<Struct; StructLayout(LayoutKind.Sequential)>]
type CeresProjection(focalLength : float, aspect : float, ppx : float, ppy : float) =
    member x.FocalLength = focalLength
    member x.Aspect = aspect
    member x.PrincipalPointX = ppx
    member x.PrincipalPointY = ppy

[<Struct; StructLayout(LayoutKind.Sequential)>]
type CeresDistortion(k1 : float, k2 : float, k3 : float, p1 : float, p2 : float) =
    member x.K1 = k1
    member x.K2 = k2
    member x.K3 = k3
    member x.P1 = p1
    member x.P2 = p2

[<Struct; StructLayout(LayoutKind.Sequential)>]
type CeresBundleResidual(projectionIndex : int, cameraIndex : int, pointIndex : int, weight: float, observation : V2d, imageSize : V2i) =
    member x.ProjectionIndex = projectionIndex
    member x.CameraIndex = cameraIndex
    member x.PointIndex = pointIndex
    member x.Observation = observation
    member x.ImageSize = imageSize
    member x.Weight = weight

    new (projectionIndex : int, cameraIndex : int, pointIndex : int, observation : V2d, imageSize : V2i) =
        CeresBundleResidual(projectionIndex, cameraIndex, pointIndex, 1.0, observation, imageSize)
        
[<Struct; StructLayout(LayoutKind.Sequential)>]
type CeresBundleIteration private(projConstant : int, distConstant : int, camConstant : int, pointConstant : int, fixedPointCount : int, fixedPointIndices : nativeptr<int>, functionTolerance : float, parameterTolerance : float, gradientTolerance : float, maxIterations : int) =
    member x.ProjConstant = projConstant <> 0
    member x.DistortionConstant = distConstant <> 0
    member x.CamConstant = camConstant <> 0
    member x.PointConstant = pointConstant <> 0
    member x.FixedPointCount = fixedPointCount
    member x.FixedPointIndices = fixedPointIndices
    member x.FunctionTolerance = functionTolerance
    member x.ParameterTolerance = parameterTolerance
    member x.GradientTolerance = gradientTolerance
    member x.MaxIterations = maxIterations

    new(projConstant : bool, distConstant : bool, camConstant : bool, pointsConstant : bool, fixedPointCount : int, fixedPointIndices : nativeptr<int>, functionTolerance : float, parameterTolerance : float, gradientTolerance : float, maxIterations : int) =
        CeresBundleIteration(
            (if projConstant then 1 else 0),
            (if distConstant then 1 else 0),
            (if camConstant then 1 else 0),
            (if pointsConstant then 1 else 0),
            fixedPointCount, fixedPointIndices,
            functionTolerance, parameterTolerance, gradientTolerance, maxIterations
        )
        
type CeresTerminationType =
    | Convergence = 0
    | NoConvergence = 1
    | Failure = 2
    | UserSuccess = 3
    | UserFailure = 4

module CeresRaw =
    
    [<Literal>]
    let lib = "CeresNative"
    
    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern CeresLossFunctionHandle cCreateLossFunction(CeresLossFunction f)
    
    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern void cReleaseLossFunction(CeresLossFunctionHandle f)

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern CeresProblem cCreateProblem()

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern void cReleaseProblem(CeresProblem problem)

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern CeresCostFunction cCreateCostFunction(int nParameterCount, int[] parameterCounts, int residualCount, nativeint delPtr)

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern void cReleaseCostFunction(CeresCostFunction func)

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern void cAddResidualFunction1(CeresProblem problem, CeresLossFunctionHandle loss, CeresCostFunction func, double* p0)

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern void cAddResidualFunction2(CeresProblem problem, CeresLossFunctionHandle loss, CeresCostFunction func, double* p0, double* p1)

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern void cAddResidualFunction3(CeresProblem problem, CeresLossFunctionHandle loss, CeresCostFunction func, double* p0, double* p1, double* p2)

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern void cAddResidualFunction4(CeresProblem problem, CeresLossFunctionHandle loss, CeresCostFunction func, double* p0, double* p1, double* p2, double* p3)

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern void cAddResidualFunction5(CeresProblem problem, CeresLossFunctionHandle loss, CeresCostFunction func, double* p0, double* p1, double* p2, double* p3, double* p4)

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern void cAddResidualFunction6(CeresProblem problem, CeresLossFunctionHandle loss, CeresCostFunction func, double* p0, double* p1, double* p2, double* p3, double* p4, double* p5)

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern void cAddResidualFunction7(CeresProblem problem, CeresLossFunctionHandle loss, CeresCostFunction func, double* p0, double* p1, double* p2, double* p3, double* p4, double* p5, double* p6)

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern void cAddResidualFunction8(CeresProblem problem, CeresLossFunctionHandle loss, CeresCostFunction func, double* p0, double* p1, double* p2, double* p3, double* p4, double* p5, double* p6, double* p7)

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern float cSolve(CeresProblem problem, CeresOptions* options, CeresTerminationType* termination, int* usable)

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern float cOptimizePhotonetwork (
        CeresOptions* options, bool nonmonotonic, bool useDifferentialPoses,
        int nIterations, CeresBundleIteration* iterations,
        int nProjections, CeresProjection* projs, CeresDistortion* dists,
        int nCams, CeresCamera3d* cams, 
        int nPoints, V3d* world,
        int nResiduals, CeresBundleResidual* residuals,
        M33d* pointCovariances, M33d* cameraLocationCovariances)
    
