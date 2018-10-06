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

module CeresRaw =
    
    [<Literal>]
    let lib = "CeresNative.dll"
    
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
    extern float cSolve(CeresProblem problem, CeresOptions* options)


