namespace CeresSharp

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


[<StructLayout(LayoutKind.Sequential)>]
type private CeresProblem =
    struct
        val mutable public Handle : nativeint

        new(h) = { Handle = h }
    end

[<StructLayout(LayoutKind.Sequential)>]
type private CeresCostFunction =
    struct
        val mutable public Handle : nativeint
    end

type private CeresCostFunctionDelegate = delegate of nativeptr<nativeptr<float>> * nativeptr<float> * nativeptr<nativeptr<float>> -> int

module private CeresRaw =
    
    [<Literal>]
    let lib = "CeresNative.dll"

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern CeresProblem cCreateProblem()

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern void cReleaseProblem(CeresProblem problem)

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern CeresCostFunction cCreateCostFunction(int nParameterCount, int[] parameterCounts, int residualCount, nativeint delPtr)

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern void cReleaseCostFunction(CeresCostFunction func)

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern void cAddResidualFunction1(CeresProblem problem, CeresCostFunction func, double* p0)

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern void cAddResidualFunction2(CeresProblem problem, CeresCostFunction func, double* p0, double* p1)

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern void cAddResidualFunction3(CeresProblem problem, CeresCostFunction func, double* p0, double* p1, double* p2)

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern void cAddResidualFunction4(CeresProblem problem, CeresCostFunction func, double* p0, double* p1, double* p2, double* p3)

    [<DllImport(lib); SuppressUnmanagedCodeSecurity>]
    extern float cSolve(CeresProblem problem, CeresOptions* options)

module TypeInfo =
    open Aardvark.Base.TypeInfo.Patterns
    type private DoubleCount<'a> private() =
        static let cnt =
            match typeof<'a> with
                | Float64 -> 1
                | VectorOf(d, Float64) -> d
                | MatrixOf(s, Float64) -> s.X * s.Y
                | t -> 
                    let s = sizeof<'a>
                    if s % sizeof<double> <> 0 then failwithf "ill-aligned parameter-type %A" typeof<'a>
                    else s / sizeof<double>

        static member Count = cnt

    let doubles<'a> = DoubleCount<'a>.Count

type IParameterBlock<'b> =
    abstract member DoubleCount : int
    abstract member Read : int * nativeptr<float> -> 'b[]
    abstract member Pointer : nativeptr<float>

type IParameterBlock<'a, 'b> =
    inherit IDisposable
    inherit IParameterBlock<'b>
    abstract member Result : 'a[]
    abstract member IntermediaryResult : 'a[]
    abstract member GetResult : bool -> 'a[]

type ParameterBlock<'a, 'b when 'a : unmanaged>(data : 'a[], read : int -> 'a -> 'b) =
    static let doubles = TypeInfo.doubles<'a>

    let gc = GCHandle.Alloc(data, GCHandleType.Pinned)

    let intermediaryResult = Array.copy data

    member x.Read(offset : int, ptr : nativeptr<'a>) =
        let res = Array.zeroCreate data.Length
        let mutable vi = offset
        let step = doubles 
        for i in 0 .. data.Length - 1 do
            let v = NativePtr.get ptr i
            res.[i] <- v |> read vi
            intermediaryResult.[i] <- v
            vi <- vi + step
        res

    member x.IntermediaryResult = intermediaryResult

    member x.Result = data

    member x.GetResult intermediate = 
        match intermediate with
        | true -> x.IntermediaryResult
        | false -> x.Result

    member x.Pointer = gc.AddrOfPinnedObject() |> NativePtr.ofNativeInt<float>

    member x.Dispose() = gc.Free()

    interface IParameterBlock<'a, 'b> with
        member x.DoubleCount = doubles * data.Length
        member x.Pointer = x.Pointer
        member x.Read(o,ptr) = x.Read(o,NativePtr.cast ptr)
        member x.IntermediaryResult = x.IntermediaryResult
        member x.Result = data
        member x.Dispose() = x.Dispose()
        member x.GetResult(b) = x.GetResult b


type Problem() =
    let mutable handle = CeresRaw.cCreateProblem()

    let costFunctions = List<CeresCostFunction * GCHandle * IDisposable>()


    member x.AddCostFunction(parameterCounts : array<int>, residualCount : int, f : nativeptr<nativeptr<float>> * nativeptr<float> * nativeptr<nativeptr<float>> -> int, free : unit -> unit, parameters : list<nativeptr<float>>) =
        let del = 
            CeresCostFunctionDelegate(fun parameters residuals jacobians ->
                f(parameters, residuals, jacobians)
            )

        let gc = GCHandle.Alloc(del)
        let ptr = Marshal.GetFunctionPointerForDelegate(del)


        let fhandle = CeresRaw.cCreateCostFunction(parameterCounts.Length, parameterCounts, residualCount, ptr)
        costFunctions.Add(fhandle, gc, { new IDisposable with member x.Dispose() = free() })

        match parameters with
            | [] -> failwith "cost function has no parameters"
            | [p0] -> CeresRaw.cAddResidualFunction1(handle, fhandle, p0)
            | [p0; p1] -> CeresRaw.cAddResidualFunction2(handle, fhandle, p0, p1)
            | [p0; p1; p2] -> CeresRaw.cAddResidualFunction3(handle, fhandle, p0, p1, p2)
            | [p0; p1; p2; p3] -> CeresRaw.cAddResidualFunction4(handle, fhandle, p0, p1, p2, p3)
            | _ -> failwithf "too many parameter-blocks for cost function: %A" (List.length parameters)


    member x.Solve(options : CeresOptions) =
        let mutable options = options
        CeresRaw.cSolve(handle, &&options)

    member private x.Dispose(disposing : bool) =
        let o = Interlocked.Exchange(&handle.Handle, 0n)
        if o <> 0n then 
            
            for (f, gc, cf) in costFunctions do
                gc.Free()
                cf.Dispose()

            CeresRaw.cReleaseProblem(CeresProblem(o))
            if disposing then GC.SuppressFinalize(x)

    member x.Dispose() = x.Dispose true
    override x.Finalize() = x.Dispose false

    interface IDisposable with
        member x.Dispose() = x.Dispose true
