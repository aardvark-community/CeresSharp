namespace CeresSharp

open System
open System.Threading
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open CeresSharp.Raw
open Microsoft.FSharp.NativeInterop
open Aardvark.Base
open CeresSharp.Raw 

#nowarn "9"

module private TypeInfo =
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

type ParameterBlock<'a, 'b when 'a : unmanaged>(data : 'a[], doubles : int, read : int -> 'a -> 'b) =
    //static let doubles = TypeInfo.doubles<'a>

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

    member private x.Dispose(disposing : bool) =
        if disposing then GC.SuppressFinalize x
        gc.Free()

    member x.Dispose() = x.Dispose(true)
    override x.Finalize() = x.Dispose(false)

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
    let loss = Dict<LossFunction, CeresLossFunctionHandle>()

    let getLoss (l : LossFunction) =
        loss.GetOrCreate(l, fun l ->
            CeresRaw.cCreateLossFunction (LossFunction.toCeresLossFunction l)
        )

    member x.AddCostFunction(parameterCounts : array<int>, residualCount : int, loss : LossFunction, f : nativeptr<nativeptr<float>> * nativeptr<float> * nativeptr<nativeptr<float>> -> int, free : unit -> unit, parameters : list<nativeptr<float>>) =
        
        let del = 
            CeresCostFunctionDelegate(fun parameters residuals jacobians ->
                f(parameters, residuals, jacobians)
            )

        let gc = GCHandle.Alloc(del)
        let ptr = Marshal.GetFunctionPointerForDelegate(del)
        
        let fhandle = CeresRaw.cCreateCostFunction(parameterCounts.Length, parameterCounts, residualCount, ptr)
        costFunctions.Add(fhandle, gc, { new IDisposable with member x.Dispose() = free() })

        let loss = getLoss loss

        match parameters with
            | [] -> failwith "cost function has no parameters"
            | [p0] -> CeresRaw.cAddResidualFunction1(handle, loss, fhandle, p0)
            | [p0; p1] -> CeresRaw.cAddResidualFunction2(handle, loss, fhandle, p0, p1)
            | [p0; p1; p2] -> CeresRaw.cAddResidualFunction3(handle, loss, fhandle, p0, p1, p2)
            | [p0; p1; p2; p3] -> CeresRaw.cAddResidualFunction4(handle, loss, fhandle, p0, p1, p2, p3)
            | _ -> failwithf "too many parameter-blocks for cost function: %A" (List.length parameters)
            
    member x.Solve(options : Config) =
        use pOptions = fixed [| Config.toCeresOptions options |]
        CeresRaw.cSolve(handle, pOptions)

    member private x.Dispose(disposing : bool) =
        if disposing then GC.SuppressFinalize(x)
        let o = Interlocked.Exchange(&handle.Handle, 0n)
        if o <> 0n then 
            //for h in loss.Values do
            //    CeresRaw.cReleaseLossFunction(h)
            loss.Clear()

            for (f, gc, cf) in costFunctions do
                gc.Free()
                cf.Dispose()
                //CeresRaw.cReleaseCostFunction f
            costFunctions.Clear()
            
            CeresRaw.cReleaseProblem(CeresProblem(o))

    member x.Dispose() = x.Dispose true
    override x.Finalize() = x.Dispose false

    interface IDisposable with
        member x.Dispose() = x.Dispose true

