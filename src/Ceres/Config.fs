namespace CeresSharp

open Aardvark.Base
open CeresSharp.Raw

type SolverType =
    | DenseNormalCholesky
    | DenseQr
    | SparseNormalCholesky
    | DenseSchur
    | SparseSchur
    | IterativeSchur
    | CGNR


type Config =
    {
        maxIterations       : int
        solverType          : SolverType
        print               : bool
        gradientTolerance   : float
        functionTolerance   : float
        parameterTolerance  : float 
    }


module Config =
    let empty =
        {
            maxIterations       = 100
            solverType          = SolverType.DenseQr
            print               = true
            gradientTolerance   = 1E-15
            functionTolerance   = 1E-15
            parameterTolerance  = 1E-15 
        }

    let private toSolverType =
        LookupTable.lookup [
            SolverType.DenseNormalCholesky , CeresSolverType.DenseNormalCholesky 
            SolverType.DenseQr             , CeresSolverType.DenseQr             
            SolverType.SparseNormalCholesky, CeresSolverType.SparseNormalCholesky
            SolverType.DenseSchur          , CeresSolverType.DenseSchur          
            SolverType.SparseSchur         , CeresSolverType.SparseSchur         
            SolverType.IterativeSchur      , CeresSolverType.IterativeSchur      
            SolverType.CGNR                , CeresSolverType.CGNR                
        ]

    let internal toCeresOptions (c : Config) =
        CeresOptions(
            c.maxIterations,
            toSolverType c.solverType,
            c.print,
            c.gradientTolerance,
            c.functionTolerance,
            c.parameterTolerance
        )

type LossFunction =
    | TrivialLoss
    | HuberLoss of float
    | SoftLOneLoss of float
    | CauchyLoss of float
    | ArcTanLoss of float
    | TolerantLoss of float * float

module LossFunction =
    let internal toCeresLossFunction (f : LossFunction) =
        match f with
            | TrivialLoss -> CeresLossFunction.Trivial
            | HuberLoss a -> CeresLossFunction.Huber a
            | SoftLOneLoss a -> CeresLossFunction.SoftLOne a
            | CauchyLoss a -> CeresLossFunction.Cauchy a
            | ArcTanLoss a -> CeresLossFunction.ArcTan a
            | TolerantLoss(a,b) -> CeresLossFunction.Tolerant(a,b)