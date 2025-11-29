namespace IpOptSharp

open Aardvark.Base
open IpOptSharp.IpOptBuilderImplementation

/// Static class providing solver options and common geometric constraints.
/// Use IpOpt.MaxIterations, IpOpt.Tolerance, etc. to configure the solver.
/// Use IpOpt.Perpendicular, IpOpt.Parallel, etc. for geometric constraints.
type IpOpt private() =
    // Termination criteria options

    /// Sets the maximum number of iterations
    static member MaxIterations(value : int) : IpOptOption =
        IpOptOption.MaxIterations value
    /// Sets the desired convergence tolerance (relative)
    static member Tolerance(value : float) : IpOptOption =
        IpOptOption.Tolerance value
    /// Sets the acceptable convergence tolerance (relative)
    static member AcceptableTolerance(value : float) : IpOptOption =
        IpOptOption.AcceptableTolerance value
    /// Sets the threshold for acceptable level of constraint violation
    static member AcceptableConstraintViolationTolerance(value : float) : IpOptOption =
        IpOptOption.AcceptableConstraintViolationTolerance value
    /// Sets the number of iterations with acceptable level before stopping
    static member AcceptableIterations(value : int) : IpOptOption =
        IpOptOption.AcceptableIterations value
    /// Sets the maximum CPU time in seconds
    static member CpuTimeLimit(value : float) : IpOptOption =
        IpOptOption.CpuTimeLimit value
    /// Sets the maximum wall-clock time in seconds
    static member MaxWallTime(value : float) : IpOptOption =
        IpOptOption.MaxWallTime value

    // Convergence tolerance options

    /// Sets the threshold for dual infeasibility
    static member DualInfeasibilityTolerance(value : float) : IpOptOption =
        IpOptOption.DualInfeasibilityTolerance value
    /// Sets the threshold for constraint violation
    static member ConstraintViolationTolerance(value : float) : IpOptOption =
        IpOptOption.ConstraintViolationTolerance value
    /// Sets the threshold for complementarity conditions
    static member ComplementarityTolerance(value : float) : IpOptOption =
        IpOptOption.ComplementarityTolerance value

    // Output and printing options

    /// Sets the output verbosity level (0=none, 12=detailed)
    static member PrintLevel(value : int) : IpOptOption =
        IpOptOption.PrintLevel value
    /// Sets the output file path for solver diagnostics
    static member OutputFile(value : string) : IpOptOption =
        IpOptOption.OutputFile value
    /// Sets whether to print info string at end of iteration
    static member PrintInfoString(value : string) : IpOptOption =
        IpOptOption.PrintInfoString value
    /// Sets whether to print user options at final point
    static member PrintUserOptions(value : string) : IpOptOption =
        IpOptOption.PrintUserOptions value

    // Linear solver options

    /// Sets the linear solver to use
    static member LinearSolver(value : string) : IpOptOption =
        IpOptOption.LinearSolver value
    /// Sets whether to enable scaling of linear system
    static member LinearSystemScaling(value : string) : IpOptOption =
        IpOptOption.LinearSystemScaling value

    // Barrier parameter options

    /// Sets the barrier parameter update strategy
    static member BarrierParameterStrategy(value : string) : IpOptOption =
        IpOptOption.BarrierParameterStrategy value
    /// Sets the initial value for barrier parameter
    static member BarrierParameterInitial(value : float) : IpOptOption =
        IpOptOption.BarrierParameterInitial value
    /// Sets the maximum value for barrier parameter
    static member BarrierParameterMaximum(value : float) : IpOptOption =
        IpOptOption.BarrierParameterMaximum value
    /// Sets the minimum value for barrier parameter
    static member BarrierParameterMinimum(value : float) : IpOptOption =
        IpOptOption.BarrierParameterMinimum value
    /// Sets the factor for barrier parameter reduction
    static member BarrierParameterLinearDecreaseFactor(value : float) : IpOptOption =
        IpOptOption.BarrierParameterLinearDecreaseFactor value
    /// Sets the superlinear decrease power for barrier parameter
    static member BarrierParameterSuperlinearDecreasePower(value : float) : IpOptOption =
        IpOptOption.BarrierParameterSuperlinearDecreasePower value

    // Hessian approximation options

    /// Sets what Hessian information is to be used
    static member HessianApproximation(value : string) : IpOptOption =
        IpOptOption.HessianApproximation value
    /// Sets the size of limited memory for quasi-Newton approximation
    static member LimitedMemoryMaxHistory(value : int) : IpOptOption =
        IpOptOption.LimitedMemoryMaxHistory value

    // NLP scaling options

    /// Sets the scaling strategy for nonlinear program
    static member NonlinearProgramScalingMethod(value : string) : IpOptOption =
        IpOptOption.NonlinearProgramScalingMethod value
    /// Sets the maximum gradient after nonlinear program scaling
    static member NonlinearProgramScalingMaxGradient(value : float) : IpOptOption =
        IpOptOption.NonlinearProgramScalingMaxGradient value
    /// Sets the target value for objective function scaling
    static member ObjectiveScalingFactor(value : float) : IpOptOption =
        IpOptOption.ObjectiveScalingFactor value

    // Derivative checking options

    /// Sets the derivative test option
    static member DerivativeTest(value : string) : IpOptOption =
        IpOptOption.DerivativeTest value
    /// Sets the threshold for acceptable derivative test
    static member DerivativeTestTolerance(value : float) : IpOptOption =
        IpOptOption.DerivativeTestTolerance value
    /// Sets whether to print all derivative test violations
    static member DerivativeTestPrintAll(value : string) : IpOptOption =
        IpOptOption.DerivativeTestPrintAll value

    // Warm start options

    /// Sets whether to use warm start initialization
    static member WarmStartInitializationPoint(value : string) : IpOptOption =
        IpOptOption.WarmStartInitializationPoint value
    /// Sets the threshold to trigger warm start initialization
    static member WarmStartBoundPush(value : float) : IpOptOption =
        IpOptOption.WarmStartBoundPush value
    /// Sets the threshold for warm start slack variables
    static member WarmStartSlackBoundPush(value : float) : IpOptOption =
        IpOptOption.WarmStartSlackBoundPush value
    /// Sets the threshold for warm start multipliers
    static member WarmStartMultiplierBoundPush(value : float) : IpOptOption =
        IpOptOption.WarmStartMultiplierBoundPush value

    // Step calculation options

    /// Sets the desired minimum relative change of barrier parameter
    static member BarrierToleranceFactor(value : float) : IpOptOption =
        IpOptOption.BarrierToleranceFactor value
    /// Sets the maximum number of second order correction trial steps
    static member MaxSecondOrderCorrection(value : int) : IpOptOption =
        IpOptOption.MaxSecondOrderCorrection value
    /// Sets the algorithm for solving bound-multiplier constraint system
    static member BoundMultiplierMethod(value : string) : IpOptOption =
        IpOptOption.BoundMultiplierMethod value

    // Restoration phase options

    /// Sets the strategy for restoration phase
    static member StartWithRestoration(value : string) : IpOptOption =
        IpOptOption.StartWithRestoration value
    /// Sets when to trigger restoration phase
    static member RequiredInfeasibilityReduction(value : float) : IpOptOption =
        IpOptOption.RequiredInfeasibilityReduction value

    // Quasi-Newton options

    /// Sets the type of quasi-Newton update formula
    static member QuasiNewtonUpdateType(value : string) : IpOptOption =
        IpOptOption.QuasiNewtonUpdateType value

    // Custom options

    /// Sets a custom integer option
    static member Int(key : string, value : int) : IpOptOption =
        IpOptOption.Int(key, value)
    /// Sets a custom floating-point option
    static member Float(key : string, value : float) : IpOptOption =
        IpOptOption.Float(key, value)
    /// Sets a custom string option
    static member String(key : string, value : string) : IpOptOption =
        IpOptOption.String(key, value)

    // Constraint helper functions

    /// Creates an equality constraint: a == b
    static member inline Equal(a, b) = equal a b
    /// Creates an inequality constraint: a >= b
    static member inline GreaterEqual(a, b) = greaterEqual a b
    /// Creates an inequality constraint: a <= b
    static member inline LessEqual(a, b) = lessEqual a b
    /// Creates a range constraint: l <= a <= h
    static member inline Range(a, l, h) = range a l h

    // Geometric constraint helpers

    /// Constrains two 2D vectors to be perpendicular (dot product = 0)
    static member inline Perpendicular(v1 : V2s, v2 : V2s) =
        [|
            Constraint(v1.X * v2.X + v1.Y * v2.Y, 0.0, 0.0)
        |]

    /// Constrains two 3D vectors to be perpendicular (dot product = 0)
    static member inline Perpendicular(v1 : V3s, v2 : V3s) =
        [|
            Constraint(v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z, 0.0, 0.0)
        |]

    /// Constrains two 4D vectors to be perpendicular (dot product = 0)
    static member inline Perpendicular(v1 : V4s, v2 : V4s) =
        [|
            Constraint(v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z + v1.W * v2.W, 0.0, 0.0)
        |]

    /// Constrains two 2D vectors to be parallel (cross product = 0)
    static member inline Parallel(v1 : V2s, v2 : V2s) =
        [|
            Constraint(v1.X * v2.Y - v1.Y * v2.X, 0.0, 0.0)
        |]

    /// Constrains two 3D vectors to be parallel (cross product = 0)
    static member inline Parallel(v1 : V3s, v2 : V3s) =
        [|
            Constraint(v1.Y * v2.Z - v1.Z * v2.Y, 0.0, 0.0)
            Constraint(v1.Z * v2.X - v1.X * v2.Z, 0.0, 0.0)
            Constraint(v1.X * v2.Y - v1.Y * v2.X, 0.0, 0.0)
        |]

    /// Constrains two 4D vectors to be parallel (all 2x2 minors = 0)
    static member inline Parallel(v1 : V4s, v2 : V4s) =
        [|
            Constraint(v1.X * v2.Y - v1.Y * v2.X, 0.0, 0.0)
            Constraint(v1.X * v2.Z - v1.Z * v2.X, 0.0, 0.0)
            Constraint(v1.X * v2.W - v1.W * v2.X, 0.0, 0.0)
            Constraint(v1.Y * v2.Z - v1.Z * v2.Y, 0.0, 0.0)
            Constraint(v1.Y * v2.W - v1.W * v2.Y, 0.0, 0.0)
            Constraint(v1.Z * v2.W - v1.W * v2.Z, 0.0, 0.0)
        |]

    /// Constrains matrix rows to be orthogonal (2x2 matrix)
    static member inline Orthogonal(m : M22s) =
        [|
            Constraint(m.M00 * m.M10 + m.M01 * m.M11, 0.0, 0.0)
        |]

    /// Constrains matrix rows to be pairwise orthogonal (3x3 matrix)
    static member inline Orthogonal(m : M33s) =
        [|
            Constraint(m.M00 * m.M10 + m.M01 * m.M11 + m.M02 * m.M12, 0.0, 0.0)
            Constraint(m.M00 * m.M20 + m.M01 * m.M21 + m.M02 * m.M22, 0.0, 0.0)
            Constraint(m.M10 * m.M20 + m.M11 * m.M21 + m.M12 * m.M22, 0.0, 0.0)
        |]

    /// Constrains matrix rows to be orthonormal (orthogonal + unit length) for 2x2 matrix.
    /// Allows both rotations (det=1) and reflections (det=-1).
    static member inline Orthonormal(m : M22s) =
        [|
            Constraint(sqr m.M00 + sqr m.M01, 1.0, 1.0)  // Row 0 has unit length
            Constraint(sqr m.M10 + sqr m.M11, 1.0, 1.0)  // Row 1 has unit length
            Constraint(m.M00 * m.M10 + m.M01 * m.M11, 0.0, 0.0)  // Rows orthogonal
        |]

    /// Constrains matrix rows to be orthonormal (orthogonal + unit length) for 3x3 matrix.
    /// Allows both rotations (det=1) and reflections (det=-1).
    static member inline Orthonormal(m : M33s) =
        [|
            Constraint(sqr m.M00 + sqr m.M01 + sqr m.M02, 1.0, 1.0)  // Row 0 has unit length
            Constraint(sqr m.M10 + sqr m.M11 + sqr m.M12, 1.0, 1.0)  // Row 1 has unit length
            Constraint(sqr m.M20 + sqr m.M21 + sqr m.M22, 1.0, 1.0)  // Row 2 has unit length
            Constraint(m.M00 * m.M10 + m.M01 * m.M11 + m.M02 * m.M12, 0.0, 0.0)  // Rows 0,1 orthogonal
            Constraint(m.M00 * m.M20 + m.M01 * m.M21 + m.M02 * m.M22, 0.0, 0.0)  // Rows 0,2 orthogonal
            Constraint(m.M10 * m.M20 + m.M11 * m.M21 + m.M12 * m.M22, 0.0, 0.0)  // Rows 1,2 orthogonal
        |]

    /// Constrains a 2x2 matrix to be a proper rotation (orthonormal with det=1).
    /// Uses minimal constraints: orthonormality + determinant = 1.
    static member inline Rotation(m : M22s) =
        [|
            Constraint(sqr m.M00 + sqr m.M01, 1.0, 1.0)  // Row 0 has unit length
            Constraint(sqr m.M10 + sqr m.M11, 1.0, 1.0)  // Row 1 has unit length
            Constraint(m.M00 * m.M10 + m.M01 * m.M11, 0.0, 0.0)  // Rows orthogonal
            Constraint(m.M00 * m.M11 - m.M01 * m.M10, 1.0, 1.0)  // det = 1
        |]

    /// Constrains a 3x3 matrix to be a proper rotation (orthonormal with det=1).
    /// Uses minimal constraints: orthonormality + determinant = 1.
    static member inline Rotation(m : M33s) =
        let det =
            m.M00 * (m.M11 * m.M22 - m.M12 * m.M21) -
            m.M01 * (m.M10 * m.M22 - m.M12 * m.M20) +
            m.M02 * (m.M10 * m.M21 - m.M11 * m.M20)
        [|
            Constraint(sqr m.M00 + sqr m.M01 + sqr m.M02, 1.0, 1.0)  // Row 0 has unit length
            Constraint(sqr m.M10 + sqr m.M11 + sqr m.M12, 1.0, 1.0)  // Row 1 has unit length
            Constraint(sqr m.M20 + sqr m.M21 + sqr m.M22, 1.0, 1.0)  // Row 2 has unit length
            Constraint(m.M00 * m.M10 + m.M01 * m.M11 + m.M02 * m.M12, 0.0, 0.0)  // Rows 0,1 orthogonal
            Constraint(m.M00 * m.M20 + m.M01 * m.M21 + m.M02 * m.M22, 0.0, 0.0)  // Rows 0,2 orthogonal
            Constraint(m.M10 * m.M20 + m.M11 * m.M21 + m.M12 * m.M22, 0.0, 0.0)  // Rows 1,2 orthogonal
            Constraint(det, 1.0, 1.0)  // det = 1 (proper rotation, not reflection)
        |]


    /// Creates an objective to minimize the given scalar expression
    static member inline Minimize(a) = minimize a
    /// Creates an objective to maximize the given scalar expression
    static member inline Maximize(a) = maximize a


