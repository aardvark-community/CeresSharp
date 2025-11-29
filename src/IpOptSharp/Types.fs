namespace IpOptSharp

/// <summary>
/// Status codes returned by IPOPT after solving an optimization problem.
/// These codes indicate whether the optimization succeeded and provide information about
/// the termination condition.
/// </summary>
type IpOptStatus =
    /// Generic success status
    | Success = 0
    /// Problem solved to acceptable tolerance
    | Solved = 1
    /// Infeasible problem detected during optimization
    | InfeasibleProblemDetected = 2
    /// Search direction became too small to continue
    | SearchDirectionBecomesTooSmall = 3
    /// Iterates are diverging
    | DivergingIterates = 4
    /// User requested termination via callback
    | UserRequestedStop = 5
    /// Feasible point found (relaxed tolerance)
    | FeasiblePointFound = 6
    /// Maximum number of iterations exceeded
    | MaximumIterationsExceeded = -1
    /// Restoration phase failed to find feasible point
    | RestorationFailed = -2
    /// Error occurred during step computation
    | ErrorInStepComputation = -3
    /// Maximum CPU time limit exceeded
    | MaximumCpuTimeExceeded = -4
    /// Problem does not have enough degrees of freedom
    | NotEnoughDegreesOfFreedom = -10
    /// Invalid problem definition provided
    | InvalidProblemDefinition = -11
    /// Invalid solver option specified
    | InvalidOption = -12
    /// NaN or Inf detected in function evaluation
    | InvalidNumberDetected = -13
    /// Unrecoverable exception occurred
    | UnrecoverableException = -100
    /// Non-IPOPT exception was thrown
    | NonIpoptExceptionThrown = -101
    /// Insufficient memory available
    | InsufficientMemory = -102
    /// Internal IPOPT error
    | InternalError = -199

/// <summary>
/// Configuration options for the IPOPT solver.
/// These options control various aspects of the optimization algorithm including
/// convergence tolerances, iteration limits, and solver behavior.
/// </summary>
[<RequireQualifiedAccess>]
type IpOptOption =
    // Termination criteria
    /// Maximum number of iterations (default: 3000)
    | MaxIterations of int
    /// Desired convergence tolerance (relative) (default: 1e-8)
    | Tolerance of float
    /// Acceptable convergence tolerance (relative) (default: 1e-6)
    | AcceptableTolerance of float
    /// Threshold for acceptable level of constraint violation (default: 1e-2)
    | AcceptableConstraintViolationTolerance of float
    /// Number of iterations with acceptable level of convergence before stopping (default: 15)
    | AcceptableIterations of int
    /// Maximum CPU time in seconds (default: 1e6)
    | CpuTimeLimit of float
    /// Maximum wall-clock time in seconds (default: 1e6)
    | MaxWallTime of float

    // Convergence tolerances
    /// Desired threshold for dual infeasibility (default: 1e-8)
    | DualInfeasibilityTolerance of float
    /// Desired threshold for constraint violation (default: 1e-4)
    | ConstraintViolationTolerance of float
    /// Desired threshold for complementarity conditions (default: 1e-4)
    | ComplementarityTolerance of float

    // Output and printing
    /// Output verbosity level: 0=no output, 12=detailed (default: 5)
    | PrintLevel of int
    /// File path for solver output
    | OutputFile of string
    /// Print info string at end of iteration (default: "yes")
    | PrintInfoString of string
    /// Printout of solution at final point (default: "no")
    | PrintUserOptions of string

    // Linear solver
    /// Linear solver to use (e.g., "mumps", "ma27", "ma57", "ma86", "ma97", "pardiso")
    | LinearSolver of string
    /// Enable scaling of linear system (default: "yes")
    | LinearSystemScaling of string

    // Barrier parameter
    /// Barrier parameter update strategy (e.g., "monotone", "adaptive") (default: "monotone")
    | BarrierParameterStrategy of string
    /// Initial value for barrier parameter (default: 0.1)
    | BarrierParameterInitial of float
    /// Maximum value for barrier parameter (default: 1e5)
    | BarrierParameterMaximum of float
    /// Minimum value for barrier parameter (default: 1e-11)
    | BarrierParameterMinimum of float
    /// Factor for barrier parameter reduction (default: 0.2)
    | BarrierParameterLinearDecreaseFactor of float
    /// Superlinear decrease power for barrier parameter (default: 1.5)
    | BarrierParameterSuperlinearDecreasePower of float

    // Hessian approximation
    /// Indicates what Hessian information is to be used (default: "exact")
    | HessianApproximation of string
    /// Size of limited memory for quasi-Newton approximation (default: 6)
    | LimitedMemoryMaxHistory of int

    // NLP scaling
    /// Scaling strategy for nonlinear program (default: "gradient-based")
    | NonlinearProgramScalingMethod of string
    /// Maximum gradient after nonlinear program scaling (default: 100.0)
    | NonlinearProgramScalingMaxGradient of float
    /// Target value for objective function scaling (default: 0.0)
    | ObjectiveScalingFactor of float

    // Derivative checking
    /// Derivative test option (e.g., "none", "first-order", "second-order", "only-second-order")
    | DerivativeTest of string
    /// Threshold for acceptable derivative test (default: 1e-4)
    | DerivativeTestTolerance of float
    /// Print violations of derivative test (default: "no")
    | DerivativeTestPrintAll of string

    // Warm start
    /// Indicates whether to use warm start initialization (default: "no")
    | WarmStartInitializationPoint of string
    /// Threshold to trigger warm start initialization (default: 1e-8)
    | WarmStartBoundPush of float
    /// Threshold to trigger warm start initialization for slack variables (default: 1e-8)
    | WarmStartSlackBoundPush of float
    /// Threshold to trigger warm start initialization for multipliers (default: 1e-8)
    | WarmStartMultiplierBoundPush of float

    // Step calculation
    /// Desired minimum relative change of barrier parameter (default: 0.2)
    | BarrierToleranceFactor of float
    /// Maximum number of second order correction trial steps (default: 1)
    | MaxSecondOrderCorrection of int
    /// Algorithm for solving the bound-multiplier constraint system (default: "mult-norm-pd")
    | BoundMultiplierMethod of string

    // Restoration phase
    /// Strategy for restoration phase (default: "adaptive")
    | StartWithRestoration of string
    /// Trigger restoration phase when constraint violation exceeds this (default: 1e-4)
    | RequiredInfeasibilityReduction of float

    // Quasi-Newton updates
    /// Type of quasi-Newton update formula (default: "bfgs")
    | QuasiNewtonUpdateType of string

    // Custom options (for any option not explicitly listed)
    /// Custom integer option (keyword, value)
    | Int of string * int
    /// Custom floating-point option (keyword, value)
    | Float of string * float
    /// Custom string option (keyword, value)
    | String of string * string
    