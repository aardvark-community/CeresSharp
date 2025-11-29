namespace IpOptSharp

open System
open System.Runtime.InteropServices
open Aardvark.Base.CodeFragment
open Microsoft.FSharp.NativeInterop
open Aardvark.Base
open IpOptSharp.Native

#nowarn "9"      // Disable warning for use of native pointers
#nowarn "1337"   // Disable warning for inline IL

/// Implementation module containing the computation expression builder for IPOPT optimization.
/// Provides a fluent API for defining optimization problems with support for automatic differentiation
/// through the scalar type system.
module IpOptBuilderImplementation =

    /// Adapter interface that bridges between concrete value types ('a) and their scalar-variable
    /// representations ('b) used in automatic differentiation.
    /// This allows the builder to work uniformly with floats, vectors, matrices, and transformations.
    type ScalarAdapter<'a, 'b> =
        /// Number of floating-point values needed to represent this type
        abstract DoubleCount : int
        /// Reads scalar variables from memory, creating automatic differentiation variables
        abstract ReadScalar : nativeptr<float> * int -> 'b
        /// Reads concrete values from memory without differentiation tracking
        abstract ReadValue : nativeptr<float> * int -> 'a
        /// Writes concrete values to memory
        abstract WriteTo : nativeptr<float> * int * 'a -> unit
        /// Extracts the concrete value from a scalar-variable type
        abstract GetValue : 'b -> 'a
        /// Creates a scalar-variable from a concrete value and variable index
        abstract Variable : 'a * int -> 'b

    /// Provides scalar adapter instances for common types through static member overloading.
    /// This enables type-directed adapter resolution via SRTP (Statically Resolved Type Parameters).
    type ScalarAdapterInstances() =
        /// Scalar adapter for single float values (1 optimization variable)
        static member GetScalarAdapter (_dummy : float) =
            { new ScalarAdapter<float, scalar> with
                member x.DoubleCount = 1
                member x.ReadScalar(ptr, id) = scalar.Variable(id, NativePtr.get ptr id)
                member x.ReadValue(ptr, id) = NativePtr.get ptr id
                member x.WriteTo(ptr, id, value) = NativePtr.set ptr id value
                member x.GetValue(s) = s.Value
                member x.Variable(value, id) = scalar.Variable(id, value)
            }

        /// Scalar adapter for 2D vectors (2 optimization variables)
        static member GetScalarAdapter (_dummy : V2d) =
            { new ScalarAdapter<V2d, V2s> with
                member x.DoubleCount = 2
                member x.ReadScalar(ptr, id) = V2s(scalar.Variable(id, NativePtr.get ptr id), scalar.Variable(id+1, NativePtr.get ptr (id+1)))
                member x.ReadValue(ptr, id) = V2d(NativePtr.get ptr id, NativePtr.get ptr (id+1))
                member x.WriteTo(ptr, id, value) =
                    NativePtr.set ptr id value.X
                    NativePtr.set ptr (id+1) value.Y
                member x.GetValue(s) = s.Value
                member x.Variable(value, id) = V2s(scalar.Variable(id, value.X), scalar.Variable(id+1, value.Y))
            }

        /// Scalar adapter for 3D vectors (3 optimization variables)
        static member GetScalarAdapter (_dummy : V3d) =
            { new ScalarAdapter<V3d, V3s> with
                member x.DoubleCount = 3
                member x.ReadScalar(ptr, id) = V3s(scalar.Variable(id, NativePtr.get ptr id), scalar.Variable(id+1, NativePtr.get ptr (id+1)), scalar.Variable(id+2, NativePtr.get ptr (id+2)))
                member x.ReadValue(ptr, id) = V3d(NativePtr.get ptr id, NativePtr.get ptr (id+1), NativePtr.get ptr (id+2))
                member x.WriteTo(ptr, id, value) =
                    NativePtr.set ptr id value.X
                    NativePtr.set ptr (id+1) value.Y
                    NativePtr.set ptr (id+2) value.Z
                member x.GetValue(s) = s.Value
                member x.Variable(value, id) = V3s(scalar.Variable(id, value.X), scalar.Variable(id+1, value.Y), scalar.Variable(id+2, value.Z))
            }

        /// Scalar adapter for 4D vectors (4 optimization variables)
        static member GetScalarAdapter (_dummy : V4d) =
            { new ScalarAdapter<V4d, V4s> with
                member x.DoubleCount = 4
                member x.ReadScalar(ptr, id) =
                    V4s.CeresRead(ptr, id)
                member x.ReadValue(ptr, id) =
                    V4d(
                        NativePtr.get ptr id,
                        NativePtr.get ptr (id+1),
                        NativePtr.get ptr (id+2),
                        NativePtr.get ptr (id+3)
                    )
                member x.WriteTo(ptr, id, value) =
                    NativePtr.set ptr id value.X
                    NativePtr.set ptr (id+1) value.Y
                    NativePtr.set ptr (id+2) value.Z
                    NativePtr.set ptr (id+3) value.W
                member x.GetValue(s) = s.Value
                member x.Variable(value, id) =
                    V4s(
                        scalar.Variable(id, value.X),
                        scalar.Variable(id+1, value.Y),
                        scalar.Variable(id+2, value.Z),
                        scalar.Variable(id+3, value.W)
                    )

            }

        /// Scalar adapter for 2x2 matrices (4 optimization variables)
        static member GetScalarAdapter (_dummy : M22d) =
            { new ScalarAdapter<M22d, M22s> with
                member x.DoubleCount = 4
                member x.ReadScalar(ptr, id) =
                    M22s.CeresRead(ptr, id)
                member x.ReadValue(ptr, id) =
                    M22d(
                        NativePtr.get ptr id,
                        NativePtr.get ptr (id+1),
                        NativePtr.get ptr (id+2),
                        NativePtr.get ptr (id+3)
                    )
                member x.WriteTo(ptr, id, value) =
                    NativePtr.set ptr id value.M00
                    NativePtr.set ptr (id+1) value.M01
                    NativePtr.set ptr (id+2) value.M10
                    NativePtr.set ptr (id+3) value.M11
                member x.GetValue(s) = s.Value
                member x.Variable(value, id) =
                    M22s(
                        scalar.Variable(id, value.M00),
                        scalar.Variable(id+1, value.M01),
                        scalar.Variable(id+2, value.M10),
                        scalar.Variable(id+3, value.M11)
                    )

            }

        /// Scalar adapter for 3x3 matrices (9 optimization variables)
        static member GetScalarAdapter (_dummy : M33d) =
            { new ScalarAdapter<M33d, M33s> with
                member x.DoubleCount = 9
                member x.ReadScalar(ptr, id) =
                    M33s.CeresRead(ptr, id)
                member x.ReadValue(ptr, id) =
                    M33d(
                        NativePtr.get ptr id,
                        NativePtr.get ptr (id+1),
                        NativePtr.get ptr (id+2),
                        NativePtr.get ptr (id+3),
                        NativePtr.get ptr (id+4),
                        NativePtr.get ptr (id+5),
                        NativePtr.get ptr (id+6),
                        NativePtr.get ptr (id+7),
                        NativePtr.get ptr (id+8)
                    )
                member x.WriteTo(ptr, id, value) =
                    NativePtr.set ptr id value.M00
                    NativePtr.set ptr (id+1) value.M01
                    NativePtr.set ptr (id+2) value.M02
                    NativePtr.set ptr (id+3) value.M10
                    NativePtr.set ptr (id+4) value.M11
                    NativePtr.set ptr (id+5) value.M12
                    NativePtr.set ptr (id+6) value.M20
                    NativePtr.set ptr (id+7) value.M21
                    NativePtr.set ptr (id+8) value.M22
                member x.GetValue(s) = s.Value
                member x.Variable(value, id) =
                    M33s(
                        scalar.Variable(id, value.M00),
                        scalar.Variable(id+1, value.M01),
                        scalar.Variable(id+2, value.M02),
                        scalar.Variable(id+3, value.M10),
                        scalar.Variable(id+4, value.M11),
                        scalar.Variable(id+5, value.M12),
                        scalar.Variable(id+6, value.M20),
                        scalar.Variable(id+7, value.M21),
                        scalar.Variable(id+8, value.M22)
                    )

            }

        /// Scalar adapter for 4x4 matrices (16 optimization variables)
        static member GetScalarAdapter (_dummy : M44d) =
            { new ScalarAdapter<M44d, M44s> with
                member x.DoubleCount = 16
                member x.ReadScalar(ptr, id) =
                    M44s.CeresRead(ptr, id)
                member x.ReadValue(ptr, id) =
                    M44d(
                        NativePtr.get ptr id,
                        NativePtr.get ptr (id+1),
                        NativePtr.get ptr (id+2),
                        NativePtr.get ptr (id+3),
                        NativePtr.get ptr (id+4),
                        NativePtr.get ptr (id+5),
                        NativePtr.get ptr (id+6),
                        NativePtr.get ptr (id+7),
                        NativePtr.get ptr (id+8),
                        NativePtr.get ptr (id+9),
                        NativePtr.get ptr (id+10),
                        NativePtr.get ptr (id+11),
                        NativePtr.get ptr (id+12),
                        NativePtr.get ptr (id+13),
                        NativePtr.get ptr (id+14),
                        NativePtr.get ptr (id+15)
                        
                    )
                member x.WriteTo(ptr, id, value) =
                    NativePtr.set ptr id value.M00
                    NativePtr.set ptr (id+1) value.M01
                    NativePtr.set ptr (id+2) value.M02
                    NativePtr.set ptr (id+3) value.M03
                    NativePtr.set ptr (id+4) value.M10
                    NativePtr.set ptr (id+5) value.M11
                    NativePtr.set ptr (id+6) value.M12
                    NativePtr.set ptr (id+7) value.M13
                    NativePtr.set ptr (id+8) value.M20
                    NativePtr.set ptr (id+9) value.M21
                    NativePtr.set ptr (id+10) value.M22
                    NativePtr.set ptr (id+11) value.M23
                    NativePtr.set ptr (id+12) value.M30
                    NativePtr.set ptr (id+13) value.M31
                    NativePtr.set ptr (id+14) value.M32
                    NativePtr.set ptr (id+15) value.M33
                member x.GetValue(s) = s.Value
                member x.Variable(value, id) =
                    M44s(
                        scalar.Variable(id, value.M00),
                        scalar.Variable(id+1, value.M01),
                        scalar.Variable(id+2, value.M02),
                        scalar.Variable(id+3, value.M03),
                        scalar.Variable(id+4, value.M10),
                        scalar.Variable(id+5, value.M11),
                        scalar.Variable(id+6, value.M12),
                        scalar.Variable(id+7, value.M13),
                        scalar.Variable(id+8, value.M20),
                        scalar.Variable(id+9, value.M21),
                        scalar.Variable(id+10, value.M22),
                        scalar.Variable(id+11, value.M23),
                        scalar.Variable(id+12, value.M30),
                        scalar.Variable(id+13, value.M31),
                        scalar.Variable(id+14, value.M32),
                        scalar.Variable(id+15, value.M33)
                    )

            }

        /// Scalar adapter for 2D rotations (1 optimization variable: angle in radians)
        static member GetScalarAdapter (_dummy : Rot2d) =
            { new ScalarAdapter<Rot2d, Rot2s> with
                member x.DoubleCount = 1
                member x.ReadScalar(ptr, id) = Rot2s(scalar.Variable(id, NativePtr.get ptr id))
                member x.ReadValue(ptr, id) = Rot2d(NativePtr.get ptr id)
                member x.WriteTo(ptr, id, value) = NativePtr.set ptr id value.Angle
                member x.GetValue(s) = s.Value
                member x.Variable(value, id) = Rot2s(scalar.Variable(id, value.Angle))
            }

        /// Scalar adapter for 3D rotations (3 optimization variables: angle-axis representation)
        static member GetScalarAdapter (_dummy : Rot3d) =
            { new ScalarAdapter<Rot3d, Rot3s> with
                member x.DoubleCount = 3
                member x.ReadScalar(ptr, id) =
                    let x = scalar.Variable(id, NativePtr.get ptr id)
                    let y = scalar.Variable(id+1, NativePtr.get ptr (id+1))
                    let z = scalar.Variable(id+2, NativePtr.get ptr (id+2))
                    Rot3s(V3s(x, y, z))
                    
                member x.ReadValue(ptr, id) =
                    let x = NativePtr.get ptr id
                    let y = NativePtr.get ptr (id+1)
                    let z = NativePtr.get ptr (id+2)
                    Rot3d.FromAngleAxis(V3d(x, y, z))
                    
                member x.WriteTo(ptr, id, value) =
                    let aa = value.ToAngleAxis()
                    NativePtr.set ptr id aa.X
                    NativePtr.set ptr (id+1) aa.Y
                    NativePtr.set ptr (id+2) aa.Z
                member x.GetValue(s) = s.Value
                member x.Variable(value, id) =
                    let aa = value.ToAngleAxis()
                    Rot3s(V3s(scalar.Variable(id, aa.X), scalar.Variable(id+1, aa.Y), scalar.Variable(id+2, aa.Z)))
            }

        /// Scalar adapter for 2D Euclidean transformations (3 optimization variables: rotation + translation)
        static member GetScalarAdapter (_dummy : Euclidean2d) =
            { new ScalarAdapter<Euclidean2d, Euclidean2s> with
                member x.DoubleCount = 3
                member x.ReadScalar(ptr, id) =
                    let r = Rot2s(scalar.Variable(id, NativePtr.get ptr id))
                    let t = V2s(scalar.Variable(id+1, NativePtr.get ptr (id+1)), scalar.Variable(id+2, NativePtr.get ptr (id+2)))
                    Euclidean2s(r, t)
                    
                member x.ReadValue(ptr, id) =
                    let r = Rot2d(NativePtr.get ptr id)
                    let t = V2d(NativePtr.get ptr (id+1), NativePtr.get ptr (id+2))
                    Euclidean2d(r, t)
                    
                member x.WriteTo(ptr, id, value) =
                    NativePtr.set ptr id value.Rot.Angle
                    NativePtr.set ptr (id+1) value.Trans.X
                    NativePtr.set ptr (id+2) value.Trans.Y
                    
                member x.GetValue(s) = s.Value
                member x.Variable(value, id) =
                    Euclidean2s(
                        Rot2s(scalar.Variable(id, value.Rot.Angle)),
                        V2s(scalar.Variable(id+1, value.Trans.X), scalar.Variable(id+2, value.Trans.Y))
                    )
            }

        /// Scalar adapter for 3D Euclidean transformations (6 optimization variables: rotation + translation)
        static member GetScalarAdapter (_dummy : Euclidean3d) =
            { new ScalarAdapter<Euclidean3d, Euclidean3s> with
                member x.DoubleCount = 6
                member x.ReadScalar(ptr, id) =
                    let rx = scalar.Variable(id, NativePtr.get ptr id)
                    let ry = scalar.Variable(id+1, NativePtr.get ptr (id+1))
                    let rz = scalar.Variable(id+2, NativePtr.get ptr (id+2))
                    let r = Rot3s(V3s(rx, ry, rz))
                    let t =
                        V3s(
                            scalar.Variable(id+3, NativePtr.get ptr (id+3)),
                            scalar.Variable(id+4, NativePtr.get ptr (id+4)),
                            scalar.Variable(id+5, NativePtr.get ptr (id+5))
                        )
                    Euclidean3s(r, t)
                    
                member x.ReadValue(ptr, id) =
                    let rx = NativePtr.get ptr id
                    let ry = NativePtr.get ptr (id+1)
                    let rz = NativePtr.get ptr (id+2)
                    
                    let r = Rot3d.FromAngleAxis(V3d(rx, ry, rz))
                    let t =
                        V3d(
                            NativePtr.get ptr (id+3),
                            NativePtr.get ptr (id+4),
                            NativePtr.get ptr (id+5)
                        )
                    Euclidean3d(r, t)
                    
                member x.WriteTo(ptr, id, value) =
                    let aa = value.Rot.ToAngleAxis()
                    NativePtr.set ptr id aa.X
                    NativePtr.set ptr (id+1) aa.Y
                    NativePtr.set ptr (id+2) aa.Z
                    NativePtr.set ptr (id+3) value.Trans.X
                    NativePtr.set ptr (id+4) value.Trans.Y
                    NativePtr.set ptr (id+5) value.Trans.Z
                    
                member x.GetValue(s) = s.Value
                
                member x.Variable(value, id) =
                    let aa = value.Rot.ToAngleAxis()
                    Euclidean3s(
                        Rot3s(
                            V3s(
                                scalar.Variable(id, aa.X),
                                scalar.Variable(id+1, aa.Y),
                                scalar.Variable(id+2, aa.Z)
                            )
                        ),
                        V3s(
                            scalar.Variable(id+3, value.Trans.X),
                            scalar.Variable(id+4, value.Trans.Y),
                            scalar.Variable(id+5, value.Trans.Z)
                        )
                    )
            }

        /// Scalar adapter for 2D similarity transformations (4 optimization variables: rotation + translation + scale)
        static member GetScalarAdapter (_dummy : Similarity2d) =
            { new ScalarAdapter<Similarity2d, Similarity2s> with
                member x.DoubleCount = 4
                member x.ReadScalar(ptr, id) =
                    let r = Rot2s(scalar.Variable(id, NativePtr.get ptr id))
                    let t = V2s(scalar.Variable(id+1, NativePtr.get ptr (id+1)), scalar.Variable(id+2, NativePtr.get ptr (id+2)))
                    let sqrtScale = scalar.Variable(id+3, NativePtr.get ptr (id+3))
                    Similarity2s(Euclidean2s(r, t), sqrtScale)
                    
                member x.ReadValue(ptr, id) =
                    let r = Rot2d(NativePtr.get ptr id)
                    let t = V2d(NativePtr.get ptr (id+1), NativePtr.get ptr (id+2))
                    let sqrtScale = NativePtr.get ptr (id+3)
                    Similarity2d(sqr sqrtScale, Euclidean2d(r, t))
                    
                member x.WriteTo(ptr, id, value) =
                    NativePtr.set ptr id value.Rot.Angle
                    NativePtr.set ptr (id+1) value.Trans.X
                    NativePtr.set ptr (id+2) value.Trans.Y
                    NativePtr.set ptr (id+3) (sqrt value.Scale)
                    
                member x.GetValue(s) =
                    Similarity2d(s.Scale.Value, s.EuclideanTransformation.Value)
                    
                member x.Variable(value, id) =
                    Similarity2s(
                        Euclidean2s(
                            Rot2s(scalar.Variable(id, value.Rot.Angle)),
                            V2s(scalar.Variable(id+1, value.Trans.X), scalar.Variable(id+2, value.Trans.Y))
                        ),
                        scalar.Variable(id+3, sqrt value.Scale)
                    )
            }

        /// Scalar adapter for 3D similarity transformations (7 optimization variables: rotation + translation + scale)
        static member GetScalarAdapter (_dummy : Similarity3d) =
            { new ScalarAdapter<Similarity3d, Similarity3s> with
                member x.DoubleCount = 7
                member x.ReadScalar(ptr, id) =
                    let rx = scalar.Variable(id, NativePtr.get ptr id)
                    let ry = scalar.Variable(id+1, NativePtr.get ptr (id+1))
                    let rz = scalar.Variable(id+2, NativePtr.get ptr (id+2))
                    let r = Rot3s(V3s(rx, ry, rz))
                    let t =
                        V3s(
                            scalar.Variable(id+3, NativePtr.get ptr (id+3)),
                            scalar.Variable(id+4, NativePtr.get ptr (id+4)),
                            scalar.Variable(id+5, NativePtr.get ptr (id+5))
                        )
                    let sqrtScale = scalar.Variable(id+6, NativePtr.get ptr (id+6))
                    Similarity3s(Euclidean3s(r, t), sqrtScale)
                    
                member x.ReadValue(ptr, id) =
                    let rx = NativePtr.get ptr id
                    let ry = NativePtr.get ptr (id+1)
                    let rz = NativePtr.get ptr (id+2)
                    
                    let r = Rot3d.FromAngleAxis(V3d(rx, ry, rz))
                    let t =
                        V3d(
                            NativePtr.get ptr (id+3),
                            NativePtr.get ptr (id+4),
                            NativePtr.get ptr (id+5)
                        )
                    let sqrtScale = NativePtr.get ptr (id+6)
                    Similarity3d(sqr sqrtScale, Euclidean3d(r, t))
                    
                member x.WriteTo(ptr, id, value) =
                    let aa = value.Rot.ToAngleAxis()
                    NativePtr.set ptr id aa.X
                    NativePtr.set ptr (id+1) aa.Y
                    NativePtr.set ptr (id+2) aa.Z
                    NativePtr.set ptr (id+3) value.Trans.X
                    NativePtr.set ptr (id+4) value.Trans.Y
                    NativePtr.set ptr (id+5) value.Trans.Z
                    NativePtr.set ptr (id+6) (sqrt value.Scale)
                    
                member x.GetValue(s) =
                    Similarity3d(s.Scale.Value, s.EuclideanTransformation.Value)
                
                member x.Variable(value, id) =
                    let aa = value.Rot.ToAngleAxis()
                    Similarity3s(
                        Euclidean3s(
                            Rot3s(
                                V3s(
                                    scalar.Variable(id, aa.X),
                                    scalar.Variable(id+1, aa.Y),
                                    scalar.Variable(id+2, aa.Z)
                                )
                            ),
                            V3s(
                                scalar.Variable(id+3, value.Trans.X),
                                scalar.Variable(id+4, value.Trans.Y),
                                scalar.Variable(id+5, value.Trans.Z)
                            )
                        ),
                        scalar.Variable(id+6, sqrt value.Scale)
                    )
            }
         
        
         
        // static member inline GetScalarAdapter (value : ^a[])  =
        //     let inner = scalarAdapterAux Unchecked.defaultof<ScalarAdapterInstances> value.[0]
        //     { new ScalarAdapter<^a[], _> with
        //         member x.DoubleCount = value.Length * inner.DoubleCount
        //         member x.ReadScalar(ptr, id) =
        //             let res = Array.zeroCreate value.Length
        //             let mutable id = id
        //             for i in 0 .. value.Length - 1 do
        //                 res.[i] <- inner.Variable(inner.ReadValue(ptr, id), id)
        //                 id <- id + inner.DoubleCount
        //             res
        //         member x.ReadValue(ptr, id) = failwith ""
        //         member x.WriteTo(ptr, id, value) = failwith ""
        //         member x.GetValue(s) =
        //             s |> Array.map inner.GetValue
        //         member x.Variable(value, id) = failwith ""
        //     }
           
    /// <summary>
    /// SRTP-based adapter resolution helper.
    /// Uses static member constraints to find the appropriate ScalarAdapter for a type.
    /// </summary>
    let inline scalarAdapterAux< ^a, ^b, ^c when (^a or ^b or ^c) : (static member GetScalarAdapter : ^a -> ScalarAdapter<^a, ^b>) > (_ : ^c) (dummy : ^a)  =
        ((^a or ^b or ^c) : (static member GetScalarAdapter : ^a -> ScalarAdapter<^a, ^b>) (dummy))

    /// <summary>
    /// Gets the scalar adapter for a given type instance.
    /// This function uses SRTP to resolve the correct adapter at compile time.
    /// </summary>
    let inline scalarAdapter dummy =
        scalarAdapterAux Unchecked.defaultof<ScalarAdapterInstances> dummy

    /// <summary>
    /// Represents a single optimization constraint as: lower_bound &lt;= scalar_expression &lt;= upper_bound
    /// </summary>
    [<Struct>]
    type Constraint = Constraint of expression : scalar * lowerBound : float * upperBound : float

    /// <summary>
    /// Internal state of the computation expression builder.
    /// Tracks optimization variables, constraints, objectives, and callback functions.
    /// </summary>
    type IpOptBuilderState =
        {
            /// True when evaluating the objective/constraints (not building the problem)
            Evaluation : bool
            /// Callbacks to update scalar variable values from native memory
            UpdateScalars : list<nativeptr<float> -> unit>
            /// Callbacks to update input variables from optimized values
            UpdateInput : list<nativeptr<float> -> unit>
            /// Callbacks to write initial values to native memory
            WriteTo : list<nativeptr<float> -> unit>
            /// Current offset in the flat variable array
            VariableOffset : int
            /// The objective function to minimize/maximize
            Objective : scalar
            /// True if maximizing (negate objective for IPOPT which only minimizes)
            NegateObjective : bool
            /// List of constraint arrays
            Constraints : list<Constraint[]>
            /// Total number of individual constraints
            ConstraintCount : int
            /// Cache for bound variables to avoid re-processing
            CachedArrays : System.Collections.Generic.Dictionary<obj, obj>
            /// Solver options
            Options : list<IpOptOption>
        }

    /// Builder function type: transforms builder state
    type Builder = IpOptBuilderState -> IpOptBuilderState

    /// <summary>
    /// Type class for creating constraints from different scalar types.
    /// Provides overloaded CreateConstraint methods for scalar, vector, and matrix types.
    type ConstraintCreator() =
        /// Creates a single constraint from a scalar expression
        static member CreateConstraint(f : scalar, l : float, h : float) =
            Constraint(f, l, h)

        /// Creates element-wise constraints for a 2D vector
        static member CreateConstraint(v : V2s, l : float, h : float) =
            [|
                Constraint(v.X, l, h)
                Constraint(v.Y, l, h)
            |]

        /// Creates element-wise constraints for a 3D vector
        static member CreateConstraint(v : V3s, l : float, h : float) =
            [|
                Constraint(v.X, l, h)
                Constraint(v.Y, l, h)
                Constraint(v.Z, l, h)
            |]

        /// Creates element-wise constraints for a 4D vector
        static member CreateConstraint(v : V4s, l : float, h : float) =
            [|
                Constraint(v.X, l, h)
                Constraint(v.Y, l, h)
                Constraint(v.Z, l, h)
                Constraint(v.W, l, h)
            |]

        /// Creates element-wise constraints for a 2x2 matrix
        static member CreateConstraint(v : M22s, l : float, h : float) =
            [|
                Constraint(v.M00, l, h)
                Constraint(v.M01, l, h)
                Constraint(v.M10, l, h)
                Constraint(v.M11, l, h)
            |]

        /// Creates element-wise constraints for a 3x3 matrix
        static member CreateConstraint(v : M33s, l : float, h : float) =
            [|
                Constraint(v.M00, l, h)
                Constraint(v.M01, l, h)
                Constraint(v.M02, l, h)
                Constraint(v.M10, l, h)
                Constraint(v.M11, l, h)
                Constraint(v.M12, l, h)
                Constraint(v.M20, l, h)
                Constraint(v.M21, l, h)
                Constraint(v.M22, l, h)
            |]

        /// Creates element-wise constraints for a 4x4 matrix
        static member CreateConstraint(v : M44s, l : float, h : float) =
            [|
                Constraint(v.M00, l, h)
                Constraint(v.M01, l, h)
                Constraint(v.M02, l, h)
                Constraint(v.M03, l, h)
                Constraint(v.M10, l, h)
                Constraint(v.M11, l, h)
                Constraint(v.M12, l, h)
                Constraint(v.M13, l, h)
                Constraint(v.M20, l, h)
                Constraint(v.M21, l, h)
                Constraint(v.M22, l, h)
                Constraint(v.M23, l, h)
                Constraint(v.M30, l, h)
                Constraint(v.M31, l, h)
                Constraint(v.M32, l, h)
                Constraint(v.M33, l, h)
            |]
        
    /// <summary>
    /// SRTP helper for constraint creation.
    /// Resolves the appropriate CreateConstraint overload based on the scalar type.
    /// </summary>
    let inline constaintAux< ^a, ^b, ^r when (^a or ^b) : (static member CreateConstraint : ^b * float * float -> ^r)> (d : ^a) (f : ^b) (l : float) (h : float) =
        ((^a or ^b) : (static member CreateConstraint : ^b * float * float -> ^r) (f, l, h))

    /// <summary>
    /// Creates a constraint with the given bounds on a scalar expression.
    /// Works with scalar, vector, and matrix types through SRTP resolution.
    /// </summary>
    let inline constr f l h = constaintAux Unchecked.defaultof<ConstraintCreator> f l h

    /// <summary>
    /// Represents an objective function with its optimization direction.
    /// </summary>
    [<Struct>]
    type Objective = Objective of isMinimization : bool * value : scalar

    /// <summary>
    /// Helper function to pin a managed array and provide a native pointer to it.
    /// Ensures the array is not moved by the GC during the callback execution.
    /// </summary>
    let inline private pin (arr : 'a[]) ([<InlineIfLambda>] action : nativeptr<'a> -> 'r) =
        let gc = GCHandle.Alloc(arr, GCHandleType.Pinned)
        try action (NativePtr.ofNativeInt (gc.AddrOfPinnedObject()))
        finally gc.Free()
    
    /// <summary>
    /// Computation expression builder for IPOPT optimization problems.
    /// Provides a fluent API for defining optimization variables, objectives, and constraints
    /// with automatic differentiation.
    /// </summary>
    type IpOptBuilder() =

        /// Return member for computation expressions (sets the objective function)
        member inline x.Return(objective : scalar) : Builder =
            fun state ->
                { state with Objective = objective }

        /// Delay member for deferred computation
        member inline x.Delay([<InlineIfLambda>] f : unit -> Builder) : Builder =
            fun state -> f() state

        /// Yield member for constraint arrays
        member inline x.Yield (c : Constraint[]) : Builder =
            fun state ->
                let newConstraints = c :: state.Constraints
                { state with Constraints = newConstraints; ConstraintCount = state.ConstraintCount + c.Length }

        /// Yield member for single constraints
        member inline x.Yield (c : Constraint) : Builder =
            fun state ->
                let newConstraints = [| c |] :: state.Constraints
                { state with Constraints = newConstraints; ConstraintCount = state.ConstraintCount + 1 }

        /// Yield member for objective functions (minimize/maximize)
        member inline x.Yield (Objective(isMin, o)) : Builder =
            fun state -> { state with Objective = o; NegateObjective = not isMin }


        /// Yield member for solver options
        member inline x.Yield (o : IpOptOption) : Builder =
            fun state ->
                // Only add options during problem setup, not during evaluation
                if state.Evaluation then state
                else { state with Options = o :: state.Options }

        /// Combine member for sequencing computation expression elements
        member inline x.Combine([<InlineIfLambda>] a : Builder, [<InlineIfLambda>] b : Builder) : Builder =
            fun state -> b (a state)

        /// <summary>
        /// Run member that executes the computation expression and solves the optimization problem.
        /// This creates the IPOPT problem, sets up all callbacks, and runs the solver.
        /// </summary>
        /// <returns>Tuple of (IpOptStatus, objective value)</returns>
        member inline x.Run([<InlineIfLambda>] f : Builder) =
            // Build initial state to collect variables, constraints, and objective
            let initialState = { Evaluation = false; Options = []; NegateObjective = false; CachedArrays = System.Collections.Generic.Dictionary(); VariableOffset = 0; UpdateScalars = []; UpdateInput = []; WriteTo = []; Objective = scalar.Zero; Constraints = []; ConstraintCount = 0 }
            let state = f initialState

            let options = state.Options
            let mutable oCache = scalar.Zero
            let mutable cCache = Array.zeroCreate<Constraint> state.ConstraintCount

            /// Updates the cached scalar values and re-evaluates objective/constraints
            let updateCache (x : nativeint) =
                // Update all scalar variables from the current optimization state
                for update in state.UpdateScalars do
                    update (NativePtr.ofNativeInt x)

                // Re-run the builder in evaluation mode to compute objective and constraints
                let state = f { initialState with Evaluation = true }
                let res = state.Objective
                let mutable oi = 0
                for cs in state.Constraints do
                    for c in cs do
                        cCache.[oi] <- c
                        oi <- oi + 1

                // Handle maximization by negating the objective
                if state.NegateObjective then
                    oCache <- -res
                else
                    oCache <- res

            /// Callback to evaluate the objective function value
            let objectiveFunc =
                EvalF (fun n x newX objValue ->
                    if newX then updateCache x
                    NativePtr.write (NativePtr.ofNativeInt objValue) oCache.Value
                    true
                )

            /// Callback to evaluate the gradient of the objective function
            let objectiveGradientFunc =
                EvalGradF (fun n x newX grad ->
                    if newX then updateCache x
                    let gradPtr = NativePtr.ofNativeInt grad
                    // Write gradient values from automatic differentiation
                    for KeyValue(i, v) in oCache.Jacobian do
                        NativePtr.set gradPtr i v
                    true
                )
                
            let cLowerBounds = Array.zeroCreate<float> state.ConstraintCount
            let cUpperBounds = Array.zeroCreate<float> state.ConstraintCount

            /// Callback to evaluate constraint function values
            let constraintsFunc =
                EvalG (fun n x newX m g ->
                    if newX then updateCache x
                    let gPtr = NativePtr.ofNativeInt g

                    // Write all constraint values
                    let mutable ci = 0
                    for Constraint(f, l, h) in cCache do
                        NativePtr.set gPtr ci f.Value
                        ci <- ci + 1

                    true
                )

            /// Callback to evaluate the Jacobian of the constraints
            /// IPOPT uses a two-phase protocol: first call returns structure (sparsity pattern),
            /// subsequent calls return values
            let constraintJacobianFunc =
                EvalJacG (fun n x newX m neleJac iRow jCol values ->
                    if newX then updateCache x
                    if values = 0n then
                        // Phase 1: return sparsity structure (row/column indices of non-zeros)
                        let pRows = NativePtr.ofNativeInt iRow
                        let pCols = NativePtr.ofNativeInt jCol
                        let mutable ji = 0
                        for ci in 0 .. cCache.Length - 1 do
                            let (Constraint(f, _, _)) = cCache.[ci]
                            for KeyValue(vi, _) in f.Jacobian do
                                NativePtr.set pRows ji ci  // Constraint index
                                NativePtr.set pCols ji vi  // Variable index
                                ji <- ji + 1
                    else
                        // Phase 2: return Jacobian values at current point
                        let pValues = NativePtr.ofNativeInt values
                        let mutable ji = 0
                        for ci in 0 .. cCache.Length - 1 do
                            let (Constraint(f, _, _)) = cCache.[ci]
                            // Write derivatives from automatic differentiation
                            for KeyValue(_, v) in f.Jacobian do
                                NativePtr.set pValues ji v
                                ji <- ji + 1

                    true
                )
                
            let values = Array.zeroCreate<float> state.VariableOffset
            
            pin values <| fun ptr ->
                
                let m = state.ConstraintCount
                let n = state.VariableOffset
                
                
                let lowerBounds = Array.create n System.Double.NegativeInfinity
                let upperBounds = Array.create n System.Double.PositiveInfinity
                // pinner {
                //     let! ptr = values
                //     let! pLower = lowerBounds
                //     let! pUpper = upperBounds
                //     let! pCLower = cLowerBounds
                //     let! pCUpper = cUpperBounds
                //     
                //     
                //     do
                // }
                
                let handle = 
                    pin lowerBounds <| fun pLower ->
                        pin upperBounds <| fun pUpper ->
                            pin cLowerBounds <| fun pCLower ->
                                pin cUpperBounds <| fun pCUpper ->
                                    let numberOfJacobianEntries =
                                        updateCache (NativePtr.toNativeInt ptr)
                                        let mutable ci = 0
                                        let mutable cnt = 0
                                        for Constraint(f, l, h) in cCache do
                                            cnt <- cnt + f.Jacobian.Count
                                            cLowerBounds.[ci] <- l
                                            cUpperBounds.[ci] <- h
                                            ci <- ci + 1
                                        cnt
                                    
                                    let handle = 
                                        IpoptNative.CreateIpoptProblem(
                                            state.VariableOffset,
                                            NativePtr.toNativeInt pLower,
                                            NativePtr.toNativeInt pUpper,
                                            state.ConstraintCount,
                                            NativePtr.toNativeInt pCLower,
                                            NativePtr.toNativeInt pCUpper,
                                            numberOfJacobianEntries,
                                            0, 0,
                                            objectiveFunc,
                                            constraintsFunc,
                                            objectiveGradientFunc,
                                            constraintJacobianFunc,
                                            EvalH(fun  _ _ _ _ _ _ _ _ _ _ _ -> false)
                                        )
                                    
                                    let tol = options |> List.tryPick (function IpOptOption.Tolerance t -> Some t | _ -> None) |> Option.defaultValue 1e-7
                                    let printLevel = options |> List.tryPick (function IpOptOption.PrintLevel l -> Some l | _ -> None) |> Option.defaultValue 0
                                    let maxIter = options |> List.tryPick (function IpOptOption.MaxIterations i -> Some i | _ -> None) |> Option.defaultValue 1000
                                    let hessianApproximation = options |> List.tryPick (function IpOptOption.HessianApproximation s -> Some s | _ -> None) |> Option.defaultValue "limited-memory"
                                    
                                    IpoptNative.AddIpoptIntOption(handle, "print_level", printLevel) |> ignore
                                    IpoptNative.AddIpoptNumOption(handle, "tol", tol) |> ignore
                                    IpoptNative.AddIpoptIntOption(handle, "max_iter", maxIter) |> ignore
                                    IpoptNative.AddIpoptStrOption(handle, "hessian_approximation", hessianApproximation) |> ignore
                                    
                                    for o in options do
                                        match o with
                                        // Already handled above
                                        | IpOptOption.Tolerance _
                                        | IpOptOption.PrintLevel _
                                        | IpOptOption.MaxIterations _
                                        | IpOptOption.HessianApproximation _ ->
                                            ()

                                        // Termination criteria
                                        | IpOptOption.AcceptableTolerance v ->
                                            IpoptNative.AddIpoptNumOption(handle, "acceptable_tol", v) |> ignore
                                        | IpOptOption.AcceptableConstraintViolationTolerance v ->
                                            IpoptNative.AddIpoptNumOption(handle, "acceptable_constr_viol_tol", v) |> ignore
                                        | IpOptOption.AcceptableIterations v ->
                                            IpoptNative.AddIpoptIntOption(handle, "acceptable_iter", v) |> ignore
                                        | IpOptOption.CpuTimeLimit v ->
                                            IpoptNative.AddIpoptNumOption(handle, "cpu_time_limit", v) |> ignore
                                        | IpOptOption.MaxWallTime v ->
                                            IpoptNative.AddIpoptNumOption(handle, "max_wall_time", v) |> ignore

                                        // Convergence tolerances
                                        | IpOptOption.DualInfeasibilityTolerance v ->
                                            IpoptNative.AddIpoptNumOption(handle, "dual_inf_tol", v) |> ignore
                                        | IpOptOption.ConstraintViolationTolerance v ->
                                            IpoptNative.AddIpoptNumOption(handle, "constr_viol_tol", v) |> ignore
                                        | IpOptOption.ComplementarityTolerance v ->
                                            IpoptNative.AddIpoptNumOption(handle, "compl_inf_tol", v) |> ignore

                                        // Output and printing
                                        | IpOptOption.OutputFile file ->
                                            IpoptNative.AddIpoptStrOption(handle, "output_file", file) |> ignore
                                        | IpOptOption.PrintInfoString v ->
                                            IpoptNative.AddIpoptStrOption(handle, "print_info_string", v) |> ignore
                                        | IpOptOption.PrintUserOptions v ->
                                            IpoptNative.AddIpoptStrOption(handle, "print_user_options", v) |> ignore

                                        // Linear solver
                                        | IpOptOption.LinearSolver solver ->
                                            IpoptNative.AddIpoptStrOption(handle, "linear_solver", solver) |> ignore
                                        | IpOptOption.LinearSystemScaling v ->
                                            IpoptNative.AddIpoptStrOption(handle, "linear_system_scaling", v) |> ignore

                                        // Barrier parameter
                                        | IpOptOption.BarrierParameterStrategy strategy ->
                                            IpoptNative.AddIpoptStrOption(handle, "mu_strategy", strategy) |> ignore
                                        | IpOptOption.BarrierParameterInitial v ->
                                            IpoptNative.AddIpoptNumOption(handle, "mu_init", v) |> ignore
                                        | IpOptOption.BarrierParameterMaximum v ->
                                            IpoptNative.AddIpoptNumOption(handle, "mu_max", v) |> ignore
                                        | IpOptOption.BarrierParameterMinimum v ->
                                            IpoptNative.AddIpoptNumOption(handle, "mu_min", v) |> ignore
                                        | IpOptOption.BarrierParameterLinearDecreaseFactor v ->
                                            IpoptNative.AddIpoptNumOption(handle, "mu_linear_decrease_factor", v) |> ignore
                                        | IpOptOption.BarrierParameterSuperlinearDecreasePower v ->
                                            IpoptNative.AddIpoptNumOption(handle, "mu_superlinear_decrease_power", v) |> ignore

                                        // Hessian approximation
                                        | IpOptOption.LimitedMemoryMaxHistory v ->
                                            IpoptNative.AddIpoptIntOption(handle, "limited_memory_max_history", v) |> ignore

                                        // NLP scaling
                                        | IpOptOption.NonlinearProgramScalingMethod v ->
                                            IpoptNative.AddIpoptStrOption(handle, "nlp_scaling_method", v) |> ignore
                                        | IpOptOption.NonlinearProgramScalingMaxGradient v ->
                                            IpoptNative.AddIpoptNumOption(handle, "nlp_scaling_max_gradient", v) |> ignore
                                        | IpOptOption.ObjectiveScalingFactor v ->
                                            IpoptNative.AddIpoptNumOption(handle, "obj_scaling_factor", v) |> ignore

                                        // Derivative checking
                                        | IpOptOption.DerivativeTest test ->
                                            IpoptNative.AddIpoptStrOption(handle, "derivative_test", test) |> ignore
                                        | IpOptOption.DerivativeTestTolerance v ->
                                            IpoptNative.AddIpoptNumOption(handle, "derivative_test_tol", v) |> ignore
                                        | IpOptOption.DerivativeTestPrintAll v ->
                                            IpoptNative.AddIpoptStrOption(handle, "derivative_test_print_all", v) |> ignore

                                        // Warm start
                                        | IpOptOption.WarmStartInitializationPoint v ->
                                            IpoptNative.AddIpoptStrOption(handle, "warm_start_init_point", v) |> ignore
                                        | IpOptOption.WarmStartBoundPush v ->
                                            IpoptNative.AddIpoptNumOption(handle, "warm_start_bound_push", v) |> ignore
                                        | IpOptOption.WarmStartSlackBoundPush v ->
                                            IpoptNative.AddIpoptNumOption(handle, "warm_start_slack_bound_push", v) |> ignore
                                        | IpOptOption.WarmStartMultiplierBoundPush v ->
                                            IpoptNative.AddIpoptNumOption(handle, "warm_start_mult_bound_push", v) |> ignore

                                        // Step calculation
                                        | IpOptOption.BarrierToleranceFactor v ->
                                            IpoptNative.AddIpoptNumOption(handle, "barrier_tol_factor", v) |> ignore
                                        | IpOptOption.MaxSecondOrderCorrection v ->
                                            IpoptNative.AddIpoptIntOption(handle, "max_soc", v) |> ignore
                                        | IpOptOption.BoundMultiplierMethod v ->
                                            IpoptNative.AddIpoptStrOption(handle, "bound_mult_init_method", v) |> ignore

                                        // Restoration phase
                                        | IpOptOption.StartWithRestoration v ->
                                            IpoptNative.AddIpoptStrOption(handle, "start_with_resto", v) |> ignore
                                        | IpOptOption.RequiredInfeasibilityReduction v ->
                                            IpoptNative.AddIpoptNumOption(handle, "required_infeas_reduction", v) |> ignore

                                        // Quasi-Newton
                                        | IpOptOption.QuasiNewtonUpdateType v ->
                                            IpoptNative.AddIpoptStrOption(handle, "hessian_approximation_space", v) |> ignore

                                        // Custom options
                                        | IpOptOption.String(k, v) ->
                                            IpoptNative.AddIpoptStrOption(handle, k, v) |> ignore
                                        | IpOptOption.Float(k, v) ->
                                            IpoptNative.AddIpoptNumOption(handle, k, v) |> ignore
                                        | IpOptOption.Int(k, v) ->
                                            IpoptNative.AddIpoptIntOption(handle, k, v) |> ignore
                                            
                                    for w in state.WriteTo do w ptr
                                    handle
                                
                if handle <> 0n then
                    let m = state.ConstraintCount
                    let n = state.VariableOffset
                    
                    
                    
                    
                    let g = Array.zeroCreate<float> m
                    let objVal = Array.zeroCreate<float> 1
                    let multG = Array.zeroCreate<float> m
                    let multXL = Array.zeroCreate<float> n
                    let multXU = Array.zeroCreate<float> n
                    
                    let gHandle = GCHandle.Alloc(g, GCHandleType.Pinned)
                    let objValHandle = GCHandle.Alloc(objVal, GCHandleType.Pinned)
                    let multGHandle = GCHandle.Alloc(multG, GCHandleType.Pinned)
                    let multXLHandle = GCHandle.Alloc(multXL, GCHandleType.Pinned)
                    let multXUHandle = GCHandle.Alloc(multXU, GCHandleType.Pinned)
                    
                    try
                        let status = IpoptNative.IpoptSolve(
                            handle,
                            NativePtr.toNativeInt ptr,
                            gHandle.AddrOfPinnedObject(),
                            objValHandle.AddrOfPinnedObject(),
                            multGHandle.AddrOfPinnedObject(),
                            multXLHandle.AddrOfPinnedObject(),
                            multXUHandle.AddrOfPinnedObject(),
                            nativeint 0)
                        
                        let returnStatus = enum<IpOptStatus>(status)
                        for r in state.UpdateInput do r ptr
                        
                        
                        let obj = if state.NegateObjective then -objVal.[0] else objVal.[0]
                        (returnStatus, obj)
                    finally
                        gHandle.Free()
                        objValHandle.Free()
                        multGHandle.Free()
                        multXLHandle.Free()
                        multXUHandle.Free()
                else
                    (IpOptStatus.InternalError, System.Double.PositiveInfinity)


        /// Bind member for ref-wrapped optimization variables.
        /// Registers a mutable reference as an optimization variable and provides its scalar representation.
        member inline x.Bind (thing : ref<_>, action) =
            fun (state : IpOptBuilderState) ->
                let mutable arr = Unchecked.defaultof<_>
                let mutable runState = state
                // Check if this variable was already registered (for reuse in computation expression)
                match state.CachedArrays.TryGetValue thing with
                | (true, cached) ->
                    arr <- cached :?> _
                | _ ->
                    let ad = scalarAdapter thing.Value
                    
                    let sValue =
                        ref (ad.Variable(thing.Value, state.VariableOffset))
                        
                    let updateScalars(src : nativeptr<float>) =
                        sValue.Value <- ad.ReadScalar(src, state.VariableOffset)
                        
                    let updateInput(src : nativeptr<float>) =
                        thing.Value <- ad.ReadValue(src, state.VariableOffset)
                        
                    let writeTo (dst : nativeptr<float>) =
                        ad.WriteTo(dst, state.VariableOffset, thing.Value)
                        
                    state.CachedArrays.[thing] <- sValue
                    arr <- sValue
                    runState <- {
                        state with
                            UpdateScalars = updateScalars :: state.UpdateScalars
                            UpdateInput = updateInput :: state.UpdateInput
                            WriteTo = writeTo :: state.WriteTo
                            VariableOffset = state.VariableOffset + ad.DoubleCount
                    }
                action arr.Value runState

        /// Bind member for array optimization variables.
        /// Registers an array of values as optimization variables and provides their scalar representations.
        member inline x.Bind (thing : array<_>, action) =
            fun (state : IpOptBuilderState) ->
                let mutable arr = Unchecked.defaultof<_>
                let mutable runState = state
                // Check if this array was already registered
                match state.CachedArrays.TryGetValue thing with
                | (true, cached) ->
                    arr <- cached :?> _
                | _ ->
                    let v = Array.tryHead thing |> Option.defaultValue Unchecked.defaultof<_>
                    let ad = scalarAdapter v
                    
                    let sValue =
                        let variables = Array.zeroCreate thing.Length
                        let mutable offset = state.VariableOffset
                        for i in 0 .. thing.Length - 1 do
                            variables.[i] <- ad.Variable(thing.[i], offset)
                            offset <- offset + ad.DoubleCount
                        variables
                        
                    let updateScalars(src : nativeptr<float>) =
                        let mutable offset = state.VariableOffset
                        for i in 0 .. thing.Length - 1 do
                            sValue.[i] <- ad.ReadScalar(src, offset)
                            offset <- offset + ad.DoubleCount
                        
                    let updateInput(src : nativeptr<float>) =
                        let mutable offset = state.VariableOffset
                        for i in 0 .. thing.Length - 1 do
                            thing.[i] <- ad.ReadValue(src, offset)
                            offset <- offset + ad.DoubleCount
                        
                    let writeTo (dst : nativeptr<float>) =
                        let mutable offset = state.VariableOffset
                        for i in 0 .. thing.Length - 1 do
                            ad.WriteTo(dst, state.VariableOffset, thing.[i])
                            offset <- offset + ad.DoubleCount
                        
                    state.CachedArrays.[thing] <- sValue
                    arr <- sValue
                    runState <- {
                        state with
                            UpdateScalars = updateScalars :: state.UpdateScalars
                            UpdateInput = updateInput :: state.UpdateInput
                            WriteTo = writeTo :: state.WriteTo
                            VariableOffset = state.VariableOffset + ad.DoubleCount * thing.Length
                    }
                action arr runState
       
       
open IpOptBuilderImplementation

/// Auto-opened module providing the main API for IPOPT optimization.
/// Contains helper functions for creating constraints and the computation expression builder.
[<AutoOpen>]
module IpOptBuilder =
    open IpOptBuilderImplementation

    /// Creates an objective to minimize the given scalar expression
    let inline minimize a =
        Objective(true, a)

    /// Creates an objective to maximize the given scalar expression
    let inline maximize a =
        Objective(false, a)

    /// Creates an equality constraint: a == b
    let inline equal a b =
        constr (a - b) 0.0 0.0

    /// Creates an inequality constraint: a >= b
    let inline greaterEqual a b =
        constr (a - b) 0.0 System.Double.PositiveInfinity

    /// Creates an inequality constraint: a <= b
    let inline lessEqual a b =
        constr (b - a) 0.0 System.Double.PositiveInfinity

    /// Creates a range constraint: l <= a <= h (normalized to [0,1])
    let inline range a l h =
        constr ((a - l) / (h - l)) 0.0 1.0

    /// The main computation expression builder instance for IPOPT problems
    let ipopt = IpOptBuilder()
