module IpOptSharp.Tests.SolverIntegrationTests

open NUnit.Framework
open IpOptSharp
open Aardvark.Base

// Initialize Aardvark once for the entire test suite to enable native library loading
[<SetUpFixture>]
type SolverSetup() =
    [<OneTimeSetUp>]
    member _.Setup() =
        Aardvark.Init()

// Helper for approximate equality with tolerance
let approxEqual tolerance a b = abs (a - b) < tolerance
let defaultTolerance = 1e-5

// ============================================================================
// Unconstrained Optimization Tests
// ============================================================================

[<Test>]
let ``Unconstrained quadratic minimization - two variables`` () =
    // Minimize f(x,y) = (x - 2)^2 + (y + 1)^2
    // Known solution: x = 2, y = -1, f(x,y) = 0
    let point = ref (V2d(0.0, 0.0))

    let (status, objective) =
        ipopt {
            let! p = point
            IpOpt.PrintLevel 0
            IpOpt.Minimize ((p.X - 2.0) * (p.X - 2.0) + (p.Y + 1.0) * (p.Y + 1.0))
        }

    Assert.AreEqual(IpOptStatus.Success, status)
    Assert.IsTrue(approxEqual defaultTolerance objective 0.0, sprintf "Objective should be ~0, got %.6f" objective)
    Assert.IsTrue(approxEqual defaultTolerance point.Value.X 2.0, sprintf "x should be ~2, got %.6f" point.Value.X)
    Assert.IsTrue(approxEqual defaultTolerance point.Value.Y -1.0, sprintf "y should be ~-1, got %.6f" point.Value.Y)

[<Test>]
let ``Rosenbrock function minimization`` () =
    // Minimize f(x,y) = (1-x)^2 + 100(y-x^2)^2
    // Known solution: x = 1, y = 1, f(x,y) = 0
    let point = ref (V2d(0.0, 0.0))

    let (status, objective) =
        ipopt {
            let! p = point
            IpOpt.PrintLevel 0
            IpOpt.MaxIterations 1000
            IpOpt.Tolerance 1e-8
            let term1 = (1.0 - p.X) * (1.0 - p.X)
            let term2 = 100.0 * (p.Y - p.X * p.X) * (p.Y - p.X * p.X)
            IpOpt.Minimize (term1 + term2)
        }

    Assert.AreEqual(IpOptStatus.Success, status)
    Assert.IsTrue(approxEqual 1e-4 objective 0.0, sprintf "Objective should be ~0, got %.6f" objective)
    Assert.IsTrue(approxEqual 1e-3 point.Value.X 1.0, sprintf "x should be ~1, got %.6f" point.Value.X)
    Assert.IsTrue(approxEqual 1e-3 point.Value.Y 1.0, sprintf "y should be ~1, got %.6f" point.Value.Y)

// ============================================================================
// Equality Constrained Optimization Tests
// ============================================================================

[<Test>]
let ``Minimize distance to point with circle constraint`` () =
    // Minimize distance to (2, 0) subject to x^2 + y^2 = 1
    // Known solution: x = 1, y = 0, distance = 1
    let point = ref (V2d(0.5, 0.5))

    let (status, objective) =
        ipopt {
            let! p = point
            IpOpt.PrintLevel 0
            IpOpt.Equal(p.Length, 1.0)  // Constrain to unit circle
            IpOpt.Minimize (Vec.lengthSquared (p - V2d(2.0, 0.0)))
        }

    Assert.AreEqual(IpOptStatus.Success, status)
    Assert.IsTrue(approxEqual 1e-4 objective 1.0, sprintf "Objective should be ~1, got %.6f" objective)
    Assert.IsTrue(approxEqual defaultTolerance point.Value.X 1.0, sprintf "x should be ~1, got %.6f" point.Value.X)
    Assert.IsTrue(approxEqual defaultTolerance point.Value.Y 0.0, sprintf "y should be ~0, got %.6f" point.Value.Y)

[<Test>]
let ``Equilateral triangle on unit circle - maximize perimeter`` () =
    // Maximize perimeter of triangle with vertices on unit circle
    // Known solution: equilateral triangle with perimeter = 3*sqrt(3) ≈ 5.196
    let a = ref (V2d(-1.0, -1.0))
    let b = ref (V2d(1.0, -1.0))
    let c = ref (V2d(0.0, 1.0))

    let (status, objective) =
        ipopt {
            let! a = a
            let! b = b
            let! c = c

            IpOpt.PrintLevel 0
            IpOpt.Tolerance 1e-6

            let circumference = Vec.length (b - a) + Vec.length (c - b) + Vec.length (a - c)
            IpOpt.Maximize circumference

            IpOpt.Equal(a.Length, 1.0)
            IpOpt.Equal(b.Length, 1.0)
            IpOpt.Equal(c.Length, 1.0)
        }

    let expectedPerimeter = 3.0 * sqrt 3.0  // ≈ 5.196
    Assert.AreEqual(IpOptStatus.Success, status)
    Assert.IsTrue(approxEqual 1e-3 objective expectedPerimeter,
                  sprintf "Perimeter should be ~%.4f, got %.6f" expectedPerimeter objective)

    // Verify all vertices are on unit circle
    Assert.IsTrue(approxEqual defaultTolerance a.Value.Length 1.0, "a should be on unit circle")
    Assert.IsTrue(approxEqual defaultTolerance b.Value.Length 1.0, "b should be on unit circle")
    Assert.IsTrue(approxEqual defaultTolerance c.Value.Length 1.0, "c should be on unit circle")

    // Verify angles are all 60 degrees (equilateral)
    let angle1 = Vec.AngleBetween(Vec.normalize (b.Value - a.Value), -Vec.normalize (c.Value - b.Value))
    let angle2 = Vec.AngleBetween(Vec.normalize (c.Value - b.Value), -Vec.normalize (a.Value - c.Value))
    let angle3 = Vec.AngleBetween(Vec.normalize (a.Value - c.Value), -Vec.normalize (b.Value - a.Value))

    let expected60Deg = Constant.Pi / 3.0
    Assert.IsTrue(approxEqual 1e-2 angle1 expected60Deg, sprintf "Angle 1 should be ~60°, got %.2f°" (angle1 * Constant.DegreesPerRadian))
    Assert.IsTrue(approxEqual 1e-2 angle2 expected60Deg, sprintf "Angle 2 should be ~60°, got %.2f°" (angle2 * Constant.DegreesPerRadian))
    Assert.IsTrue(approxEqual 1e-2 angle3 expected60Deg, sprintf "Angle 3 should be ~60°, got %.2f°" (angle3 * Constant.DegreesPerRadian))

[<Test>]
let ``Linear equality constraint`` () =
    // Minimize f(x,y) = x^2 + y^2 subject to x + y = 1
    // Known solution: x = 0.5, y = 0.5, f(x,y) = 0.5
    let point = ref (V2d(0.0, 0.0))

    let (status, objective) =
        ipopt {
            let! p = point
            IpOpt.PrintLevel 0
            IpOpt.Equal(p.X + p.Y, 1.0)
            IpOpt.Minimize (p.X * p.X + p.Y * p.Y)
        }

    Assert.AreEqual(IpOptStatus.Success, status)
    Assert.IsTrue(approxEqual defaultTolerance objective 0.5, sprintf "Objective should be ~0.5, got %.6f" objective)
    Assert.IsTrue(approxEqual defaultTolerance point.Value.X 0.5, sprintf "x should be ~0.5, got %.6f" point.Value.X)
    Assert.IsTrue(approxEqual defaultTolerance point.Value.Y 0.5, sprintf "y should be ~0.5, got %.6f" point.Value.Y)

// ============================================================================
// Inequality Constrained Optimization Tests
// ============================================================================

[<Test>]
let ``Box constraint optimization`` () =
    // Minimize f(x,y) = (x-2)^2 + (y-2)^2 subject to x,y >= 0 and x,y <= 1
    // Known solution: x = 1, y = 1, f(x,y) = 2
    let point = ref (V2d(0.5, 0.5))

    let (status, objective) =
        ipopt {
            let! p = point
            IpOpt.PrintLevel 0

            IpOpt.GreaterEqual(p.X, 0.0)
            IpOpt.GreaterEqual(p.Y, 0.0)
            IpOpt.LessEqual(p.X, 1.0)
            IpOpt.LessEqual(p.Y, 1.0)

            IpOpt.Minimize ((p.X - 2.0) * (p.X - 2.0) + (p.Y - 2.0) * (p.Y - 2.0))
        }

    Assert.AreEqual(IpOptStatus.Success, status)
    Assert.IsTrue(approxEqual defaultTolerance objective 2.0, sprintf "Objective should be ~2, got %.6f" objective)
    Assert.IsTrue(approxEqual defaultTolerance point.Value.X 1.0, sprintf "x should be ~1, got %.6f" point.Value.X)
    Assert.IsTrue(approxEqual defaultTolerance point.Value.Y 1.0, sprintf "y should be ~1, got %.6f" point.Value.Y)

[<Test>]
let ``Linear inequality constraint`` () =
    // Minimize f(x,y) = -x - y subject to x + y <= 1, x >= 0, y >= 0
    // Known solution: x can be anything, y = 1-x, but minimum is at x=0, y=1 or x=1, y=0
    // Objective = -1
    let point = ref (V2d(0.5, 0.5))

    let (status, objective) =
        ipopt {
            let! p = point
            IpOpt.PrintLevel 0

            IpOpt.GreaterEqual(p.X, 0.0)
            IpOpt.GreaterEqual(p.Y, 0.0)
            IpOpt.LessEqual(p.X + p.Y, 1.0)

            IpOpt.Minimize (-p.X - p.Y)
        }

    Assert.AreEqual(IpOptStatus.Success, status)
    Assert.IsTrue(approxEqual defaultTolerance objective -1.0, sprintf "Objective should be ~-1, got %.6f" objective)
    // Solution should satisfy x + y = 1 (active constraint)
    Assert.IsTrue(approxEqual defaultTolerance (point.Value.X + point.Value.Y) 1.0,
                  sprintf "x + y should be ~1, got %.6f" (point.Value.X + point.Value.Y))

[<Test>]
let ``Minimize with circle constraint`` () =
    // Minimize (x-3)^2 + y^2 subject to x^2 + y^2 <= 1 (inside unit circle)
    // Unconstrained minimum is at (3,0), but constrained to unit circle gives (1,0)
    // Known solution: x = 1, y = 0, objective = 4
    let point = ref (V2d(0.5, 0.5))

    let (status, objective) =
        ipopt {
            let! p = point
            IpOpt.PrintLevel 0

            IpOpt.Minimize ((p.X - 3.0) * (p.X - 3.0) + p.Y * p.Y)

            // Must be inside or on unit circle
            IpOpt.LessEqual(Vec.lengthSquared p, 1.0)
        }

    Assert.AreEqual(IpOptStatus.Success, status)
    Assert.IsTrue(approxEqual 1e-3 objective 4.0, sprintf "Objective should be ~4, got %.6f" objective)
    Assert.IsTrue(approxEqual 1e-2 point.Value.X 1.0, sprintf "x should be ~1, got %.6f" point.Value.X)
    Assert.IsTrue(approxEqual 1e-2 (abs point.Value.Y) 0.0, sprintf "y should be ~0, got %.6f" point.Value.Y)

// ============================================================================
// 3D Geometry Tests
// ============================================================================

[<Test>]
let ``Find rotation matrix - orthonormal constraints`` () =
    // Find rotation that maps coordinate axes to specific targets
    // This is a real application of the solver
    let ta = V3d.IOO  // target for basis vector a
    let tb = V3d.OIO  // target for basis vector b
    let tc = V3d.OOI  // target for basis vector c

    let ra = V3d.OIO  // source basis vector a
    let rb = V3d.IOO  // source basis vector b
    let rc = V3d.OON  // source basis vector c

    let rot = ref (M33d.Rotation(V3d.III.Normalized, 0.2))

    let (status, objective) =
        ipopt {
            let! rot = rot

            IpOpt.PrintLevel 0
            IpOpt.MaxIterations 100
            IpOpt.ConstraintViolationTolerance 1e-10

            IpOpt.Orthonormal rot
            IpOpt.Minimize (
                Vec.lengthSquared (rot * V3s ra - ta) +
                Vec.lengthSquared (rot * V3s rb - tb) +
                Vec.lengthSquared (rot * V3s rc - tc)
            )
        }

    Assert.AreEqual(IpOptStatus.Success, status)
    Assert.IsTrue(approxEqual 1e-6 objective 0.0, sprintf "Objective should be ~0, got %.10f" objective)

    // Verify the matrix is orthonormal
    Assert.IsTrue(rot.Value.IsOrthonormal(1e-7), "Result should be orthonormal")

    // Verify the rotation maps sources to targets correctly
    let a_result = rot.Value.Transform ra
    let b_result = rot.Value.Transform rb
    let c_result = rot.Value.Transform rc

    Assert.IsTrue(approxEqual 1e-5 (Vec.distance a_result ta) 0.0, "Should map ra to ta")
    Assert.IsTrue(approxEqual 1e-5 (Vec.distance b_result tb) 0.0, "Should map rb to tb")
    Assert.IsTrue(approxEqual 1e-5 (Vec.distance c_result tc) 0.0, "Should map rc to tc")

[<Test>]
let ``Minimize sphere radius enclosing points`` () =
    // Find smallest sphere enclosing a set of points
    let p1 = V3d(1.0, 0.0, 0.0)
    let p2 = V3d(-1.0, 0.0, 0.0)
    let p3 = V3d(0.0, 1.0, 0.0)
    let p4 = V3d(0.0, -1.0, 0.0)

    let sphere = ref (Sphere3d(V3d.Zero, 2.0))

    let (status, objective) =
        ipopt {
            let! s = sphere

            IpOpt.PrintLevel 0

            // Minimize the radius
            IpOpt.Minimize s.Radius

            // All points must be inside or on the sphere
            let dx1 = p1.X - s.Center.X
            let dy1 = p1.Y - s.Center.Y
            let dz1 = p1.Z - s.Center.Z
            IpOpt.LessEqual(dx1 * dx1 + dy1 * dy1 + dz1 * dz1, s.Radius * s.Radius)

            let dx2 = p2.X - s.Center.X
            let dy2 = p2.Y - s.Center.Y
            let dz2 = p2.Z - s.Center.Z
            IpOpt.LessEqual(dx2 * dx2 + dy2 * dy2 + dz2 * dz2, s.Radius * s.Radius)

            let dx3 = p3.X - s.Center.X
            let dy3 = p3.Y - s.Center.Y
            let dz3 = p3.Z - s.Center.Z
            IpOpt.LessEqual(dx3 * dx3 + dy3 * dy3 + dz3 * dz3, s.Radius * s.Radius)

            let dx4 = p4.X - s.Center.X
            let dy4 = p4.Y - s.Center.Y
            let dz4 = p4.Z - s.Center.Z
            IpOpt.LessEqual(dx4 * dx4 + dy4 * dy4 + dz4 * dz4, s.Radius * s.Radius)
        }

    Assert.AreEqual(IpOptStatus.Success, status)
    // Smallest enclosing sphere for these points has radius 1 and center at origin
    Assert.IsTrue(approxEqual 1e-2 sphere.Value.Radius 1.0,
                  sprintf "Radius should be ~1, got %.6f" sphere.Value.Radius)
    Assert.IsTrue(approxEqual 1e-2 (Vec.length sphere.Value.Center) 0.0,
                  sprintf "Center should be at origin, got distance %.6f" (Vec.length sphere.Value.Center))
