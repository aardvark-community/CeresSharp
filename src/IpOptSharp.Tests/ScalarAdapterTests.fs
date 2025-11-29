module IpOptSharp.Tests.ScalarAdapterTests

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Aardvark.Base
open IpOptSharp.IpOptBuilderImplementation
open Microsoft.FSharp.NativeInterop
open System.Runtime.InteropServices

#nowarn "9"

// Helper function to pin an array and provide a native pointer
let inline private withPtr (arr: 'a[]) ([<InlineIfLambda>] action: nativeptr<'a> -> 'r) =
    let gc = GCHandle.Alloc(arr, GCHandleType.Pinned)
    try action (NativePtr.ofNativeInt (gc.AddrOfPinnedObject()))
    finally gc.Free()

// Helper to get the scalar adapter for a type
let inline private getAdapter (dummy: 'a) = scalarAdapter dummy

// Generic test for round-trip conversion: WriteTo -> ReadValue
let inline testRoundTrip (value: 'a) (adapter: ScalarAdapter<'a, 'b>) =
    let buffer = Array.zeroCreate<float> adapter.DoubleCount
    withPtr buffer (fun ptr ->
        adapter.WriteTo(ptr, 0, value)
        let readBack = adapter.ReadValue(ptr, 0)
        readBack
    )

// Generic test for Variable -> GetValue
let inline testVariableGetValue (value: 'a) (adapter: ScalarAdapter<'a, 'b>) =
    let scalarVar = adapter.Variable(value, 0)
    let extractedValue = adapter.GetValue(scalarVar)
    extractedValue

// Generic test for ReadScalar -> GetValue
let inline testReadScalar (value: 'a) (adapter: ScalarAdapter<'a, 'b>) =
    let buffer = Array.zeroCreate<float> adapter.DoubleCount
    withPtr buffer (fun ptr ->
        adapter.WriteTo(ptr, 0, value)
        let scalarVar = adapter.ReadScalar(ptr, 0)
        let extractedValue = adapter.GetValue(scalarVar)
        extractedValue
    )

// Tolerance for floating-point comparisons
let epsilon = 1e-9
let inline approxEqual a b = abs (a - b) < epsilon

// Helper to extract NormalFloat value
let inline nf (NormalFloat f) = f

// ============================================================================
// Tests for float (scalar)
// ============================================================================

[<Property>]
let ``float: WriteTo -> ReadValue round-trip`` (value: NormalFloat) =
    let v = nf value
    let adapter = getAdapter v
    let result = testRoundTrip v adapter
    approxEqual result v

[<Property>]
let ``float: Variable -> GetValue`` (value: NormalFloat) =
    let v = nf value
    let adapter = getAdapter v
    let result = testVariableGetValue v adapter
    approxEqual result v

[<Property>]
let ``float: ReadScalar -> GetValue`` (value: NormalFloat) =
    let v = nf value
    let adapter = getAdapter v
    let result = testReadScalar v adapter
    approxEqual result v

[<Test>]
let ``float: DoubleCount is 1`` () =
    let adapter = getAdapter 0.0
    Assert.AreEqual(1, adapter.DoubleCount)

// ============================================================================
// Tests for V2d
// ============================================================================

[<Property>]
let ``V2d: WriteTo -> ReadValue round-trip`` (x: NormalFloat, y: NormalFloat) =
    let value = V2d(nf x, nf y)
    let adapter = getAdapter value
    let result = testRoundTrip value adapter
    approxEqual result.X value.X && approxEqual result.Y value.Y

[<Property>]
let ``V2d: Variable -> GetValue`` (x: NormalFloat, y: NormalFloat) =
    let value = V2d(nf x, nf y)
    let adapter = getAdapter value
    let result = testVariableGetValue value adapter
    approxEqual result.X value.X && approxEqual result.Y value.Y

[<Property>]
let ``V2d: ReadScalar -> GetValue`` (x: NormalFloat, y: NormalFloat) =
    let value = V2d(nf x, nf y)
    let adapter = getAdapter value
    let result = testReadScalar value adapter
    approxEqual result.X value.X && approxEqual result.Y value.Y

[<Test>]
let ``V2d: DoubleCount is 2`` () =
    let adapter = getAdapter V2d.Zero
    Assert.AreEqual(2, adapter.DoubleCount)

// ============================================================================
// Tests for V3d
// ============================================================================

[<Property>]
let ``V3d: WriteTo -> ReadValue round-trip`` (x: NormalFloat, y: NormalFloat, z: NormalFloat) =
    let value = V3d(nf x, nf y, nf z)
    let adapter = getAdapter value
    let result = testRoundTrip value adapter
    approxEqual result.X value.X && approxEqual result.Y value.Y && approxEqual result.Z value.Z

[<Property>]
let ``V3d: Variable -> GetValue`` (x: NormalFloat, y: NormalFloat, z: NormalFloat) =
    let value = V3d(nf x, nf y, nf z)
    let adapter = getAdapter value
    let result = testVariableGetValue value adapter
    approxEqual result.X value.X && approxEqual result.Y value.Y && approxEqual result.Z value.Z

[<Property>]
let ``V3d: ReadScalar -> GetValue`` (x: NormalFloat, y: NormalFloat, z: NormalFloat) =
    let value = V3d(nf x, nf y, nf z)
    let adapter = getAdapter value
    let result = testReadScalar value adapter
    approxEqual result.X value.X && approxEqual result.Y value.Y && approxEqual result.Z value.Z

[<Test>]
let ``V3d: DoubleCount is 3`` () =
    let adapter = getAdapter V3d.Zero
    Assert.AreEqual(3, adapter.DoubleCount)

// ============================================================================
// Tests for V4d
// ============================================================================

[<Property>]
let ``V4d: WriteTo -> ReadValue round-trip`` (x: NormalFloat, y: NormalFloat, z: NormalFloat, w: NormalFloat) =
    let value = V4d(nf x, nf y, nf z, nf w)
    let adapter = getAdapter value
    let result = testRoundTrip value adapter
    approxEqual result.X value.X && approxEqual result.Y value.Y &&
    approxEqual result.Z value.Z && approxEqual result.W value.W

[<Property>]
let ``V4d: Variable -> GetValue`` (x: NormalFloat, y: NormalFloat, z: NormalFloat, w: NormalFloat) =
    let value = V4d(nf x, nf y, nf z, nf w)
    let adapter = getAdapter value
    let result = testVariableGetValue value adapter
    approxEqual result.X value.X && approxEqual result.Y value.Y &&
    approxEqual result.Z value.Z && approxEqual result.W value.W

[<Property>]
let ``V4d: ReadScalar -> GetValue`` (x: NormalFloat, y: NormalFloat, z: NormalFloat, w: NormalFloat) =
    let value = V4d(nf x, nf y, nf z, nf w)
    let adapter = getAdapter value
    let result = testReadScalar value adapter
    approxEqual result.X value.X && approxEqual result.Y value.Y &&
    approxEqual result.Z value.Z && approxEqual result.W value.W

[<Test>]
let ``V4d: DoubleCount is 4`` () =
    let adapter = getAdapter V4d.Zero
    Assert.AreEqual(4, adapter.DoubleCount)

// ============================================================================
// Tests for M22d
// ============================================================================

[<Property>]
let ``M22d: WriteTo -> ReadValue round-trip`` (m00: NormalFloat, m01: NormalFloat, m10: NormalFloat, m11: NormalFloat) =
    let value = M22d(nf m00, nf m01, nf m10, nf m11)
    let adapter = getAdapter value
    let result = testRoundTrip value adapter
    approxEqual result.M00 value.M00 && approxEqual result.M01 value.M01 &&
    approxEqual result.M10 value.M10 && approxEqual result.M11 value.M11

[<Property>]
let ``M22d: Variable -> GetValue`` (m00: NormalFloat, m01: NormalFloat, m10: NormalFloat, m11: NormalFloat) =
    let value = M22d(nf m00, nf m01, nf m10, nf m11)
    let adapter = getAdapter value
    let result = testVariableGetValue value adapter
    approxEqual result.M00 value.M00 && approxEqual result.M01 value.M01 &&
    approxEqual result.M10 value.M10 && approxEqual result.M11 value.M11

[<Property>]
let ``M22d: ReadScalar -> GetValue`` (m00: NormalFloat, m01: NormalFloat, m10: NormalFloat, m11: NormalFloat) =
    let value = M22d(nf m00, nf m01, nf m10, nf m11)
    let adapter = getAdapter value
    let result = testReadScalar value adapter
    approxEqual result.M00 value.M00 && approxEqual result.M01 value.M01 &&
    approxEqual result.M10 value.M10 && approxEqual result.M11 value.M11

[<Test>]
let ``M22d: DoubleCount is 4`` () =
    let adapter = getAdapter M22d.Zero
    Assert.AreEqual(4, adapter.DoubleCount)

// ============================================================================
// Tests for M33d
// ============================================================================

[<Property>]
let ``M33d: WriteTo -> ReadValue round-trip`` (values: NormalFloat[]) =
    (values.Length >= 9) ==> lazy (
        let v = values |> Array.map nf
        let value = M33d(v.[0], v.[1], v.[2], v.[3], v.[4], v.[5], v.[6], v.[7], v.[8])
        let adapter = getAdapter value
        let result = testRoundTrip value adapter
        approxEqual result.M00 value.M00 && approxEqual result.M01 value.M01 && approxEqual result.M02 value.M02 &&
        approxEqual result.M10 value.M10 && approxEqual result.M11 value.M11 && approxEqual result.M12 value.M12 &&
        approxEqual result.M20 value.M20 && approxEqual result.M21 value.M21 && approxEqual result.M22 value.M22
    )

[<Property>]
let ``M33d: Variable -> GetValue`` (values: NormalFloat[]) =
    (values.Length >= 9) ==> lazy (
        let v = values |> Array.map nf
        let value = M33d(v.[0], v.[1], v.[2], v.[3], v.[4], v.[5], v.[6], v.[7], v.[8])
        let adapter = getAdapter value
        let result = testVariableGetValue value adapter
        approxEqual result.M00 value.M00 && approxEqual result.M01 value.M01 && approxEqual result.M02 value.M02 &&
        approxEqual result.M10 value.M10 && approxEqual result.M11 value.M11 && approxEqual result.M12 value.M12 &&
        approxEqual result.M20 value.M20 && approxEqual result.M21 value.M21 && approxEqual result.M22 value.M22
    )

[<Test>]
let ``M33d: DoubleCount is 9`` () =
    let adapter = getAdapter M33d.Zero
    Assert.AreEqual(9, adapter.DoubleCount)

// ============================================================================
// Tests for M44d
// ============================================================================

[<Property>]
let ``M44d: WriteTo -> ReadValue round-trip`` (values: NormalFloat[]) =
    (values.Length >= 16) ==> lazy (
        let v = values |> Array.map nf
        let value = M44d(v.[0], v.[1], v.[2], v.[3], v.[4], v.[5], v.[6], v.[7],
                        v.[8], v.[9], v.[10], v.[11], v.[12], v.[13], v.[14], v.[15])
        let adapter = getAdapter value
        let result = testRoundTrip value adapter
        approxEqual result.M00 value.M00 && approxEqual result.M01 value.M01 &&
        approxEqual result.M02 value.M02 && approxEqual result.M03 value.M03 &&
        approxEqual result.M10 value.M10 && approxEqual result.M11 value.M11 &&
        approxEqual result.M12 value.M12 && approxEqual result.M13 value.M13 &&
        approxEqual result.M20 value.M20 && approxEqual result.M21 value.M21 &&
        approxEqual result.M22 value.M22 && approxEqual result.M23 value.M23 &&
        approxEqual result.M30 value.M30 && approxEqual result.M31 value.M31 &&
        approxEqual result.M32 value.M32 && approxEqual result.M33 value.M33
    )

[<Property>]
let ``M44d: Variable -> GetValue`` (values: NormalFloat[]) =
    (values.Length >= 16) ==> lazy (
        let v = values |> Array.map nf
        let value = M44d(v.[0], v.[1], v.[2], v.[3], v.[4], v.[5], v.[6], v.[7],
                        v.[8], v.[9], v.[10], v.[11], v.[12], v.[13], v.[14], v.[15])
        let adapter = getAdapter value
        let result = testVariableGetValue value adapter
        approxEqual result.M00 value.M00 && approxEqual result.M01 value.M01 &&
        approxEqual result.M02 value.M02 && approxEqual result.M03 value.M03 &&
        approxEqual result.M10 value.M10 && approxEqual result.M11 value.M11 &&
        approxEqual result.M12 value.M12 && approxEqual result.M13 value.M13 &&
        approxEqual result.M20 value.M20 && approxEqual result.M21 value.M21 &&
        approxEqual result.M22 value.M22 && approxEqual result.M23 value.M23 &&
        approxEqual result.M30 value.M30 && approxEqual result.M31 value.M31 &&
        approxEqual result.M32 value.M32 && approxEqual result.M33 value.M33
    )

[<Test>]
let ``M44d: DoubleCount is 16`` () =
    let adapter = getAdapter M44d.Zero
    Assert.AreEqual(16, adapter.DoubleCount)

// ============================================================================
// Tests for Rot2d
// ============================================================================

[<Property>]
let ``Rot2d: WriteTo -> ReadValue round-trip`` (angle: NormalFloat) =
    let value = Rot2d(nf angle)
    let adapter = getAdapter value
    let result = testRoundTrip value adapter
    approxEqual result.Angle value.Angle

[<Property>]
let ``Rot2d: Variable -> GetValue`` (angle: NormalFloat) =
    let value = Rot2d(nf angle)
    let adapter = getAdapter value
    let result = testVariableGetValue value adapter
    approxEqual result.Angle value.Angle

[<Property>]
let ``Rot2d: ReadScalar -> GetValue`` (angle: NormalFloat) =
    let value = Rot2d(nf angle)
    let adapter = getAdapter value
    let result = testReadScalar value adapter
    approxEqual result.Angle value.Angle

[<Test>]
let ``Rot2d: DoubleCount is 1`` () =
    let adapter = getAdapter Rot2d.Identity
    Assert.AreEqual(1, adapter.DoubleCount)

// ============================================================================
// Tests for Rot3d (angle-axis representation)
// ============================================================================

[<Property>]
let ``Rot3d: WriteTo -> ReadValue round-trip`` (x: NormalFloat, y: NormalFloat, z: NormalFloat) =
    let axis = V3d(nf x, nf y, nf z)
    (Vec.lengthSquared axis > 1e-10) ==> lazy (
        let value = Rot3d.FromAngleAxis(axis)
        let adapter = getAdapter value
        let result = testRoundTrip value adapter
        let aa1 = value.ToAngleAxis()
        let aa2 = result.ToAngleAxis()
        approxEqual aa1.X aa2.X && approxEqual aa1.Y aa2.Y && approxEqual aa1.Z aa2.Z
    )

[<Property>]
let ``Rot3d: Variable -> GetValue`` (x: NormalFloat, y: NormalFloat, z: NormalFloat) =
    let axis = V3d(nf x, nf y, nf z)
    (Vec.lengthSquared axis > 1e-10) ==> lazy (
        let value = Rot3d.FromAngleAxis(axis)
        let adapter = getAdapter value
        let result = testVariableGetValue value adapter
        let aa1 = value.ToAngleAxis()
        let aa2 = result.ToAngleAxis()
        approxEqual aa1.X aa2.X && approxEqual aa1.Y aa2.Y && approxEqual aa1.Z aa2.Z
    )

[<Test>]
let ``Rot3d: DoubleCount is 3`` () =
    let adapter = getAdapter Rot3d.Identity
    Assert.AreEqual(3, adapter.DoubleCount)

// ============================================================================
// Tests for Euclidean2d
// ============================================================================

[<Property>]
let ``Euclidean2d: WriteTo -> ReadValue round-trip`` (angle: NormalFloat, tx: NormalFloat, ty: NormalFloat) =
    let value = Euclidean2d(Rot2d(nf angle), V2d(nf tx, nf ty))
    let adapter = getAdapter value
    let result = testRoundTrip value adapter
    approxEqual result.Rot.Angle value.Rot.Angle &&
    approxEqual result.Trans.X value.Trans.X && approxEqual result.Trans.Y value.Trans.Y

[<Property>]
let ``Euclidean2d: Variable -> GetValue`` (angle: NormalFloat, tx: NormalFloat, ty: NormalFloat) =
    let value = Euclidean2d(Rot2d(nf angle), V2d(nf tx, nf ty))
    let adapter = getAdapter value
    let result = testVariableGetValue value adapter
    approxEqual result.Rot.Angle value.Rot.Angle &&
    approxEqual result.Trans.X value.Trans.X && approxEqual result.Trans.Y value.Trans.Y

[<Property>]
let ``Euclidean2d: ReadScalar -> GetValue`` (angle: NormalFloat, tx: NormalFloat, ty: NormalFloat) =
    let value = Euclidean2d(Rot2d(nf angle), V2d(nf tx, nf ty))
    let adapter = getAdapter value
    let result = testReadScalar value adapter
    approxEqual result.Rot.Angle value.Rot.Angle &&
    approxEqual result.Trans.X value.Trans.X && approxEqual result.Trans.Y value.Trans.Y

[<Test>]
let ``Euclidean2d: DoubleCount is 3`` () =
    let adapter = getAdapter Euclidean2d.Identity
    Assert.AreEqual(3, adapter.DoubleCount)

// ============================================================================
// Tests for Euclidean3d
// ============================================================================

[<Property>]
let ``Euclidean3d: WriteTo -> ReadValue round-trip`` (rx: NormalFloat, ry: NormalFloat, rz: NormalFloat, tx: NormalFloat, ty: NormalFloat, tz: NormalFloat) =
    let axis = V3d(nf rx, nf ry, nf rz)
    (Vec.lengthSquared axis > 1e-10) ==> lazy (
        let value = Euclidean3d(Rot3d.FromAngleAxis(axis), V3d(nf tx, nf ty, nf tz))
        let adapter = getAdapter value
        let result = testRoundTrip value adapter
        let aa1 = value.Rot.ToAngleAxis()
        let aa2 = result.Rot.ToAngleAxis()
        approxEqual aa1.X aa2.X && approxEqual aa1.Y aa2.Y && approxEqual aa1.Z aa2.Z &&
        approxEqual result.Trans.X value.Trans.X &&
        approxEqual result.Trans.Y value.Trans.Y &&
        approxEqual result.Trans.Z value.Trans.Z
    )

[<Property>]
let ``Euclidean3d: Variable -> GetValue`` (rx: NormalFloat, ry: NormalFloat, rz: NormalFloat, tx: NormalFloat, ty: NormalFloat, tz: NormalFloat) =
    let axis = V3d(nf rx, nf ry, nf rz)
    (Vec.lengthSquared axis > 1e-10) ==> lazy (
        let value = Euclidean3d(Rot3d.FromAngleAxis(axis), V3d(nf tx, nf ty, nf tz))
        let adapter = getAdapter value
        let result = testVariableGetValue value adapter
        let aa1 = value.Rot.ToAngleAxis()
        let aa2 = result.Rot.ToAngleAxis()
        approxEqual aa1.X aa2.X && approxEqual aa1.Y aa2.Y && approxEqual aa1.Z aa2.Z &&
        approxEqual result.Trans.X value.Trans.X &&
        approxEqual result.Trans.Y value.Trans.Y &&
        approxEqual result.Trans.Z value.Trans.Z
    )

[<Test>]
let ``Euclidean3d: DoubleCount is 6`` () =
    let adapter = getAdapter Euclidean3d.Identity
    Assert.AreEqual(6, adapter.DoubleCount)

// ============================================================================
// Tests for Similarity2d
// ============================================================================

[<Property>]
let ``Similarity2d: WriteTo -> ReadValue round-trip`` (angle: NormalFloat, tx: NormalFloat, ty: NormalFloat, scale: NormalFloat) =
    let s = abs (nf scale) + 0.01
    let value = Similarity2d(s, Euclidean2d(Rot2d(nf angle), V2d(nf tx, nf ty)))
    let adapter = getAdapter value
    let result = testRoundTrip value adapter
    approxEqual result.Scale value.Scale &&
    approxEqual result.Rot.Angle value.Rot.Angle &&
    approxEqual result.Trans.X value.Trans.X && approxEqual result.Trans.Y value.Trans.Y

[<Property>]
let ``Similarity2d: Variable -> GetValue`` (angle: NormalFloat, tx: NormalFloat, ty: NormalFloat, scale: NormalFloat) =
    let s = abs (nf scale) + 0.01
    let value = Similarity2d(s, Euclidean2d(Rot2d(nf angle), V2d(nf tx, nf ty)))
    let adapter = getAdapter value
    let result = testVariableGetValue value adapter
    approxEqual result.Scale value.Scale &&
    approxEqual result.Rot.Angle value.Rot.Angle &&
    approxEqual result.Trans.X value.Trans.X && approxEqual result.Trans.Y value.Trans.Y

[<Test>]
let ``Similarity2d: DoubleCount is 4`` () =
    let adapter = getAdapter Similarity2d.Identity
    Assert.AreEqual(4, adapter.DoubleCount)

// ============================================================================
// Tests for Similarity3d
// ============================================================================

[<Property>]
let ``Similarity3d: WriteTo -> ReadValue round-trip`` (rx: NormalFloat, ry: NormalFloat, rz: NormalFloat, tx: NormalFloat, ty: NormalFloat, tz: NormalFloat, scale: NormalFloat) =
    let axis = V3d(nf rx, nf ry, nf rz)
    let s = abs (nf scale) + 0.01
    (Vec.lengthSquared axis > 1e-10) ==> lazy (
        let value = Similarity3d(s, Euclidean3d(Rot3d.FromAngleAxis(axis), V3d(nf tx, nf ty, nf tz)))
        let adapter = getAdapter value
        let result = testRoundTrip value adapter
        approxEqual result.Scale value.Scale &&
        approxEqual result.Trans.X value.Trans.X &&
        approxEqual result.Trans.Y value.Trans.Y &&
        approxEqual result.Trans.Z value.Trans.Z
    )

[<Property>]
let ``Similarity3d: Variable -> GetValue`` (rx: NormalFloat, ry: NormalFloat, rz: NormalFloat, tx: NormalFloat, ty: NormalFloat, tz: NormalFloat, scale: NormalFloat) =
    let axis = V3d(nf rx, nf ry, nf rz)
    let s = abs (nf scale) + 0.01
    (Vec.lengthSquared axis > 1e-10) ==> lazy (
        let value = Similarity3d(s, Euclidean3d(Rot3d.FromAngleAxis(axis), V3d(nf tx, nf ty, nf tz)))
        let adapter = getAdapter value
        let result = testVariableGetValue value adapter
        approxEqual result.Scale value.Scale &&
        approxEqual result.Trans.X value.Trans.X &&
        approxEqual result.Trans.Y value.Trans.Y &&
        approxEqual result.Trans.Z value.Trans.Z
    )

[<Test>]
let ``Similarity3d: DoubleCount is 7`` () =
    let adapter = getAdapter Similarity3d.Identity
    Assert.AreEqual(7, adapter.DoubleCount)

// ============================================================================
// Tests for Circle2d
// ============================================================================

[<Property>]
let ``Circle2d: WriteTo -> ReadValue round-trip`` (cx: NormalFloat, cy: NormalFloat, r: NormalFloat) =
    let radius = abs (nf r) + 0.01
    let value = Circle2d(V2d(nf cx, nf cy), radius)
    let adapter = getAdapter value
    let result = testRoundTrip value adapter
    approxEqual result.Radius value.Radius &&
    approxEqual result.Center.X value.Center.X && approxEqual result.Center.Y value.Center.Y

[<Property>]
let ``Circle2d: Variable -> GetValue`` (cx: NormalFloat, cy: NormalFloat, r: NormalFloat) =
    let radius = abs (nf r) + 0.01
    let value = Circle2d(V2d(nf cx, nf cy), radius)
    let adapter = getAdapter value
    let result = testVariableGetValue value adapter
    approxEqual result.Radius value.Radius &&
    approxEqual result.Center.X value.Center.X && approxEqual result.Center.Y value.Center.Y

[<Property>]
let ``Circle2d: ReadScalar -> GetValue`` (cx: NormalFloat, cy: NormalFloat, r: NormalFloat) =
    let radius = abs (nf r) + 0.01
    let value = Circle2d(V2d(nf cx, nf cy), radius)
    let adapter = getAdapter value
    let result = testReadScalar value adapter
    approxEqual result.Radius value.Radius &&
    approxEqual result.Center.X value.Center.X && approxEqual result.Center.Y value.Center.Y

[<Test>]
let ``Circle2d: DoubleCount is 3`` () =
    let adapter = getAdapter (Circle2d(V2d.Zero, 1.0))
    Assert.AreEqual(3, adapter.DoubleCount)

// ============================================================================
// Tests for Sphere3d
// ============================================================================

[<Property>]
let ``Sphere3d: WriteTo -> ReadValue round-trip`` (cx: NormalFloat, cy: NormalFloat, cz: NormalFloat, r: NormalFloat) =
    let radius = abs (nf r) + 0.01
    let value = Sphere3d(V3d(nf cx, nf cy, nf cz), radius)
    let adapter = getAdapter value
    let result = testRoundTrip value adapter
    approxEqual result.Radius value.Radius &&
    approxEqual result.Center.X value.Center.X &&
    approxEqual result.Center.Y value.Center.Y &&
    approxEqual result.Center.Z value.Center.Z

[<Property>]
let ``Sphere3d: Variable -> GetValue`` (cx: NormalFloat, cy: NormalFloat, cz: NormalFloat, r: NormalFloat) =
    let radius = abs (nf r) + 0.01
    let value = Sphere3d(V3d(nf cx, nf cy, nf cz), radius)
    let adapter = getAdapter value
    let result = testVariableGetValue value adapter
    approxEqual result.Radius value.Radius &&
    approxEqual result.Center.X value.Center.X &&
    approxEqual result.Center.Y value.Center.Y &&
    approxEqual result.Center.Z value.Center.Z

[<Property>]
let ``Sphere3d: ReadScalar -> GetValue`` (cx: NormalFloat, cy: NormalFloat, cz: NormalFloat, r: NormalFloat) =
    let radius = abs (nf r) + 0.01
    let value = Sphere3d(V3d(nf cx, nf cy, nf cz), radius)
    let adapter = getAdapter value
    let result = testReadScalar value adapter
    approxEqual result.Radius value.Radius &&
    approxEqual result.Center.X value.Center.X &&
    approxEqual result.Center.Y value.Center.Y &&
    approxEqual result.Center.Z value.Center.Z

[<Test>]
let ``Sphere3d: DoubleCount is 4`` () =
    let adapter = getAdapter (Sphere3d(V3d.Zero, 1.0))
    Assert.AreEqual(4, adapter.DoubleCount)

// ============================================================================
// Tests for Plane2d
// ============================================================================

[<Property>]
let ``Plane2d: WriteTo -> ReadValue round-trip`` (angle: NormalFloat, dist: NormalFloat) =
    let normal = V2d(cos (nf angle), sin (nf angle))
    let value = Plane2d(normal, nf dist)
    let adapter = getAdapter value
    let result = testRoundTrip value adapter
    let norm1 = Vec.normalize value.Normal
    let norm2 = Vec.normalize result.Normal
    let dist1 = value.Distance / Vec.length value.Normal
    let dist2 = result.Distance / Vec.length result.Normal
    approxEqual norm1.X norm2.X && approxEqual norm1.Y norm2.Y && approxEqual dist1 dist2

[<Property>]
let ``Plane2d: Variable -> GetValue`` (angle: NormalFloat, dist: NormalFloat) =
    let normal = V2d(cos (nf angle), sin (nf angle))
    let value = Plane2d(normal, nf dist)
    let adapter = getAdapter value
    let result = testVariableGetValue value adapter
    let norm1 = Vec.normalize value.Normal
    let norm2 = Vec.normalize result.Normal
    let dist1 = value.Distance / Vec.length value.Normal
    let dist2 = result.Distance / Vec.length result.Normal
    approxEqual norm1.X norm2.X && approxEqual norm1.Y norm2.Y && approxEqual dist1 dist2

[<Test>]
let ``Plane2d: DoubleCount is 2`` () =
    let adapter = getAdapter (Plane2d(V2d.XAxis, 0.0))
    Assert.AreEqual(2, adapter.DoubleCount)

// ============================================================================
// Tests for Plane3d
// ============================================================================

[<Property>]
let ``Plane3d: WriteTo -> ReadValue round-trip`` (phi: NormalFloat, theta: NormalFloat, dist: NormalFloat) =
    let t = clamp -1.5 1.5 (nf theta)
    let ct = cos t
    let normal = V3d(cos (nf phi) * ct, sin (nf phi) * ct, sin t)
    let value = Plane3d(normal, nf dist)
    let adapter = getAdapter value
    let result = testRoundTrip value adapter
    let norm1 = Vec.normalize value.Normal
    let norm2 = Vec.normalize result.Normal
    let dist1 = value.Distance / Vec.length value.Normal
    let dist2 = result.Distance / Vec.length result.Normal
    approxEqual norm1.X norm2.X && approxEqual norm1.Y norm2.Y && approxEqual norm1.Z norm2.Z && approxEqual dist1 dist2

[<Property>]
let ``Plane3d: Variable -> GetValue`` (phi: NormalFloat, theta: NormalFloat, dist: NormalFloat) =
    let t = clamp -1.5 1.5 (nf theta)
    let ct = cos t
    let normal = V3d(cos (nf phi) * ct, sin (nf phi) * ct, sin t)
    let value = Plane3d(normal, nf dist)
    let adapter = getAdapter value
    let result = testVariableGetValue value adapter
    let norm1 = Vec.normalize value.Normal
    let norm2 = Vec.normalize result.Normal
    let dist1 = value.Distance / Vec.length value.Normal
    let dist2 = result.Distance / Vec.length result.Normal
    approxEqual norm1.X norm2.X && approxEqual norm1.Y norm2.Y && approxEqual norm1.Z norm2.Z && approxEqual dist1 dist2

[<Test>]
let ``Plane3d: DoubleCount is 3`` () =
    let adapter = getAdapter (Plane3d(V3d.ZAxis, 0.0))
    Assert.AreEqual(3, adapter.DoubleCount)

[<Test>]
let ``All ScalarAdapter DoubleCount values are correct`` () =
    Assert.AreEqual(1, (getAdapter 0.0).DoubleCount, "float")
    Assert.AreEqual(2, (getAdapter V2d.Zero).DoubleCount, "V2d")
    Assert.AreEqual(3, (getAdapter V3d.Zero).DoubleCount, "V3d")
    Assert.AreEqual(4, (getAdapter V4d.Zero).DoubleCount, "V4d")
    Assert.AreEqual(4, (getAdapter M22d.Zero).DoubleCount, "M22d")
    Assert.AreEqual(9, (getAdapter M33d.Zero).DoubleCount, "M33d")
    Assert.AreEqual(16, (getAdapter M44d.Zero).DoubleCount, "M44d")
    Assert.AreEqual(1, (getAdapter Rot2d.Identity).DoubleCount, "Rot2d")
    Assert.AreEqual(3, (getAdapter Rot3d.Identity).DoubleCount, "Rot3d")
    Assert.AreEqual(3, (getAdapter Euclidean2d.Identity).DoubleCount, "Euclidean2d")
    Assert.AreEqual(6, (getAdapter Euclidean3d.Identity).DoubleCount, "Euclidean3d")
    Assert.AreEqual(4, (getAdapter Similarity2d.Identity).DoubleCount, "Similarity2d")
    Assert.AreEqual(7, (getAdapter Similarity3d.Identity).DoubleCount, "Similarity3d")
    Assert.AreEqual(3, (getAdapter (Circle2d(V2d.Zero, 1.0))).DoubleCount, "Circle2d")
    Assert.AreEqual(4, (getAdapter (Sphere3d(V3d.Zero, 1.0))).DoubleCount, "Sphere3d")
    Assert.AreEqual(2, (getAdapter (Plane2d(V2d.XAxis, 0.0))).DoubleCount, "Plane2d")
    Assert.AreEqual(3, (getAdapter (Plane3d(V3d.ZAxis, 0.0))).DoubleCount, "Plane3d")
