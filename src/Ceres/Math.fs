namespace Aardvark.Base

#nowarn "9"

open Microsoft.FSharp.NativeInterop
type V2s =
    struct
        val mutable public X : scalar
        val mutable public Y : scalar
        static member CeresDim = 2
        static member CeresRead(ptr : nativeptr<float>, id : int) =
            V2s(scalar.Variable(id + 0, NativePtr.get ptr (id + 0)), scalar.Variable(id + 1, NativePtr.get ptr (id + 1)))
        static member CeresWrite(value : V2s, ptr : nativeptr<float>, id : int) =
            NativePtr.set ptr (id + 0) value.X.Value; NativePtr.set ptr (id + 1) value.Y.Value
        static member Equation(l : V2s, r : V2s) = Equation [l.X - r.X; l.Y - r.Y]
        static member Equation(l : V2d, r : V2s) = Equation [l.X - r.X; l.Y - r.Y]
        static member Equation(l : V2s, r : V2d) = Equation [l.X - r.X; l.Y - r.Y]
        static member Zero = V2s(0.0, 0.0)
        static member One = V2s(1.0, 1.0)
        static member OO = V2s(0.0, 0.0)
        static member OI = V2s(0.0, 1.0)
        static member IO = V2s(1.0, 0.0)
        static member II = V2s(1.0, 1.0)
        member x.LengthSquared = x.X * x.X + x.Y * x.Y
        member x.Length = sqrt(x.X * x.X + x.Y * x.Y)
        member x.Normalized = x / x.Length
        member x.Value = V2d(x.X.Value, x.Y.Value)
        static member (~-) (v : V2s) = V2s(-v.X, -v.Y)
        static member (+) (l : V2s, r : V2s) = V2s(l.X + r.X, l.Y + r.Y)
        static member (-) (l : V2s, r : V2s) = V2s(l.X - r.X, l.Y - r.Y)
        static member (*) (l : V2s, r : V2s) = V2s(l.X * r.X, l.Y * r.Y)
        static member (/) (l : V2s, r : V2s) = V2s(l.X / r.X, l.Y / r.Y)
        static member (+) (l : V2s, r : V2d) = V2s(l.X + r.X, l.Y + r.Y)
        static member (-) (l : V2s, r : V2d) = V2s(l.X - r.X, l.Y - r.Y)
        static member (*) (l : V2s, r : V2d) = V2s(l.X * r.X, l.Y * r.Y)
        static member (/) (l : V2s, r : V2d) = V2s(l.X / r.X, l.Y / r.Y)
        static member (+) (l : V2d, r : V2s) = V2s(l.X + r.X, l.Y + r.Y)
        static member (-) (l : V2d, r : V2s) = V2s(l.X - r.X, l.Y - r.Y)
        static member (*) (l : V2d, r : V2s) = V2s(l.X * r.X, l.Y * r.Y)
        static member (/) (l : V2d, r : V2s) = V2s(l.X / r.X, l.Y / r.Y)
        static member Dot(l : V2s, r : V2s) = l.X * r.X + l.Y * r.Y
        static member Dot(l : V2s, r : V2d) = l.X * r.X + l.Y * r.Y
        static member Dot(l : V2d, r : V2s) = l.X * r.X + l.Y * r.Y
        static member (*) (l : float, r : V2s) = V2s(l * r.X, l * r.Y)
        static member (*) (l : V2s, r : float) = V2s(l.X * r, l.Y * r)
        static member (/) (l : float, r : V2s) = V2s(l / r.X, l / r.Y)
        static member (/) (l : V2s, r : float) = V2s(l.X / r, l.Y / r)
        static member (*) (l : scalar, r : V2s) = V2s(l * r.X, l * r.Y)
        static member (*) (l : V2s, r : scalar) = V2s(l.X * r, l.Y * r)
        static member (/) (l : scalar, r : V2s) = V2s(l / r.X, l / r.Y)
        static member (/) (l : V2s, r : scalar) = V2s(l.X / r, l.Y / r)
        new(x : int8, y : int8) = { X = scalar x; Y = scalar y }
        new(x : uint8, y : uint8) = { X = scalar x; Y = scalar y }
        new(x : int16, y : int16) = { X = scalar x; Y = scalar y }
        new(x : uint16, y : uint16) = { X = scalar x; Y = scalar y }
        new(x : int32, y : int32) = { X = scalar x; Y = scalar y }
        new(x : uint32, y : uint32) = { X = scalar x; Y = scalar y }
        new(x : int64, y : int64) = { X = scalar x; Y = scalar y }
        new(x : uint64, y : uint64) = { X = scalar x; Y = scalar y }
        new(x : float32, y : float32) = { X = scalar x; Y = scalar y }
        new(x : float, y : float) = { X = scalar x; Y = scalar y }
        new(x : decimal, y : decimal) = { X = scalar x; Y = scalar y }
        new(x : scalar, y : scalar) = { X = x; Y = y }
        new(v : V2i) = { X = scalar v.X; Y = scalar v.Y }
        new(v : V2d) = { X = scalar v.X; Y = scalar v.Y }
        new(v : V2f) = { X = scalar v.X; Y = scalar v.Y }
        new(v : V2l) = { X = scalar v.X; Y = scalar v.Y }
    end
type V3s =
    struct
        val mutable public X : scalar
        val mutable public Y : scalar
        val mutable public Z : scalar
        static member CeresDim = 3
        static member CeresRead(ptr : nativeptr<float>, id : int) =
            V3s(scalar.Variable(id + 0, NativePtr.get ptr (id + 0)), scalar.Variable(id + 1, NativePtr.get ptr (id + 1)), scalar.Variable(id + 2, NativePtr.get ptr (id + 2)))
        static member CeresWrite(value : V3s, ptr : nativeptr<float>, id : int) =
            NativePtr.set ptr (id + 0) value.X.Value; NativePtr.set ptr (id + 1) value.Y.Value; NativePtr.set ptr (id + 2) value.Z.Value
        static member Equation(l : V3s, r : V3s) = Equation [l.X - r.X; l.Y - r.Y; l.Z - r.Z]
        static member Equation(l : V3d, r : V3s) = Equation [l.X - r.X; l.Y - r.Y; l.Z - r.Z]
        static member Equation(l : V3s, r : V3d) = Equation [l.X - r.X; l.Y - r.Y; l.Z - r.Z]
        static member Zero = V3s(0.0, 0.0, 0.0)
        static member One = V3s(1.0, 1.0, 1.0)
        static member OOO = V3s(0.0, 0.0, 0.0)
        static member OOI = V3s(0.0, 0.0, 1.0)
        static member OIO = V3s(0.0, 1.0, 0.0)
        static member OII = V3s(0.0, 1.0, 1.0)
        static member IOO = V3s(1.0, 0.0, 0.0)
        static member IOI = V3s(1.0, 0.0, 1.0)
        static member IIO = V3s(1.0, 1.0, 0.0)
        static member III = V3s(1.0, 1.0, 1.0)
        member x.LengthSquared = x.X * x.X + x.Y * x.Y + x.Z * x.Z
        member x.Length = sqrt(x.X * x.X + x.Y * x.Y + x.Z * x.Z)
        member x.Normalized = x / x.Length
        member x.Value = V3d(x.X.Value, x.Y.Value, x.Z.Value)
        member x.YZ = V2s(x.Y, x.Z)
        member x.XZ = V2s(x.X, x.Z)
        member x.XY = V2s(x.X, x.Y)
        static member (~-) (v : V3s) = V3s(-v.X, -v.Y, -v.Z)
        static member (+) (l : V3s, r : V3s) = V3s(l.X + r.X, l.Y + r.Y, l.Z + r.Z)
        static member (-) (l : V3s, r : V3s) = V3s(l.X - r.X, l.Y - r.Y, l.Z - r.Z)
        static member (*) (l : V3s, r : V3s) = V3s(l.X * r.X, l.Y * r.Y, l.Z * r.Z)
        static member (/) (l : V3s, r : V3s) = V3s(l.X / r.X, l.Y / r.Y, l.Z / r.Z)
        static member (+) (l : V3s, r : V3d) = V3s(l.X + r.X, l.Y + r.Y, l.Z + r.Z)
        static member (-) (l : V3s, r : V3d) = V3s(l.X - r.X, l.Y - r.Y, l.Z - r.Z)
        static member (*) (l : V3s, r : V3d) = V3s(l.X * r.X, l.Y * r.Y, l.Z * r.Z)
        static member (/) (l : V3s, r : V3d) = V3s(l.X / r.X, l.Y / r.Y, l.Z / r.Z)
        static member (+) (l : V3d, r : V3s) = V3s(l.X + r.X, l.Y + r.Y, l.Z + r.Z)
        static member (-) (l : V3d, r : V3s) = V3s(l.X - r.X, l.Y - r.Y, l.Z - r.Z)
        static member (*) (l : V3d, r : V3s) = V3s(l.X * r.X, l.Y * r.Y, l.Z * r.Z)
        static member (/) (l : V3d, r : V3s) = V3s(l.X / r.X, l.Y / r.Y, l.Z / r.Z)
        static member Dot(l : V3s, r : V3s) = l.X * r.X + l.Y * r.Y + l.Z * r.Z
        static member Dot(l : V3s, r : V3d) = l.X * r.X + l.Y * r.Y + l.Z * r.Z
        static member Dot(l : V3d, r : V3s) = l.X * r.X + l.Y * r.Y + l.Z * r.Z
        static member Cross(l : V3s, r : V3s) = V3s(l.Y * r.Z - l.Z * r.Y, l.Z * r.X - l.X * r.Z, l.X * r.Y - l.Y * r.X)
        static member Cross(l : V3s, r : V3d) = V3s(l.Y * r.Z - l.Z * r.Y, l.Z * r.X - l.X * r.Z, l.X * r.Y - l.Y * r.X)
        static member Cross(l : V3d, r : V3s) = V3s(l.Y * r.Z - l.Z * r.Y, l.Z * r.X - l.X * r.Z, l.X * r.Y - l.Y * r.X)
        static member (*) (l : float, r : V3s) = V3s(l * r.X, l * r.Y, l * r.Z)
        static member (*) (l : V3s, r : float) = V3s(l.X * r, l.Y * r, l.Z * r)
        static member (/) (l : float, r : V3s) = V3s(l / r.X, l / r.Y, l / r.Z)
        static member (/) (l : V3s, r : float) = V3s(l.X / r, l.Y / r, l.Z / r)
        static member (*) (l : scalar, r : V3s) = V3s(l * r.X, l * r.Y, l * r.Z)
        static member (*) (l : V3s, r : scalar) = V3s(l.X * r, l.Y * r, l.Z * r)
        static member (/) (l : scalar, r : V3s) = V3s(l / r.X, l / r.Y, l / r.Z)
        static member (/) (l : V3s, r : scalar) = V3s(l.X / r, l.Y / r, l.Z / r)

        member x.Item 
            with get(i : int) =
                match i with
                    | 0 -> x.X
                    | 1 -> x.Y
                    | 2 -> x.Z
                    | _ -> raise <| System.IndexOutOfRangeException()

        new(x : int8, y : int8, z : int8) = { X = scalar x; Y = scalar y; Z = scalar z }
        new(x : uint8, y : uint8, z : uint8) = { X = scalar x; Y = scalar y; Z = scalar z }
        new(x : int16, y : int16, z : int16) = { X = scalar x; Y = scalar y; Z = scalar z }
        new(x : uint16, y : uint16, z : uint16) = { X = scalar x; Y = scalar y; Z = scalar z }
        new(x : int32, y : int32, z : int32) = { X = scalar x; Y = scalar y; Z = scalar z }
        new(x : uint32, y : uint32, z : uint32) = { X = scalar x; Y = scalar y; Z = scalar z }
        new(x : int64, y : int64, z : int64) = { X = scalar x; Y = scalar y; Z = scalar z }
        new(x : uint64, y : uint64, z : uint64) = { X = scalar x; Y = scalar y; Z = scalar z }
        new(x : float32, y : float32, z : float32) = { X = scalar x; Y = scalar y; Z = scalar z }
        new(x : float, y : float, z : float) = { X = scalar x; Y = scalar y; Z = scalar z }
        new(x : decimal, y : decimal, z : decimal) = { X = scalar x; Y = scalar y; Z = scalar z }
        new(x : scalar, y : scalar, z : scalar) = { X = x; Y = y; Z = z }
        new(v : V3i) = { X = scalar v.X; Y = scalar v.Y; Z = scalar v.Z }
        new(v : V3d) = { X = scalar v.X; Y = scalar v.Y; Z = scalar v.Z }
        new(v : V3f) = { X = scalar v.X; Y = scalar v.Y; Z = scalar v.Z }
        new(v : V3l) = { X = scalar v.X; Y = scalar v.Y; Z = scalar v.Z }
    end
type V4s =
    struct
        val mutable public X : scalar
        val mutable public Y : scalar
        val mutable public Z : scalar
        val mutable public W : scalar
        static member CeresDim = 4
        static member CeresRead(ptr : nativeptr<float>, id : int) =
            V4s(scalar.Variable(id + 0, NativePtr.get ptr (id + 0)), scalar.Variable(id + 1, NativePtr.get ptr (id + 1)), scalar.Variable(id + 2, NativePtr.get ptr (id + 2)), scalar.Variable(id + 3, NativePtr.get ptr (id + 3)))
        static member CeresWrite(value : V4s, ptr : nativeptr<float>, id : int) =
            NativePtr.set ptr (id + 0) value.X.Value; NativePtr.set ptr (id + 1) value.Y.Value; NativePtr.set ptr (id + 2) value.Z.Value; NativePtr.set ptr (id + 3) value.W.Value
        static member Equation(l : V4s, r : V4s) = Equation [l.X - r.X; l.Y - r.Y; l.Z - r.Z; l.W - r.W]
        static member Equation(l : V4d, r : V4s) = Equation [l.X - r.X; l.Y - r.Y; l.Z - r.Z; l.W - r.W]
        static member Equation(l : V4s, r : V4d) = Equation [l.X - r.X; l.Y - r.Y; l.Z - r.Z; l.W - r.W]
        static member Zero = V4s(0.0, 0.0, 0.0, 0.0)
        static member One = V4s(1.0, 1.0, 1.0, 1.0)
        static member OOOO = V4s(0.0, 0.0, 0.0, 0.0)
        static member OOOI = V4s(0.0, 0.0, 0.0, 1.0)
        static member OOIO = V4s(0.0, 0.0, 1.0, 0.0)
        static member OOII = V4s(0.0, 0.0, 1.0, 1.0)
        static member OIOO = V4s(0.0, 1.0, 0.0, 0.0)
        static member OIOI = V4s(0.0, 1.0, 0.0, 1.0)
        static member OIIO = V4s(0.0, 1.0, 1.0, 0.0)
        static member OIII = V4s(0.0, 1.0, 1.0, 1.0)
        static member IOOO = V4s(1.0, 0.0, 0.0, 0.0)
        static member IOOI = V4s(1.0, 0.0, 0.0, 1.0)
        static member IOIO = V4s(1.0, 0.0, 1.0, 0.0)
        static member IOII = V4s(1.0, 0.0, 1.0, 1.0)
        static member IIOO = V4s(1.0, 1.0, 0.0, 0.0)
        static member IIOI = V4s(1.0, 1.0, 0.0, 1.0)
        static member IIIO = V4s(1.0, 1.0, 1.0, 0.0)
        static member IIII = V4s(1.0, 1.0, 1.0, 1.0)
        member x.LengthSquared = x.X * x.X + x.Y * x.Y + x.Z * x.Z + x.W * x.W
        member x.Length = sqrt(x.X * x.X + x.Y * x.Y + x.Z * x.Z + x.W * x.W)
        member x.Normalized = x / x.Length
        member x.Value = V4d(x.X.Value, x.Y.Value, x.Z.Value, x.W.Value)
        member x.ZW = V2s(x.Z, x.W)
        member x.YW = V2s(x.Y, x.W)
        member x.YZ = V2s(x.Y, x.Z)
        member x.XW = V2s(x.X, x.W)
        member x.XZ = V2s(x.X, x.Z)
        member x.XY = V2s(x.X, x.Y)
        member x.YZW = V3s(x.Y, x.Z, x.W)
        member x.XZW = V3s(x.X, x.Z, x.W)
        member x.XYW = V3s(x.X, x.Y, x.W)
        member x.XYZ = V3s(x.X, x.Y, x.Z)
        static member (~-) (v : V4s) = V4s(-v.X, -v.Y, -v.Z, -v.W)
        static member (+) (l : V4s, r : V4s) = V4s(l.X + r.X, l.Y + r.Y, l.Z + r.Z, l.W + r.W)
        static member (-) (l : V4s, r : V4s) = V4s(l.X - r.X, l.Y - r.Y, l.Z - r.Z, l.W - r.W)
        static member (*) (l : V4s, r : V4s) = V4s(l.X * r.X, l.Y * r.Y, l.Z * r.Z, l.W * r.W)
        static member (/) (l : V4s, r : V4s) = V4s(l.X / r.X, l.Y / r.Y, l.Z / r.Z, l.W / r.W)
        static member (+) (l : V4s, r : V4d) = V4s(l.X + r.X, l.Y + r.Y, l.Z + r.Z, l.W + r.W)
        static member (-) (l : V4s, r : V4d) = V4s(l.X - r.X, l.Y - r.Y, l.Z - r.Z, l.W - r.W)
        static member (*) (l : V4s, r : V4d) = V4s(l.X * r.X, l.Y * r.Y, l.Z * r.Z, l.W * r.W)
        static member (/) (l : V4s, r : V4d) = V4s(l.X / r.X, l.Y / r.Y, l.Z / r.Z, l.W / r.W)
        static member (+) (l : V4d, r : V4s) = V4s(l.X + r.X, l.Y + r.Y, l.Z + r.Z, l.W + r.W)
        static member (-) (l : V4d, r : V4s) = V4s(l.X - r.X, l.Y - r.Y, l.Z - r.Z, l.W - r.W)
        static member (*) (l : V4d, r : V4s) = V4s(l.X * r.X, l.Y * r.Y, l.Z * r.Z, l.W * r.W)
        static member (/) (l : V4d, r : V4s) = V4s(l.X / r.X, l.Y / r.Y, l.Z / r.Z, l.W / r.W)
        static member Dot(l : V4s, r : V4s) = l.X * r.X + l.Y * r.Y + l.Z * r.Z + l.W * r.W
        static member Dot(l : V4s, r : V4d) = l.X * r.X + l.Y * r.Y + l.Z * r.Z + l.W * r.W
        static member Dot(l : V4d, r : V4s) = l.X * r.X + l.Y * r.Y + l.Z * r.Z + l.W * r.W
        static member (*) (l : float, r : V4s) = V4s(l * r.X, l * r.Y, l * r.Z, l * r.W)
        static member (*) (l : V4s, r : float) = V4s(l.X * r, l.Y * r, l.Z * r, l.W * r)
        static member (/) (l : float, r : V4s) = V4s(l / r.X, l / r.Y, l / r.Z, l / r.W)
        static member (/) (l : V4s, r : float) = V4s(l.X / r, l.Y / r, l.Z / r, l.W / r)
        static member (*) (l : scalar, r : V4s) = V4s(l * r.X, l * r.Y, l * r.Z, l * r.W)
        static member (*) (l : V4s, r : scalar) = V4s(l.X * r, l.Y * r, l.Z * r, l.W * r)
        static member (/) (l : scalar, r : V4s) = V4s(l / r.X, l / r.Y, l / r.Z, l / r.W)
        static member (/) (l : V4s, r : scalar) = V4s(l.X / r, l.Y / r, l.Z / r, l.W / r)
        new(x : int8, y : int8, z : int8, w : int8) = { X = scalar x; Y = scalar y; Z = scalar z; W = scalar w }
        new(x : uint8, y : uint8, z : uint8, w : uint8) = { X = scalar x; Y = scalar y; Z = scalar z; W = scalar w }
        new(x : int16, y : int16, z : int16, w : int16) = { X = scalar x; Y = scalar y; Z = scalar z; W = scalar w }
        new(x : uint16, y : uint16, z : uint16, w : uint16) = { X = scalar x; Y = scalar y; Z = scalar z; W = scalar w }
        new(x : int32, y : int32, z : int32, w : int32) = { X = scalar x; Y = scalar y; Z = scalar z; W = scalar w }
        new(x : uint32, y : uint32, z : uint32, w : uint32) = { X = scalar x; Y = scalar y; Z = scalar z; W = scalar w }
        new(x : int64, y : int64, z : int64, w : int64) = { X = scalar x; Y = scalar y; Z = scalar z; W = scalar w }
        new(x : uint64, y : uint64, z : uint64, w : uint64) = { X = scalar x; Y = scalar y; Z = scalar z; W = scalar w }
        new(x : float32, y : float32, z : float32, w : float32) = { X = scalar x; Y = scalar y; Z = scalar z; W = scalar w }
        new(x : float, y : float, z : float, w : float) = { X = scalar x; Y = scalar y; Z = scalar z; W = scalar w }
        new(x : decimal, y : decimal, z : decimal, w : decimal) = { X = scalar x; Y = scalar y; Z = scalar z; W = scalar w }
        new(x : scalar, y : scalar, z : scalar, w : scalar) = { X = x; Y = y; Z = z; W = w }
        new(v : V4i) = { X = scalar v.X; Y = scalar v.Y; Z = scalar v.Z; W = scalar v.W }
        new(v : V4d) = { X = scalar v.X; Y = scalar v.Y; Z = scalar v.Z; W = scalar v.W }
        new(v : V4f) = { X = scalar v.X; Y = scalar v.Y; Z = scalar v.Z; W = scalar v.W }
        new(v : V4l) = { X = scalar v.X; Y = scalar v.Y; Z = scalar v.Z; W = scalar v.W }
    end
type M22s =
    struct
        val mutable public M00 : scalar
        val mutable public M01 : scalar
        val mutable public M10 : scalar
        val mutable public M11 : scalar
        static member CeresDim = 4
        static member CeresRead(ptr : nativeptr<float>, id : int) =
            M22s(scalar.Variable(id + 0, NativePtr.get ptr (id + 0)), scalar.Variable(id + 1, NativePtr.get ptr (id + 1)), scalar.Variable(id + 2, NativePtr.get ptr (id + 2)), scalar.Variable(id + 3, NativePtr.get ptr (id + 3)))
        static member CeresWrite(value : M22s, ptr : nativeptr<float>, id : int) =
            NativePtr.set ptr (id + 0) value.M00.Value; NativePtr.set ptr (id + 1) value.M01.Value; NativePtr.set ptr (id + 2) value.M10.Value; NativePtr.set ptr (id + 3) value.M11.Value
        static member Equation(l : M22s, r : M22s) = Equation [l.M00 - r.M00; l.M01 - r.M01; l.M10 - r.M10; l.M11 - r.M11]
        static member Equation(l : M22d, r : M22s) = Equation [l.M00 - r.M00; l.M01 - r.M01; l.M10 - r.M10; l.M11 - r.M11]
        static member Equation(l : M22s, r : M22d) = Equation [l.M00 - r.M00; l.M01 - r.M01; l.M10 - r.M10; l.M11 - r.M11]
        static member Zero = M22s(0.0, 0.0, 0.0, 0.0)
        static member Identity = M22s(1.0, 0.0, 0.0, 1.0)
        static member One = M22s(1.0, 0.0, 0.0, 1.0)
        static member (+) (l : M22s, r : M22s) = M22s(l.M00 + r.M00, l.M01 + r.M01, l.M10 + r.M10, l.M11 + r.M11)
        static member (-) (l : M22s, r : M22s) = M22s(l.M00 - r.M00, l.M01 - r.M01, l.M10 - r.M10, l.M11 - r.M11)
        static member (*) (l : float, r : M22s) = M22s(l * r.M00, l * r.M01, l * r.M10, l * r.M11)
        static member (*) (l : M22s, r : float) = M22s(l.M00 * r, l.M01 * r, l.M10 * r, l.M11 * r)
        static member (/) (l : float, r : M22s) = M22s(l / r.M00, l / r.M01, l / r.M10, l / r.M11)
        static member (/) (l : M22s, r : float) = M22s(l.M00 / r, l.M01 / r, l.M10 / r, l.M11 / r)
        static member (*) (l : scalar, r : M22s) = M22s(l * r.M00, l * r.M01, l * r.M10, l * r.M11)
        static member (*) (l : M22s, r : scalar) = M22s(l.M00 * r, l.M01 * r, l.M10 * r, l.M11 * r)
        static member (/) (l : scalar, r : M22s) = M22s(l / r.M00, l / r.M01, l / r.M10, l / r.M11)
        static member (/) (l : M22s, r : scalar) = M22s(l.M00 / r, l.M01 / r, l.M10 / r, l.M11 / r)
        static member (*) (m : M22s, v : V2s) = V2s(m.M00 * v.X + m.M01 * v.Y, m.M10 * v.X + m.M11 * v.Y)
        static member (*) (l : M22s, r : M22s) = M22s(l.M00 * r.M00 + l.M01 * r.M10, l.M00 * r.M01 + l.M01 * r.M11, l.M10 * r.M00 + l.M11 * r.M10, l.M10 * r.M01 + l.M11 * r.M11)
        static member (*) (l : M22s, r : M22d) = M22s(l.M00 * r.M00 + l.M01 * r.M10, l.M00 * r.M01 + l.M01 * r.M11, l.M10 * r.M00 + l.M11 * r.M10, l.M10 * r.M01 + l.M11 * r.M11)
        static member (*) (l : M22d, r : M22s) = M22s(l.M00 * r.M00 + l.M01 * r.M10, l.M00 * r.M01 + l.M01 * r.M11, l.M10 * r.M00 + l.M11 * r.M10, l.M10 * r.M01 + l.M11 * r.M11)
        member x.Transposed = M22s(x.M00, x.M10, x.M01, x.M11)
        member x.Value = M22d(x.M00.Value, x.M01.Value, x.M10.Value, x.M11.Value)
        new(m00 : int8, m01 : int8, m10 : int8, m11 : int8) = { M00 = scalar m00; M01 = scalar m01; M10 = scalar m10; M11 = scalar m11 }
        new(m00 : uint8, m01 : uint8, m10 : uint8, m11 : uint8) = { M00 = scalar m00; M01 = scalar m01; M10 = scalar m10; M11 = scalar m11 }
        new(m00 : int16, m01 : int16, m10 : int16, m11 : int16) = { M00 = scalar m00; M01 = scalar m01; M10 = scalar m10; M11 = scalar m11 }
        new(m00 : uint16, m01 : uint16, m10 : uint16, m11 : uint16) = { M00 = scalar m00; M01 = scalar m01; M10 = scalar m10; M11 = scalar m11 }
        new(m00 : int32, m01 : int32, m10 : int32, m11 : int32) = { M00 = scalar m00; M01 = scalar m01; M10 = scalar m10; M11 = scalar m11 }
        new(m00 : uint32, m01 : uint32, m10 : uint32, m11 : uint32) = { M00 = scalar m00; M01 = scalar m01; M10 = scalar m10; M11 = scalar m11 }
        new(m00 : int64, m01 : int64, m10 : int64, m11 : int64) = { M00 = scalar m00; M01 = scalar m01; M10 = scalar m10; M11 = scalar m11 }
        new(m00 : uint64, m01 : uint64, m10 : uint64, m11 : uint64) = { M00 = scalar m00; M01 = scalar m01; M10 = scalar m10; M11 = scalar m11 }
        new(m00 : float32, m01 : float32, m10 : float32, m11 : float32) = { M00 = scalar m00; M01 = scalar m01; M10 = scalar m10; M11 = scalar m11 }
        new(m00 : float, m01 : float, m10 : float, m11 : float) = { M00 = scalar m00; M01 = scalar m01; M10 = scalar m10; M11 = scalar m11 }
        new(m00 : decimal, m01 : decimal, m10 : decimal, m11 : decimal) = { M00 = scalar m00; M01 = scalar m01; M10 = scalar m10; M11 = scalar m11 }
        new(m00 : scalar, m01 : scalar, m10 : scalar, m11 : scalar) = { M00 = m00; M01 = m01; M10 = m10; M11 = m11 }
    end
type M33s =
    struct
        val mutable public M00 : scalar
        val mutable public M01 : scalar
        val mutable public M02 : scalar
        val mutable public M10 : scalar
        val mutable public M11 : scalar
        val mutable public M12 : scalar
        val mutable public M20 : scalar
        val mutable public M21 : scalar
        val mutable public M22 : scalar
        static member CeresDim = 9
        static member CeresRead(ptr : nativeptr<float>, id : int) =
            M33s(scalar.Variable(id + 0, NativePtr.get ptr (id + 0)), scalar.Variable(id + 1, NativePtr.get ptr (id + 1)), scalar.Variable(id + 2, NativePtr.get ptr (id + 2)), scalar.Variable(id + 3, NativePtr.get ptr (id + 3)), scalar.Variable(id + 4, NativePtr.get ptr (id + 4)), scalar.Variable(id + 5, NativePtr.get ptr (id + 5)), scalar.Variable(id + 6, NativePtr.get ptr (id + 6)), scalar.Variable(id + 7, NativePtr.get ptr (id + 7)), scalar.Variable(id + 8, NativePtr.get ptr (id + 8)))
        static member CeresWrite(value : M33s, ptr : nativeptr<float>, id : int) =
            NativePtr.set ptr (id + 0) value.M00.Value; NativePtr.set ptr (id + 1) value.M01.Value; NativePtr.set ptr (id + 2) value.M02.Value; NativePtr.set ptr (id + 3) value.M10.Value; NativePtr.set ptr (id + 4) value.M11.Value; NativePtr.set ptr (id + 5) value.M12.Value; NativePtr.set ptr (id + 6) value.M20.Value; NativePtr.set ptr (id + 7) value.M21.Value; NativePtr.set ptr (id + 8) value.M22.Value
        static member Equation(l : M33s, r : M33s) = Equation [l.M00 - r.M00; l.M01 - r.M01; l.M02 - r.M02; l.M10 - r.M10; l.M11 - r.M11; l.M12 - r.M12; l.M20 - r.M20; l.M21 - r.M21; l.M22 - r.M22]
        static member Equation(l : M33d, r : M33s) = Equation [l.M00 - r.M00; l.M01 - r.M01; l.M02 - r.M02; l.M10 - r.M10; l.M11 - r.M11; l.M12 - r.M12; l.M20 - r.M20; l.M21 - r.M21; l.M22 - r.M22]
        static member Equation(l : M33s, r : M33d) = Equation [l.M00 - r.M00; l.M01 - r.M01; l.M02 - r.M02; l.M10 - r.M10; l.M11 - r.M11; l.M12 - r.M12; l.M20 - r.M20; l.M21 - r.M21; l.M22 - r.M22]
        static member Zero = M33s(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
        static member Identity = M33s(1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0)
        static member One = M33s(1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0)
        static member (+) (l : M33s, r : M33s) = M33s(l.M00 + r.M00, l.M01 + r.M01, l.M02 + r.M02, l.M10 + r.M10, l.M11 + r.M11, l.M12 + r.M12, l.M20 + r.M20, l.M21 + r.M21, l.M22 + r.M22)
        static member (-) (l : M33s, r : M33s) = M33s(l.M00 - r.M00, l.M01 - r.M01, l.M02 - r.M02, l.M10 - r.M10, l.M11 - r.M11, l.M12 - r.M12, l.M20 - r.M20, l.M21 - r.M21, l.M22 - r.M22)
        static member (*) (l : float, r : M33s) = M33s(l * r.M00, l * r.M01, l * r.M02, l * r.M10, l * r.M11, l * r.M12, l * r.M20, l * r.M21, l * r.M22)
        static member (*) (l : M33s, r : float) = M33s(l.M00 * r, l.M01 * r, l.M02 * r, l.M10 * r, l.M11 * r, l.M12 * r, l.M20 * r, l.M21 * r, l.M22 * r)
        static member (/) (l : float, r : M33s) = M33s(l / r.M00, l / r.M01, l / r.M02, l / r.M10, l / r.M11, l / r.M12, l / r.M20, l / r.M21, l / r.M22)
        static member (/) (l : M33s, r : float) = M33s(l.M00 / r, l.M01 / r, l.M02 / r, l.M10 / r, l.M11 / r, l.M12 / r, l.M20 / r, l.M21 / r, l.M22 / r)
        static member (*) (l : scalar, r : M33s) = M33s(l * r.M00, l * r.M01, l * r.M02, l * r.M10, l * r.M11, l * r.M12, l * r.M20, l * r.M21, l * r.M22)
        static member (*) (l : M33s, r : scalar) = M33s(l.M00 * r, l.M01 * r, l.M02 * r, l.M10 * r, l.M11 * r, l.M12 * r, l.M20 * r, l.M21 * r, l.M22 * r)
        static member (/) (l : scalar, r : M33s) = M33s(l / r.M00, l / r.M01, l / r.M02, l / r.M10, l / r.M11, l / r.M12, l / r.M20, l / r.M21, l / r.M22)
        static member (/) (l : M33s, r : scalar) = M33s(l.M00 / r, l.M01 / r, l.M02 / r, l.M10 / r, l.M11 / r, l.M12 / r, l.M20 / r, l.M21 / r, l.M22 / r)
        static member (*) (m : M33s, v : V3s) = V3s(m.M00 * v.X + m.M01 * v.Y + m.M02 * v.Z, m.M10 * v.X + m.M11 * v.Y + m.M12 * v.Z, m.M20 * v.X + m.M21 * v.Y + m.M22 * v.Z)
        static member (*) (l : M33s, r : M33s) = M33s(l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20, l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21, l.M00 * r.M02 + l.M01 * r.M12 + l.M02 * r.M22, l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20, l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21, l.M10 * r.M02 + l.M11 * r.M12 + l.M12 * r.M22, l.M20 * r.M00 + l.M21 * r.M10 + l.M22 * r.M20, l.M20 * r.M01 + l.M21 * r.M11 + l.M22 * r.M21, l.M20 * r.M02 + l.M21 * r.M12 + l.M22 * r.M22)
        static member (*) (l : M33s, r : M33d) = M33s(l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20, l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21, l.M00 * r.M02 + l.M01 * r.M12 + l.M02 * r.M22, l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20, l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21, l.M10 * r.M02 + l.M11 * r.M12 + l.M12 * r.M22, l.M20 * r.M00 + l.M21 * r.M10 + l.M22 * r.M20, l.M20 * r.M01 + l.M21 * r.M11 + l.M22 * r.M21, l.M20 * r.M02 + l.M21 * r.M12 + l.M22 * r.M22)
        static member (*) (l : M33d, r : M33s) = M33s(l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20, l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21, l.M00 * r.M02 + l.M01 * r.M12 + l.M02 * r.M22, l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20, l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21, l.M10 * r.M02 + l.M11 * r.M12 + l.M12 * r.M22, l.M20 * r.M00 + l.M21 * r.M10 + l.M22 * r.M20, l.M20 * r.M01 + l.M21 * r.M11 + l.M22 * r.M21, l.M20 * r.M02 + l.M21 * r.M12 + l.M22 * r.M22)
        member x.Transposed = M33s(x.M00, x.M10, x.M20, x.M01, x.M11, x.M21, x.M02, x.M12, x.M22)
        member x.Value = M33d(x.M00.Value, x.M01.Value, x.M02.Value, x.M10.Value, x.M11.Value, x.M12.Value, x.M20.Value, x.M21.Value, x.M22.Value)
        static member Rotation(angle : scalar) = 
            let ca = cos angle
            let sa = sin angle
            M33s(ca, sa, scalar 0.0, -sa, ca, scalar 0.0, scalar 0.0, scalar 0.0, scalar 1.0)
        static member Translation(x : scalar, y : scalar) = M33s(scalar 1.0, scalar 0.0, x, scalar 0.0, scalar 1.0, y, scalar 0.0, scalar 0.0, scalar 1.0)
        static member Translation(v : V2d) = M33s.Translation(scalar v.X, scalar v.Y)
        static member Translation(v : V2s) = M33s.Translation(v.X, v.Y)
        static member Scale(x : scalar, y : scalar) = M33s(x, scalar 0.0, scalar 0.0, scalar 0.0, y, scalar 0.0, scalar 0.0, scalar 0.0, scalar 1.0)
        static member Scale(f : scalar) = M33s.Scale(f,f)
        static member Scale(f : V2s) = M33s.Scale(f.X,f.Y)
        member x.TransformDir(v : V2s) = (x * V3s(v.X, v.Y, scalar 0.0)).XY
        member x.TransformPos(v : V2s) = (x * V3s(v.X, v.Y, scalar 1.0)).XY
        new(m00 : int8, m01 : int8, m02 : int8, m10 : int8, m11 : int8, m12 : int8, m20 : int8, m21 : int8, m22 : int8) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22 }
        new(m00 : uint8, m01 : uint8, m02 : uint8, m10 : uint8, m11 : uint8, m12 : uint8, m20 : uint8, m21 : uint8, m22 : uint8) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22 }
        new(m00 : int16, m01 : int16, m02 : int16, m10 : int16, m11 : int16, m12 : int16, m20 : int16, m21 : int16, m22 : int16) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22 }
        new(m00 : uint16, m01 : uint16, m02 : uint16, m10 : uint16, m11 : uint16, m12 : uint16, m20 : uint16, m21 : uint16, m22 : uint16) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22 }
        new(m00 : int32, m01 : int32, m02 : int32, m10 : int32, m11 : int32, m12 : int32, m20 : int32, m21 : int32, m22 : int32) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22 }
        new(m00 : uint32, m01 : uint32, m02 : uint32, m10 : uint32, m11 : uint32, m12 : uint32, m20 : uint32, m21 : uint32, m22 : uint32) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22 }
        new(m00 : int64, m01 : int64, m02 : int64, m10 : int64, m11 : int64, m12 : int64, m20 : int64, m21 : int64, m22 : int64) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22 }
        new(m00 : uint64, m01 : uint64, m02 : uint64, m10 : uint64, m11 : uint64, m12 : uint64, m20 : uint64, m21 : uint64, m22 : uint64) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22 }
        new(m00 : float32, m01 : float32, m02 : float32, m10 : float32, m11 : float32, m12 : float32, m20 : float32, m21 : float32, m22 : float32) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22 }
        new(m00 : float, m01 : float, m02 : float, m10 : float, m11 : float, m12 : float, m20 : float, m21 : float, m22 : float) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22 }
        new(m00 : decimal, m01 : decimal, m02 : decimal, m10 : decimal, m11 : decimal, m12 : decimal, m20 : decimal, m21 : decimal, m22 : decimal) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22 }
        new(m00 : scalar, m01 : scalar, m02 : scalar, m10 : scalar, m11 : scalar, m12 : scalar, m20 : scalar, m21 : scalar, m22 : scalar) = { M00 = m00; M01 = m01; M02 = m02; M10 = m10; M11 = m11; M12 = m12; M20 = m20; M21 = m21; M22 = m22 }
    end
type M44s =
    struct
        val mutable public M00 : scalar
        val mutable public M01 : scalar
        val mutable public M02 : scalar
        val mutable public M03 : scalar
        val mutable public M10 : scalar
        val mutable public M11 : scalar
        val mutable public M12 : scalar
        val mutable public M13 : scalar
        val mutable public M20 : scalar
        val mutable public M21 : scalar
        val mutable public M22 : scalar
        val mutable public M23 : scalar
        val mutable public M30 : scalar
        val mutable public M31 : scalar
        val mutable public M32 : scalar
        val mutable public M33 : scalar
        static member CeresDim = 16
        static member CeresRead(ptr : nativeptr<float>, id : int) =
            M44s(scalar.Variable(id + 0, NativePtr.get ptr (id + 0)), scalar.Variable(id + 1, NativePtr.get ptr (id + 1)), scalar.Variable(id + 2, NativePtr.get ptr (id + 2)), scalar.Variable(id + 3, NativePtr.get ptr (id + 3)), scalar.Variable(id + 4, NativePtr.get ptr (id + 4)), scalar.Variable(id + 5, NativePtr.get ptr (id + 5)), scalar.Variable(id + 6, NativePtr.get ptr (id + 6)), scalar.Variable(id + 7, NativePtr.get ptr (id + 7)), scalar.Variable(id + 8, NativePtr.get ptr (id + 8)), scalar.Variable(id + 9, NativePtr.get ptr (id + 9)), scalar.Variable(id + 10, NativePtr.get ptr (id + 10)), scalar.Variable(id + 11, NativePtr.get ptr (id + 11)), scalar.Variable(id + 12, NativePtr.get ptr (id + 12)), scalar.Variable(id + 13, NativePtr.get ptr (id + 13)), scalar.Variable(id + 14, NativePtr.get ptr (id + 14)), scalar.Variable(id + 15, NativePtr.get ptr (id + 15)))
        static member CeresWrite(value : M44s, ptr : nativeptr<float>, id : int) =
            NativePtr.set ptr (id + 0) value.M00.Value; NativePtr.set ptr (id + 1) value.M01.Value; NativePtr.set ptr (id + 2) value.M02.Value; NativePtr.set ptr (id + 3) value.M03.Value; NativePtr.set ptr (id + 4) value.M10.Value; NativePtr.set ptr (id + 5) value.M11.Value; NativePtr.set ptr (id + 6) value.M12.Value; NativePtr.set ptr (id + 7) value.M13.Value; NativePtr.set ptr (id + 8) value.M20.Value; NativePtr.set ptr (id + 9) value.M21.Value; NativePtr.set ptr (id + 10) value.M22.Value; NativePtr.set ptr (id + 11) value.M23.Value; NativePtr.set ptr (id + 12) value.M30.Value; NativePtr.set ptr (id + 13) value.M31.Value; NativePtr.set ptr (id + 14) value.M32.Value; NativePtr.set ptr (id + 15) value.M33.Value
        static member Equation(l : M44s, r : M44s) = Equation [l.M00 - r.M00; l.M01 - r.M01; l.M02 - r.M02; l.M03 - r.M03; l.M10 - r.M10; l.M11 - r.M11; l.M12 - r.M12; l.M13 - r.M13; l.M20 - r.M20; l.M21 - r.M21; l.M22 - r.M22; l.M23 - r.M23; l.M30 - r.M30; l.M31 - r.M31; l.M32 - r.M32; l.M33 - r.M33]
        static member Equation(l : M44d, r : M44s) = Equation [l.M00 - r.M00; l.M01 - r.M01; l.M02 - r.M02; l.M03 - r.M03; l.M10 - r.M10; l.M11 - r.M11; l.M12 - r.M12; l.M13 - r.M13; l.M20 - r.M20; l.M21 - r.M21; l.M22 - r.M22; l.M23 - r.M23; l.M30 - r.M30; l.M31 - r.M31; l.M32 - r.M32; l.M33 - r.M33]
        static member Equation(l : M44s, r : M44d) = Equation [l.M00 - r.M00; l.M01 - r.M01; l.M02 - r.M02; l.M03 - r.M03; l.M10 - r.M10; l.M11 - r.M11; l.M12 - r.M12; l.M13 - r.M13; l.M20 - r.M20; l.M21 - r.M21; l.M22 - r.M22; l.M23 - r.M23; l.M30 - r.M30; l.M31 - r.M31; l.M32 - r.M32; l.M33 - r.M33]
        static member Zero = M44s(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
        static member Identity = M44s(1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0)
        static member One = M44s(1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0)
        static member (+) (l : M44s, r : M44s) = M44s(l.M00 + r.M00, l.M01 + r.M01, l.M02 + r.M02, l.M03 + r.M03, l.M10 + r.M10, l.M11 + r.M11, l.M12 + r.M12, l.M13 + r.M13, l.M20 + r.M20, l.M21 + r.M21, l.M22 + r.M22, l.M23 + r.M23, l.M30 + r.M30, l.M31 + r.M31, l.M32 + r.M32, l.M33 + r.M33)
        static member (-) (l : M44s, r : M44s) = M44s(l.M00 - r.M00, l.M01 - r.M01, l.M02 - r.M02, l.M03 - r.M03, l.M10 - r.M10, l.M11 - r.M11, l.M12 - r.M12, l.M13 - r.M13, l.M20 - r.M20, l.M21 - r.M21, l.M22 - r.M22, l.M23 - r.M23, l.M30 - r.M30, l.M31 - r.M31, l.M32 - r.M32, l.M33 - r.M33)
        static member (*) (l : float, r : M44s) = M44s(l * r.M00, l * r.M01, l * r.M02, l * r.M03, l * r.M10, l * r.M11, l * r.M12, l * r.M13, l * r.M20, l * r.M21, l * r.M22, l * r.M23, l * r.M30, l * r.M31, l * r.M32, l * r.M33)
        static member (*) (l : M44s, r : float) = M44s(l.M00 * r, l.M01 * r, l.M02 * r, l.M03 * r, l.M10 * r, l.M11 * r, l.M12 * r, l.M13 * r, l.M20 * r, l.M21 * r, l.M22 * r, l.M23 * r, l.M30 * r, l.M31 * r, l.M32 * r, l.M33 * r)
        static member (/) (l : float, r : M44s) = M44s(l / r.M00, l / r.M01, l / r.M02, l / r.M03, l / r.M10, l / r.M11, l / r.M12, l / r.M13, l / r.M20, l / r.M21, l / r.M22, l / r.M23, l / r.M30, l / r.M31, l / r.M32, l / r.M33)
        static member (/) (l : M44s, r : float) = M44s(l.M00 / r, l.M01 / r, l.M02 / r, l.M03 / r, l.M10 / r, l.M11 / r, l.M12 / r, l.M13 / r, l.M20 / r, l.M21 / r, l.M22 / r, l.M23 / r, l.M30 / r, l.M31 / r, l.M32 / r, l.M33 / r)
        static member (*) (l : scalar, r : M44s) = M44s(l * r.M00, l * r.M01, l * r.M02, l * r.M03, l * r.M10, l * r.M11, l * r.M12, l * r.M13, l * r.M20, l * r.M21, l * r.M22, l * r.M23, l * r.M30, l * r.M31, l * r.M32, l * r.M33)
        static member (*) (l : M44s, r : scalar) = M44s(l.M00 * r, l.M01 * r, l.M02 * r, l.M03 * r, l.M10 * r, l.M11 * r, l.M12 * r, l.M13 * r, l.M20 * r, l.M21 * r, l.M22 * r, l.M23 * r, l.M30 * r, l.M31 * r, l.M32 * r, l.M33 * r)
        static member (/) (l : scalar, r : M44s) = M44s(l / r.M00, l / r.M01, l / r.M02, l / r.M03, l / r.M10, l / r.M11, l / r.M12, l / r.M13, l / r.M20, l / r.M21, l / r.M22, l / r.M23, l / r.M30, l / r.M31, l / r.M32, l / r.M33)
        static member (/) (l : M44s, r : scalar) = M44s(l.M00 / r, l.M01 / r, l.M02 / r, l.M03 / r, l.M10 / r, l.M11 / r, l.M12 / r, l.M13 / r, l.M20 / r, l.M21 / r, l.M22 / r, l.M23 / r, l.M30 / r, l.M31 / r, l.M32 / r, l.M33 / r)
        static member (*) (m : M44s, v : V4s) = V4s(m.M00 * v.X + m.M01 * v.Y + m.M02 * v.Z + m.M03 * v.W, m.M10 * v.X + m.M11 * v.Y + m.M12 * v.Z + m.M13 * v.W, m.M20 * v.X + m.M21 * v.Y + m.M22 * v.Z + m.M23 * v.W, m.M30 * v.X + m.M31 * v.Y + m.M32 * v.Z + m.M33 * v.W)
        static member (*) (l : M44s, r : M44s) = M44s(l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20 + l.M03 * r.M30, l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21 + l.M03 * r.M31, l.M00 * r.M02 + l.M01 * r.M12 + l.M02 * r.M22 + l.M03 * r.M32, l.M00 * r.M03 + l.M01 * r.M13 + l.M02 * r.M23 + l.M03 * r.M33, l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20 + l.M13 * r.M30, l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21 + l.M13 * r.M31, l.M10 * r.M02 + l.M11 * r.M12 + l.M12 * r.M22 + l.M13 * r.M32, l.M10 * r.M03 + l.M11 * r.M13 + l.M12 * r.M23 + l.M13 * r.M33, l.M20 * r.M00 + l.M21 * r.M10 + l.M22 * r.M20 + l.M23 * r.M30, l.M20 * r.M01 + l.M21 * r.M11 + l.M22 * r.M21 + l.M23 * r.M31, l.M20 * r.M02 + l.M21 * r.M12 + l.M22 * r.M22 + l.M23 * r.M32, l.M20 * r.M03 + l.M21 * r.M13 + l.M22 * r.M23 + l.M23 * r.M33, l.M30 * r.M00 + l.M31 * r.M10 + l.M32 * r.M20 + l.M33 * r.M30, l.M30 * r.M01 + l.M31 * r.M11 + l.M32 * r.M21 + l.M33 * r.M31, l.M30 * r.M02 + l.M31 * r.M12 + l.M32 * r.M22 + l.M33 * r.M32, l.M30 * r.M03 + l.M31 * r.M13 + l.M32 * r.M23 + l.M33 * r.M33)
        static member (*) (l : M44s, r : M44d) = M44s(l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20 + l.M03 * r.M30, l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21 + l.M03 * r.M31, l.M00 * r.M02 + l.M01 * r.M12 + l.M02 * r.M22 + l.M03 * r.M32, l.M00 * r.M03 + l.M01 * r.M13 + l.M02 * r.M23 + l.M03 * r.M33, l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20 + l.M13 * r.M30, l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21 + l.M13 * r.M31, l.M10 * r.M02 + l.M11 * r.M12 + l.M12 * r.M22 + l.M13 * r.M32, l.M10 * r.M03 + l.M11 * r.M13 + l.M12 * r.M23 + l.M13 * r.M33, l.M20 * r.M00 + l.M21 * r.M10 + l.M22 * r.M20 + l.M23 * r.M30, l.M20 * r.M01 + l.M21 * r.M11 + l.M22 * r.M21 + l.M23 * r.M31, l.M20 * r.M02 + l.M21 * r.M12 + l.M22 * r.M22 + l.M23 * r.M32, l.M20 * r.M03 + l.M21 * r.M13 + l.M22 * r.M23 + l.M23 * r.M33, l.M30 * r.M00 + l.M31 * r.M10 + l.M32 * r.M20 + l.M33 * r.M30, l.M30 * r.M01 + l.M31 * r.M11 + l.M32 * r.M21 + l.M33 * r.M31, l.M30 * r.M02 + l.M31 * r.M12 + l.M32 * r.M22 + l.M33 * r.M32, l.M30 * r.M03 + l.M31 * r.M13 + l.M32 * r.M23 + l.M33 * r.M33)
        static member (*) (l : M44d, r : M44s) = M44s(l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20 + l.M03 * r.M30, l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21 + l.M03 * r.M31, l.M00 * r.M02 + l.M01 * r.M12 + l.M02 * r.M22 + l.M03 * r.M32, l.M00 * r.M03 + l.M01 * r.M13 + l.M02 * r.M23 + l.M03 * r.M33, l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20 + l.M13 * r.M30, l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21 + l.M13 * r.M31, l.M10 * r.M02 + l.M11 * r.M12 + l.M12 * r.M22 + l.M13 * r.M32, l.M10 * r.M03 + l.M11 * r.M13 + l.M12 * r.M23 + l.M13 * r.M33, l.M20 * r.M00 + l.M21 * r.M10 + l.M22 * r.M20 + l.M23 * r.M30, l.M20 * r.M01 + l.M21 * r.M11 + l.M22 * r.M21 + l.M23 * r.M31, l.M20 * r.M02 + l.M21 * r.M12 + l.M22 * r.M22 + l.M23 * r.M32, l.M20 * r.M03 + l.M21 * r.M13 + l.M22 * r.M23 + l.M23 * r.M33, l.M30 * r.M00 + l.M31 * r.M10 + l.M32 * r.M20 + l.M33 * r.M30, l.M30 * r.M01 + l.M31 * r.M11 + l.M32 * r.M21 + l.M33 * r.M31, l.M30 * r.M02 + l.M31 * r.M12 + l.M32 * r.M22 + l.M33 * r.M32, l.M30 * r.M03 + l.M31 * r.M13 + l.M32 * r.M23 + l.M33 * r.M33)
        member x.Transposed = M44s(x.M00, x.M10, x.M20, x.M30, x.M01, x.M11, x.M21, x.M31, x.M02, x.M12, x.M22, x.M32, x.M03, x.M13, x.M23, x.M33)
        member x.Value = M44d(x.M00.Value, x.M01.Value, x.M02.Value, x.M03.Value, x.M10.Value, x.M11.Value, x.M12.Value, x.M13.Value, x.M20.Value, x.M21.Value, x.M22.Value, x.M23.Value, x.M30.Value, x.M31.Value, x.M32.Value, x.M33.Value)
        static member Rotation(axis : V3s, angle : scalar) = 
            let axis = axis.Normalized
            let halfAngle = angle / 2.0
            let sh = sin halfAngle
            let r = V4s(axis.X * sh, axis.Y * sh, axis.Z * sh, cos halfAngle)
            let xx = r.X * r.X
            let yy = r.Y * r.Y
            let zz = r.Z * r.Z
            let xy = r.X * r.Y
            let xz = r.X * r.Z
            let yz = r.Y * r.Z
            let xw = r.X * r.W
            let yw = r.Y * r.W
            let zw = r.Z * r.W
            M44s(1.0 - 2.0 * (yy + zz), 2.0 * (xy - zw), 2.0 * (xz + yw), scalar 0.0, 2.0 * (xy + zw), 1.0 - 2.0 * (zz + xx), 2.0 * (yz - xw), scalar 0.0, 2.0 * (xz - yw), 2.0 * (yz + xw), 1.0 - 2.0 * (yy + xx), scalar 0.0, scalar 0.0, scalar 0.0, scalar 0.0, scalar 1.0 )
        static member Translation(x : scalar, y : scalar, z : scalar) = M44s(scalar 1.0, scalar 0.0, scalar 0.0, x, scalar 0.0, scalar 1.0, scalar 0.0, y, scalar 0.0, scalar 0.0, scalar 1.0, z, scalar 0.0, scalar 0.0, scalar 0.0, scalar 1.0)
        static member Translation(v : V3d) = M44s.Translation(scalar v.X, scalar v.Y, scalar v.Z)
        static member Translation(v : V3s) = M44s.Translation(v.X, v.Y, v.Z)
        static member Scale(x : scalar, y : scalar, z : scalar) = M44s(x, scalar 0.0, scalar 0.0, scalar 0.0, scalar 0.0, y, scalar 0.0, scalar 0.0, scalar 0.0, scalar 0.0, z, scalar 0.0, scalar 0.0, scalar 0.0, scalar 0.0, scalar 1.0)
        static member Scale(f : scalar) = M44s.Scale(f,f,f)
        static member Scale(f : V3s) = M44s.Scale(f.X,f.Y,f.Z)
        member x.TransformDir(v : V3s) = (x * V4s(v.X, v.Y, v.Z, scalar 0.0)).XYZ
        member x.TransformPos(v : V3s) = (x * V4s(v.X, v.Y, v.Z, scalar 1.0)).XYZ
        new(m00 : int8, m01 : int8, m02 : int8, m03 : int8, m10 : int8, m11 : int8, m12 : int8, m13 : int8, m20 : int8, m21 : int8, m22 : int8, m23 : int8, m30 : int8, m31 : int8, m32 : int8, m33 : int8) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M03 = scalar m03; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M13 = scalar m13; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22; M23 = scalar m23; M30 = scalar m30; M31 = scalar m31; M32 = scalar m32; M33 = scalar m33 }
        new(m00 : uint8, m01 : uint8, m02 : uint8, m03 : uint8, m10 : uint8, m11 : uint8, m12 : uint8, m13 : uint8, m20 : uint8, m21 : uint8, m22 : uint8, m23 : uint8, m30 : uint8, m31 : uint8, m32 : uint8, m33 : uint8) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M03 = scalar m03; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M13 = scalar m13; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22; M23 = scalar m23; M30 = scalar m30; M31 = scalar m31; M32 = scalar m32; M33 = scalar m33 }
        new(m00 : int16, m01 : int16, m02 : int16, m03 : int16, m10 : int16, m11 : int16, m12 : int16, m13 : int16, m20 : int16, m21 : int16, m22 : int16, m23 : int16, m30 : int16, m31 : int16, m32 : int16, m33 : int16) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M03 = scalar m03; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M13 = scalar m13; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22; M23 = scalar m23; M30 = scalar m30; M31 = scalar m31; M32 = scalar m32; M33 = scalar m33 }
        new(m00 : uint16, m01 : uint16, m02 : uint16, m03 : uint16, m10 : uint16, m11 : uint16, m12 : uint16, m13 : uint16, m20 : uint16, m21 : uint16, m22 : uint16, m23 : uint16, m30 : uint16, m31 : uint16, m32 : uint16, m33 : uint16) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M03 = scalar m03; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M13 = scalar m13; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22; M23 = scalar m23; M30 = scalar m30; M31 = scalar m31; M32 = scalar m32; M33 = scalar m33 }
        new(m00 : int32, m01 : int32, m02 : int32, m03 : int32, m10 : int32, m11 : int32, m12 : int32, m13 : int32, m20 : int32, m21 : int32, m22 : int32, m23 : int32, m30 : int32, m31 : int32, m32 : int32, m33 : int32) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M03 = scalar m03; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M13 = scalar m13; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22; M23 = scalar m23; M30 = scalar m30; M31 = scalar m31; M32 = scalar m32; M33 = scalar m33 }
        new(m00 : uint32, m01 : uint32, m02 : uint32, m03 : uint32, m10 : uint32, m11 : uint32, m12 : uint32, m13 : uint32, m20 : uint32, m21 : uint32, m22 : uint32, m23 : uint32, m30 : uint32, m31 : uint32, m32 : uint32, m33 : uint32) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M03 = scalar m03; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M13 = scalar m13; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22; M23 = scalar m23; M30 = scalar m30; M31 = scalar m31; M32 = scalar m32; M33 = scalar m33 }
        new(m00 : int64, m01 : int64, m02 : int64, m03 : int64, m10 : int64, m11 : int64, m12 : int64, m13 : int64, m20 : int64, m21 : int64, m22 : int64, m23 : int64, m30 : int64, m31 : int64, m32 : int64, m33 : int64) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M03 = scalar m03; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M13 = scalar m13; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22; M23 = scalar m23; M30 = scalar m30; M31 = scalar m31; M32 = scalar m32; M33 = scalar m33 }
        new(m00 : uint64, m01 : uint64, m02 : uint64, m03 : uint64, m10 : uint64, m11 : uint64, m12 : uint64, m13 : uint64, m20 : uint64, m21 : uint64, m22 : uint64, m23 : uint64, m30 : uint64, m31 : uint64, m32 : uint64, m33 : uint64) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M03 = scalar m03; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M13 = scalar m13; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22; M23 = scalar m23; M30 = scalar m30; M31 = scalar m31; M32 = scalar m32; M33 = scalar m33 }
        new(m00 : float32, m01 : float32, m02 : float32, m03 : float32, m10 : float32, m11 : float32, m12 : float32, m13 : float32, m20 : float32, m21 : float32, m22 : float32, m23 : float32, m30 : float32, m31 : float32, m32 : float32, m33 : float32) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M03 = scalar m03; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M13 = scalar m13; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22; M23 = scalar m23; M30 = scalar m30; M31 = scalar m31; M32 = scalar m32; M33 = scalar m33 }
        new(m00 : float, m01 : float, m02 : float, m03 : float, m10 : float, m11 : float, m12 : float, m13 : float, m20 : float, m21 : float, m22 : float, m23 : float, m30 : float, m31 : float, m32 : float, m33 : float) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M03 = scalar m03; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M13 = scalar m13; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22; M23 = scalar m23; M30 = scalar m30; M31 = scalar m31; M32 = scalar m32; M33 = scalar m33 }
        new(m00 : decimal, m01 : decimal, m02 : decimal, m03 : decimal, m10 : decimal, m11 : decimal, m12 : decimal, m13 : decimal, m20 : decimal, m21 : decimal, m22 : decimal, m23 : decimal, m30 : decimal, m31 : decimal, m32 : decimal, m33 : decimal) = { M00 = scalar m00; M01 = scalar m01; M02 = scalar m02; M03 = scalar m03; M10 = scalar m10; M11 = scalar m11; M12 = scalar m12; M13 = scalar m13; M20 = scalar m20; M21 = scalar m21; M22 = scalar m22; M23 = scalar m23; M30 = scalar m30; M31 = scalar m31; M32 = scalar m32; M33 = scalar m33 }
        new(m00 : scalar, m01 : scalar, m02 : scalar, m03 : scalar, m10 : scalar, m11 : scalar, m12 : scalar, m13 : scalar, m20 : scalar, m21 : scalar, m22 : scalar, m23 : scalar, m30 : scalar, m31 : scalar, m32 : scalar, m33 : scalar) = { M00 = m00; M01 = m01; M02 = m02; M03 = m03; M10 = m10; M11 = m11; M12 = m12; M13 = m13; M20 = m20; M21 = m21; M22 = m22; M23 = m23; M30 = m30; M31 = m31; M32 = m32; M33 = m33 }
    end


type AngleAxis private() =
    static member RotatePoint(aa : V3d, p : V3d) =
        let theta2 = aa.LengthSquared
        if not (Fun.IsTiny theta2) then
            let theta = sqrt theta2
            let costheta = cos theta
            let sintheta = sin theta
            let thetainverse = 1.0 / theta

            let w = aa * thetainverse

            let wCrossP = Vec.cross w p
            let tmp = (Vec.dot w p) * (1.0 - costheta)


            (p * costheta) + (wCrossP * sintheta) + (w * tmp)

        else
            let wCrossP = Vec.cross aa p
            p + wCrossP

    static member RotatePoint(aa : V3d, p : V3s) =
        let theta2 = aa.LengthSquared
        if not (Fun.IsTiny theta2) then
            let theta = sqrt theta2
            let costheta = cos theta
            let sintheta = sin theta
            let thetainverse = 1.0 / theta

            let w = aa * thetainverse

            let wCrossP = Vec.cross (V3s w) p
            let tmp = (Vec.dot (V3s w) p) * (1.0 - costheta)


            (p * costheta) + (wCrossP * sintheta) + ((V3s w) * tmp)

        else
            let wCrossP = Vec.cross (V3s aa) p
            p + wCrossP

    static member RotatePoint(aa : V3s, p : V3s) =
        let theta2 = aa.LengthSquared
        if not (Fun.IsTiny theta2.Value) then
            let theta = sqrt theta2
            let costheta = cos theta
            let sintheta = sin theta
            let thetainverse = 1.0 / theta

            let w = aa * thetainverse

            let wCrossP = Vec.cross w p
            let tmp = (Vec.dot w p) * (1.0 - costheta)


            (p * costheta) + (wCrossP * sintheta) + (w * tmp)

        else
            let wCrossP = Vec.cross aa p
            p + wCrossP

    static member RotatePoint(aa : V3s, p : V3d) =
        let theta2 = aa.LengthSquared
        if not (Fun.IsTiny theta2.Value) then
            let theta = sqrt theta2
            let costheta = cos theta
            let sintheta = sin theta
            let thetainverse = 1.0 / theta

            let w = aa * thetainverse

            let wCrossP = Vec.cross w (V3s p)
            let tmp = (Vec.dot w (V3s p)) * (1.0 - costheta)


            ((V3s p) * costheta) + (wCrossP * sintheta) + (w * tmp)

        else
            let wCrossP = Vec.cross aa (V3s p)
            p + wCrossP

    static member Trafo(aa : V3d) =
        let x = AngleAxis.RotatePoint(aa, V3d.IOO)
        let y = AngleAxis.RotatePoint(aa, V3d.OIO)
        let z = AngleAxis.RotatePoint(aa, V3d.OOI)
        Trafo3d.FromBasis(x, y, z, V3d.Zero)
