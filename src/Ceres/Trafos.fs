namespace Aardvark.Base

open CeresSharp

type Rot2s(angle : scalar) =
    
    member x.Angle = angle

    member x.TransformDir(d : V2s) =
        let c = cos angle
        let s = sin angle
        V2s(
            c * d.X + s * d.Y,
            c * d.Y - s * d.X
        )

    member x.TransformDir(d : V2d) =
        let c = cos angle
        let s = sin angle
        V2s(
            c * d.X + s * d.Y,
            c * d.Y - s * d.X
        )
        
    member x.InvTransformDir(d : V2s) =
        let c = cos angle
        let s = sin angle
        V2s(
            c * d.X - s * d.Y,
            c * d.Y + s * d.X
        )
        
    member x.InvTransformDir(d : V2d) =
        let c = cos angle
        let s = sin angle
        V2s(
            c * d.X - s * d.Y,
            c * d.Y + s * d.X
        )

    member x.TransformPos(p : V2s) = x.TransformDir p
    member x.TransformPos(p : V2d) = x.TransformDir p
    member x.InvTransformPos(p : V2s) = x.InvTransformDir p
    member x.InvTransformPos(p : V2d) = x.InvTransformDir p

    member x.Inverse = Rot2s(-angle)


    static member (*) (l : Rot2s, r : Rot2s) = Rot2s(l.Angle + r.Angle)
    static member (*) (l : Rot2s, r : Rot2d) = Rot2s(l.Angle + r.Angle)
    static member (*) (l : Rot2d, r : Rot2s) = Rot2s(l.Angle + r.Angle)
    
    static member Identity = Rot2s(scalar.Zero)

    member x.ToM22s() =
        let c = cos angle
        let s = sin angle
        M22s(
             c, s,
            -s, c
        )

    member x.ToM33s() =
        let c = cos angle
        let s = sin angle
        M33s(
             c, s, scalar.Zero,
            -s, c, scalar.Zero,
             scalar.Zero,  scalar.Zero, scalar.One
        )

    member x.Value = Rot2d(angle.Value)

    new(r : Rot2d) = Rot2s(scalar r.Angle)

type Rot3s(angleAxis : V3s) =

    static let asQuaternion (l : Rot3s) (f : V4s -> V4s)  =
        f (l.ToQuaternion())
        |> Rot3s.OfQuaternion

    static let asQuaternion2 (l : Rot3s) (r : Rot3s) (f : V4s -> V4s -> V4s)  =
        f (l.ToQuaternion()) (r.ToQuaternion())
        |> Rot3s.OfQuaternion

    static member private OfQuaternion(q : V4s) =
        let w = q.W
        let v = q.XYZ
        let sinTheta2 = v.LengthSquared
        if sinTheta2.Value > Constant.PositiveTinyValue then
            let sinTheta = sqrt sinTheta2
            let cosTheta = q.X
            let twoTheta = 2.0 * atan (sinTheta / cosTheta)
            Rot3s(v * (twoTheta / sinTheta))
        else
            Rot3s(v * 2.0)
        
    member private x.ToQuaternion() =
        let theta2 = angleAxis.LengthSquared
        if theta2.Value > Constant.PositiveTinyValue then
            let theta = sqrt theta2
            let thetaHalf = theta / 2.0
            let k = sin thetaHalf / theta
            V4s(k * angleAxis.X, k * angleAxis.Y, k * angleAxis.Z, cos thetaHalf)
        else
            V4s(0.5 * angleAxis.X, 0.5 * angleAxis.Y, 0.5 * angleAxis.Z, scalar.One)

    member private x.AngleAxis = angleAxis
    member x.Value = Rot3d.FromAngleAxis(angleAxis.Value)

    member x.TransformDir(d : V3s) = AngleAxis.RotatePoint(angleAxis, d)
    member x.TransformDir(d : V3d) = AngleAxis.RotatePoint(angleAxis, d)
    member x.InvTransformDir(d : V3s) = AngleAxis.RotatePoint(-angleAxis, d)
    member x.InvTransformDir(d : V3d) = AngleAxis.RotatePoint(-angleAxis, d)
    
    member x.TransformPos(d : V3s) = AngleAxis.RotatePoint(angleAxis, d)
    member x.TransformPos(d : V3d) = AngleAxis.RotatePoint(angleAxis, d)
    member x.InvTransformPos(d : V3s) = AngleAxis.RotatePoint(-angleAxis, d)
    member x.InvTransformPos(d : V3d) = AngleAxis.RotatePoint(-angleAxis, d)

    member x.Inverse = Rot3s(-angleAxis)
    member x.Conjugated = asQuaternion x (fun q -> -q)

    static member FromAngleAxis(aa : V3s) = Rot3s(aa)
    member x.ToAngleAxis() = angleAxis
    
    static member Identity = Rot3s(V3s.Zero)

    static member (~-) (r : Rot3s) = asQuaternion r ((~-))

    static member (*) (l : Rot3s, r : scalar) = asQuaternion l ((*) r)
    static member (*) (l : Rot3s, r : float) = asQuaternion l ((*) r)
    static member (*) (a : scalar, b : Rot3s) = asQuaternion b ((*) a)
    static member (*) (a : float, b : Rot3s) = asQuaternion b ((*) a)
    
    static member (/) (l : Rot3s, r : scalar) = asQuaternion l (fun v -> v / r)
    static member (/) (l : Rot3s, r : float) = asQuaternion l (fun v -> v / r)
    static member (/) (l : scalar, r : Rot3s) = asQuaternion r (fun v -> l / v)
    static member (/) (l : float, r : Rot3s) = asQuaternion r (fun v -> l / v)

    static member (+) (l : Rot3s, r : Rot3s) = asQuaternion2 l r (+)
    static member (+) (l : Rot3s, r : Rot3d) = asQuaternion l (fun v -> v + V4d(r.V, r.W))
    static member (+) (l : Rot3d, r : Rot3s) = asQuaternion r (fun v -> V4d(l.V, l.W) + v)

    static member (-) (l : Rot3s, r : Rot3s) = asQuaternion2 l r (+)
    static member (-) (l : Rot3s, r : Rot3d) = asQuaternion l (fun v -> v - V4d(r.V, r.W))
    static member (-) (l : Rot3d, r : Rot3s) = asQuaternion r (fun v -> V4d(l.V, l.W) - v)
    
    static member (*) (l : Rot3s, r : Rot3s) =
        asQuaternion2 l r (fun a b ->
            V4s(
                a.W * b.W - a.X * b.X - a.Y * b.Y - a.Z * b.Z,
                a.W * b.X + a.X * b.W + a.Y * b.Z - a.Z * b.Y,
                a.W * b.Y + a.Y * b.W + a.Z * b.X - a.X * b.Z,
                a.W * b.Z + a.Z * b.W + a.X * b.Y - a.Y * b.X
            )
        )
          
    static member (*) (l : Rot3s, b : Rot3d) =
        asQuaternion l (fun a ->
            V4s(
                a.W * b.W - a.X * b.X - a.Y * b.Y - a.Z * b.Z,
                a.W * b.X + a.X * b.W + a.Y * b.Z - a.Z * b.Y,
                a.W * b.Y + a.Y * b.W + a.Z * b.X - a.X * b.Z,
                a.W * b.Z + a.Z * b.W + a.X * b.Y - a.Y * b.X
            )
        )

    static member (*) (a : Rot3d, r : Rot3s) =
        asQuaternion r (fun b ->
            V4s(
                a.W * b.W - a.X * b.X - a.Y * b.Y - a.Z * b.Z,
                a.W * b.X + a.X * b.W + a.Y * b.Z - a.Z * b.Y,
                a.W * b.Y + a.Y * b.W + a.Z * b.X - a.X * b.Z,
                a.W * b.Z + a.Z * b.W + a.X * b.Y - a.Y * b.X
            )
        )

    member x.ToM33s() =
        let r = x.ToQuaternion()
        let xx = r.X * r.X
        let yy = r.Y * r.Y
        let zz = r.Z * r.Z
        let xy = r.X * r.Y
        let xz = r.X * r.Z
        let yz = r.Y * r.Z
        let xw = r.X * r.W
        let yw = r.Y * r.W
        let zw = r.Z * r.W
        M33s(
            1.0 - 2.0 * (yy + zz),
            2.0 * (xy - zw),
            2.0 * (xz + yw),

            2.0 * (xy + zw),
            1.0 - 2.0 * (zz + xx),
            2.0 * (yz - xw),

            2.0 * (xz - yw),
            2.0 * (yz + xw),
            1.0 - 2.0 * (yy + xx)
        )

    member x.ToM44s() =
        let r = x.ToQuaternion()
        let xx = r.X * r.X
        let yy = r.Y * r.Y
        let zz = r.Z * r.Z
        let xy = r.X * r.Y
        let xz = r.X * r.Z
        let yz = r.Y * r.Z
        let xw = r.X * r.W
        let yw = r.Y * r.W
        let zw = r.Z * r.W
        M44s(
            1.0 - 2.0 * (yy + zz),
            2.0 * (xy - zw),
            2.0 * (xz + yw),
            scalar.Zero,

            2.0 * (xy + zw),
            1.0 - 2.0 * (zz + xx),
            2.0 * (yz - xw),
            scalar.Zero,

            2.0 * (xz - yw),
            2.0 * (yz + xw),
            1.0 - 2.0 * (yy + xx),
            scalar.Zero,
            
            scalar.Zero, scalar.Zero, scalar.Zero, scalar.One
        )

    override x.ToString() =
        x.Value.ToString()


    new (r : Rot3d) = Rot3s(V3s(r.ToAngleAxis()))

[<AutoOpen>]
module ``Rot Extensions`` =

    let private pickle2 (r : Rot2d) = r.Angle
    let private unpickle2 (v : float) = Rot2d(v)
    let private read2 (offset : int) (v : float) = Rot2s(read offset v)
      
    let private pickle3 (r : Rot3d) = r.ToAngleAxis()
    let private unpickle3 (v : V3d) = Rot3d.FromAngleAxis(v)
    let private read3 (offset : int) (v : V3d) = Rot3s(read offset v)

    type Problem with
        member x.AddParameterBlock(rs : Rot3d[]) = x.AddParameterBlock(rs, pickle3, unpickle3, read3)
        member x.AddParameterBlock(rs : Rot2d[]) = x.AddParameterBlock(rs, pickle2, unpickle2, read2)



type Euclidean2s(rot : Rot2s, trans : V2s) =
    member x.Rot = rot
    member x.Trans = trans
    
    static member Identity = Euclidean2s(Rot2s.Identity, V2s.Zero)

    static member (*) (a : Euclidean2s, b : Euclidean2s) =
        Euclidean2s(
            a.Rot * b.Rot,
            a.Trans + a.Rot.TransformDir(b.Trans)
        )
    
    static member (*) (a : Euclidean2s, b : Euclidean2d) =
        Euclidean2s(
            a.Rot * b.Rot,
            a.Trans + a.Rot.TransformDir(b.Trans)
        )
        
    static member (*) (a : Euclidean2d, b : Euclidean2s) =
        Euclidean2s(
            a.Rot * b.Rot,
            a.Trans + (Rot2s a.Rot).TransformDir(b.Trans)
        )

    static member (*) (a : Euclidean2s, b : Rot2s) =
        Euclidean2s(
            a.Rot * b,
            a.Trans
        )

    static member (*) (a : Euclidean2s, b : Rot2d) =
        Euclidean2s(
            a.Rot * b,
            a.Trans
        )

    static member (*) (a : Rot2s, b : Euclidean2s) =
        Euclidean2s(
            a * b.Rot,
            a.TransformDir(b.Trans)
        )

    static member (*) (a : Rot2d, b : Euclidean2s) =
        Euclidean2s(
            a * b.Rot,
            (Rot2s a).TransformDir(b.Trans)
        )

    member x.TransformDir (d : V2s) = rot.TransformDir d
    member x.TransformDir (d : V2d) = rot.TransformDir d
    member x.InvTransformDir (d : V2s) = rot.InvTransformDir d
    member x.InvTransformDir (d : V2d) = rot.InvTransformDir d
    
    member x.TransformPos (p : V2s) = rot.TransformDir p + trans
    member x.TransformPos (p : V2d) = rot.TransformDir p + trans
    member x.InvTransformPos (p : V2s) = rot.InvTransformDir (p - trans)
    member x.InvTransformPos (p : V2d) = rot.InvTransformDir (p - trans)

    member x.Value =
        Euclidean2d(rot.Value, trans.Value)

    member x.Inverse =
        let newr = rot.Inverse
        Euclidean2s(newr, -newr.TransformDir(trans))

    member x.ToM33s() =
        let r = rot.ToM22s()
        M33s(
            r.M00, r.M01, trans.X,
            r.M10, r.M11, trans.Y,
            scalar.Zero, scalar.Zero, scalar.One
        )

        
    new (e : Euclidean2d) = Euclidean2s(Rot2s e.Rot, V2s e.Trans)

type Euclidean3s(rot : Rot3s, trans : V3s) =
    member x.Rot = rot
    member x.Trans = trans
    
    static member Identity = Euclidean3s(Rot3s.Identity, V3s.Zero)

    static member (*) (a : Euclidean3s, b : Euclidean3s) =
        Euclidean3s(
            a.Rot * b.Rot,
            a.Trans + a.Rot.TransformDir(b.Trans)
        )

    static member (*) (a : Euclidean3s, b : Euclidean3d) =
        Euclidean3s(
            a.Rot * b.Rot,
            a.Trans + a.Rot.TransformDir(b.Trans)
        )

    static member (*) (a : Euclidean3d, b : Euclidean3s) =
        Euclidean3s(
            a.Rot * b.Rot,
            a.Trans + (Rot3s a.Rot).TransformDir(b.Trans)
        )

    static member (*) (a : Euclidean3s, b : Rot3s) =
        Euclidean3s(
            a.Rot * b,
            a.Trans
        )

    static member (*) (a : Euclidean3s, b : Rot3d) =
        Euclidean3s(
            a.Rot * b,
            a.Trans
        )

    static member (*) (a : Rot3s, b : Euclidean3s) =
        Euclidean3s(
            a * b.Rot,
            a.TransformDir(b.Trans)
        )

    static member (*) (a : Rot3d, b : Euclidean3s) =
        Euclidean3s(
            a * b.Rot,
            (Rot3s a).TransformDir(b.Trans)
        )

    member x.TransformDir (d : V3s) = rot.TransformDir d
    member x.TransformDir (d : V3d) = rot.TransformDir d
    member x.InvTransformDir (d : V3s) = rot.InvTransformDir d
    member x.InvTransformDir (d : V3d) = rot.InvTransformDir d
    
    member x.TransformPos (p : V3s) = rot.TransformDir p + trans
    member x.TransformPos (p : V3d) = rot.TransformDir p + trans
    member x.InvTransformPos (p : V3s) = rot.InvTransformDir (p - trans)
    member x.InvTransformPos (p : V3d) = rot.InvTransformDir (p - trans)

    member x.Value =
        Euclidean3d(rot.Value, trans.Value)

    member x.Inverse =
        let newr = rot.Inverse
        Euclidean3s(newr, -newr.TransformDir(trans))

    member x.ToM44s() =
        let r = rot.ToM33s()
        M44s(
            r.M00, r.M01, r.M02, trans.X,
            r.M10, r.M11, r.M12, trans.Y,
            r.M20, r.M21, r.M22, trans.Z,
            scalar.Zero, scalar.Zero, scalar.Zero, scalar.One
        )

        
    new (e : Euclidean3d) = Euclidean3s(Rot3s e.Rot, V3s e.Trans)

[<AutoOpen>]
module ``Euclidean Extensions`` =

    [<Struct>]
    type private EuclideanAdapter(rot : V3d, trans : V3d) =
        member x.AngleAxis = rot
        member x.Trans = trans
        
    let private pickle3 (r : Euclidean3d) = EuclideanAdapter(r.Rot.ToAngleAxis(), r.Trans)
    let private unpickle3 (v : EuclideanAdapter) = Euclidean3d(Rot3d.FromAngleAxis(v.AngleAxis), v.Trans)
    let private read3 (offset : int) (v : EuclideanAdapter) = 
        if offset < 0 then
            Euclidean3s(Rot3s(V3s v.AngleAxis), V3s v.Trans)
        else
            Euclidean3s(
                Rot3s(read offset v.AngleAxis), 
                read (offset + 3) v.Trans
            )

    let private read2 (offset : int) (v : Euclidean2d) = 
        if offset < 0 then
            Euclidean2s(Rot2s(scalar v.Rot.Angle), V2s v.Trans)
        else
            Euclidean2s(
                Rot2s(read offset v.Rot.Angle), 
                read (offset + 1) v.Trans
            )

    type Problem with
        member x.AddParameterBlock(rs : Euclidean3d[]) = x.AddParameterBlock(rs, pickle3, unpickle3, read3)
        member x.AddParameterBlock(rs : Euclidean2d[]) = x.AddParameterBlock(rs, read2)



[<Struct>]
type Similarity2d(scale : float, euclidean : Euclidean2d) =
    member x.Scale = scale
    member x.Rot = euclidean.Rot
    member x.Trans = euclidean.Trans
    member x.EuclideanTransformation = euclidean
    
    static member Identity = Similarity2d(1.0, Euclidean2d.Identity)

    override x.ToString() =
        System.String.Format(System.Globalization.CultureInfo.InvariantCulture, "[{0}, {1}]", scale, euclidean)

    static member (*) (l : Similarity2d, r : Similarity2d) =
        Similarity2d(
            l.Scale * r.Scale,
            Euclidean2d(
                l.Rot * r.Rot,
                l.Trans + l.Rot.TransformDir(l.Scale * r.Trans)
            )
        )
        
    static member (*) (l : Euclidean2d, r : Similarity2d) =
        Similarity2d(
            r.Scale,
            Euclidean2d(
                l.Rot * r.Rot,
                l.Trans + l.Rot.TransformDir(r.Trans)
            )
        )

    static member (*) (l : Similarity2d, r : Euclidean2d) =
        Similarity2d(
            l.Scale,
            Euclidean2d(
                l.Rot * r.Rot,
                l.Trans + l.Rot.TransformDir(l.Scale * r.Trans)
            )
        )
           
    static member (*) (l : Rot2d, r : Similarity2d) =
        Similarity2d(
            r.Scale,
            Euclidean2d(
                l * r.Rot,
                l.TransformDir(r.Trans)
            )
        )

    static member (*) (l : Similarity2d, r : Rot2d) =
        Similarity2d(
            l.Scale,
            Euclidean2d(
                l.Rot * r,
                l.Trans
            )
        )
        
    member x.TransformDir (d : V2d) = euclidean.TransformDir(scale * d)
    member x.InvTransformDir (d : V2d) = euclidean.InvTransformDir(d) / scale

    member x.TransformPos (d : V2d) = euclidean.TransformPos(scale * d)
    member x.InvTransformPos (d : V2d) = euclidean.InvTransformPos(d) / scale

    member x.Inverse =
        let news = 1.0 / scale
        let newr = euclidean.Inverse
        Similarity2d(news, Euclidean2d(newr.Rot, newr.Trans * news))
 
type Similarity2s(scale : scalar, euclidean : Euclidean2s) =
    member x.Scale = scale
    member x.Rot = euclidean.Rot
    member x.Trans = euclidean.Trans
    member x.EuclideanTransformation = euclidean

    static member Identity = Similarity2s(scalar.One, Euclidean2s.Identity)

    static member (*) (l : Similarity2s, r : Similarity2s) =
        Similarity2s(
            l.Scale * r.Scale,
            Euclidean2s(
                l.Rot * r.Rot,
                l.Trans + l.Rot.TransformDir(l.Scale * r.Trans)
            )
        )

    static member (*) (l : Similarity2s, r : Similarity2d) =
        Similarity2s(
            l.Scale * r.Scale,
            Euclidean2s(
                l.Rot * r.Rot,
                l.Trans + l.Rot.TransformDir(l.Scale * V2s r.Trans)
            )
        )

    static member (*) (l : Similarity2d, r : Similarity2s) =
        Similarity2s(
            l.Scale * r.Scale,
            Euclidean2s(
                l.Rot * r.Rot,
                l.Trans + (Rot2s l.Rot).TransformDir(l.Scale * r.Trans)
            )
        )

    static member (*) (l : Similarity2s, r : Euclidean2s) =
        Similarity2s(
            l.Scale,
            Euclidean2s(
                l.Rot * r.Rot,
                l.Trans + l.Rot.TransformDir(l.Scale * r.Trans)
            )
        )
        
    static member (*) (l : Similarity2s, r : Euclidean2d) =
        Similarity2s(
            l.Scale,
            Euclidean2s(
                l.Rot * r.Rot,
                l.Trans + l.Rot.TransformDir(l.Scale * V2s r.Trans)
            )
        )

    static member (*) (l : Euclidean2s, r : Similarity2s) =
        Similarity2s(
            r.Scale,
            Euclidean2s(
                l.Rot * r.Rot,
                l.Trans + l.Rot.TransformDir(r.Trans)
            )
        )
        
    static member (*) (l : Euclidean2d, r : Similarity2s) =
        Similarity2s(
            r.Scale,
            Euclidean2s(
                l.Rot * r.Rot,
                l.Trans + (Rot2s l.Rot).TransformDir(r.Trans)
            )
        )

    member x.TransformDir (d : V2s) = euclidean.TransformDir(scale * d)
    member x.TransformDir (d : V2d) = euclidean.TransformDir(scale * V2s d)
    member x.InvTransformDir (d : V2s) = euclidean.InvTransformDir(d) / scale
    member x.InvTransformDir (d : V2d) = euclidean.InvTransformDir(V2s d) / scale

    member x.TransformPos (d : V2s) = euclidean.TransformPos(scale * d)
    member x.TransformPos (d : V2d) = euclidean.TransformPos(scale * V2s d)
    member x.InvTransformPos (d : V2s) = euclidean.InvTransformPos(d) / scale
    member x.InvTransformPos (d : V2d) = euclidean.InvTransformPos(V2s d) / scale

    member x.Inverse =
        let news = scalar.One / scale
        let newr = euclidean.Inverse
        Similarity2s(news, Euclidean2s(newr.Rot, newr.Trans * news))

    new(s : Similarity2d) = Similarity2s(scalar s.Scale, Euclidean2s s.EuclideanTransformation)

type Similarity3s(scale : scalar, euclidean : Euclidean3s) =
    member x.Scale = scale
    member x.Rot = euclidean.Rot
    member x.Trans = euclidean.Trans
    member x.EuclideanTransformation = euclidean

    static member (*) (l : Similarity3s, r : Similarity3s) =
        Similarity3s(
            l.Scale * r.Scale,
            Euclidean3s(
                l.Rot * r.Rot,
                l.Trans + l.Rot.TransformDir(l.Scale * r.Trans)
            )
        )

    static member (*) (l : Similarity3s, r : Similarity3d) =
        Similarity3s(
            l.Scale * r.Scale,
            Euclidean3s(
                l.Rot * r.Rot,
                l.Trans + l.Rot.TransformDir(l.Scale * V3s r.Trans)
            )
        )

    static member (*) (l : Similarity3d, r : Similarity3s) =
        Similarity3s(
            l.Scale * r.Scale,
            Euclidean3s(
                l.Rot * r.Rot,
                l.Trans + (Rot3s l.Rot).TransformDir(l.Scale * r.Trans)
            )
        )

    static member (*) (l : Similarity3s, r : Euclidean3s) =
        Similarity3s(
            l.Scale,
            Euclidean3s(
                l.Rot * r.Rot,
                l.Trans + l.Rot.TransformDir(l.Scale * r.Trans)
            )
        )
        
    static member (*) (l : Similarity3s, r : Euclidean3d) =
        Similarity3s(
            l.Scale,
            Euclidean3s(
                l.Rot * r.Rot,
                l.Trans + l.Rot.TransformDir(l.Scale * V3s r.Trans)
            )
        )

    static member (*) (l : Euclidean3s, r : Similarity3s) =
        Similarity3s(
            r.Scale,
            Euclidean3s(
                l.Rot * r.Rot,
                l.Trans + l.Rot.TransformDir(r.Trans)
            )
        )
        
    static member (*) (l : Euclidean3d, r : Similarity3s) =
        Similarity3s(
            r.Scale,
            Euclidean3s(
                l.Rot * r.Rot,
                l.Trans + (Rot3s l.Rot).TransformDir(r.Trans)
            )
        )

    member x.TransformDir (d : V3s) = euclidean.TransformDir(scale * d)
    member x.TransformDir (d : V3d) = euclidean.TransformDir(scale * V3s d)
    member x.InvTransformDir (d : V3s) = euclidean.InvTransformDir(d) / scale
    member x.InvTransformDir (d : V3d) = euclidean.InvTransformDir(V3s d) / scale

    member x.TransformPos (d : V3s) = euclidean.TransformPos(scale * d)
    member x.TransformPos (d : V3d) = euclidean.TransformPos(scale * V3s d)
    member x.InvTransformPos (d : V3s) = euclidean.InvTransformPos(d) / scale
    member x.InvTransformPos (d : V3d) = euclidean.InvTransformPos(V3s d) / scale

    member x.Inverse =
        let news = scalar.One / scale
        let newr = euclidean.Inverse
        Similarity3s(news, Euclidean3s(newr.Rot, newr.Trans * news))

    new(s : Similarity3d) = Similarity3s(scalar s.Scale, Euclidean3s s.EuclideanTransformation)


[<AutoOpen>]
module ``Similarity Extensions`` =

    [<Struct>]
    type private SimilarityAdapter(scale : float, rot : V3d, trans : V3d) =
        member x.Scale = scale
        member x.AngleAxis = rot
        member x.Trans = trans
        
    let private pickle3 (r : Similarity3d) = SimilarityAdapter(r.Scale, r.Rot.ToAngleAxis(), r.Trans)
    let private unpickle3 (v : SimilarityAdapter) = Similarity3d(v.Scale, Euclidean3d(Rot3d.FromAngleAxis(v.AngleAxis), v.Trans))

    let private read3 (offset : int) (v : SimilarityAdapter) = 
        if offset < 0 then
            Similarity3s(scalar v.Scale, Euclidean3s(Rot3s(V3s v.AngleAxis), V3s v.Trans))
        else
            Similarity3s(
                read offset v.Scale,
                Euclidean3s(
                    Rot3s(read (offset + 1) v.AngleAxis), 
                    read (offset + 4) v.Trans
                )
            )
    let private read2 (offset : int) (v : Similarity2d) = 
        if offset < 0 then
            Similarity2s(scalar v.Scale, Euclidean2s(Rot2s v.Rot, V2s v.Trans))
        else
            Similarity2s(
                read offset v.Scale,
                Euclidean2s(
                    Rot2s(read (offset + 1) v.Rot.Angle), 
                    read (offset + 2) v.Trans
                )
            )

    type Problem with
        member x.AddParameterBlock(rs : Similarity3d[]) = x.AddParameterBlock(rs, pickle3, unpickle3, read3)
        member x.AddParameterBlock(rs : Similarity2d[]) = x.AddParameterBlock(rs, read2)
