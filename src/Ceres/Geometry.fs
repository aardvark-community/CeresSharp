namespace Aardvark.Base

open CeresSharp


type Circle2s internal(sqrtradius : scalar, center : V2s) =
    let radius = sqrtradius * sqrtradius

    member x.Center = center
    member x.Radius = radius
    member x.RadiusSquared = radius * radius
    
    member x.Area = radius * radius * Constant.Pi
    member x.Circumference = 2.0 * radius * Constant.Pi

    member x.Distance(pt : V2s) =
        Vec.length (pt - center) - radius
        
    member x.DistanceSquared(pt : V2s) =
        Vec.lengthSquared (pt - center) - radius * radius
        
    member x.Distance(pt : V2d) =
        Vec.length (pt - center) - radius
        
    member x.DistanceSquared(pt : V2d) =
        Vec.lengthSquared (pt - center) - radius * radius
        
    new(center : V2s, radius : scalar) = Circle2s(center, sqrt radius)
    new(c : Circle2d) = Circle2s(V2s c.Center, scalar (sqrt (abs c.Radius)))
    
type Sphere3s internal(sqrtradius : scalar, center : V3s) =
    let radius = sqrtradius * sqrtradius
    
    member x.Center = center
    member x.Radius = radius
    member x.RadiusSquared = radius * radius
    
    member x.Volume = (Constant.PiTimesFour * radius * radius * radius) / 3.0
    member x.SurfaceArea = Constant.PiTimesFour * radius * radius

    member x.Distance(pt : V3s) =
        Vec.length (pt - center) - radius
        
    member x.DistanceSquared(pt : V3s) =
        Vec.lengthSquared (pt - center) - radius * radius
        
    member x.Distance(pt : V3d) =
        Vec.length (pt - center) - radius
        
    member x.DistanceSquared(pt : V3d) =
        Vec.lengthSquared (pt - center) - radius * radius
             
    new(center : V3s, radius : scalar) = Sphere3s(center, sqrt radius)
    new(c : Sphere3d) = Sphere3s(V3s c.Center, scalar (sqrt (abs c.Radius)))
   
[<AutoOpen>]
module ``Circle Sphere Extensions`` =

    let private read2 (offset : int) (c : V3d) =
        if offset < 0 then
            Circle2s(scalar c.Z, V2s c.XY)
        else
            Circle2s(
                read (offset + 2) c.Z,
                read offset c.XY 
            )

    let private pickle2 (c : Circle2d) = V3d(c.Center, sqrt (abs c.Radius))
    let private unpickle2 (v : V3d) = Circle2d(v.XY, v.Z * v.Z)
   
    let private read3 (offset : int) (c : V4d) =
        if offset < 0 then
            Sphere3s(scalar c.W, V3s c.XYZ)
        else
            Sphere3s(
                read (offset + 3) c.W,
                read offset c.XYZ 
            )

    let private pickle3 (c : Sphere3d) = V4d(c.Center, sqrt (abs c.Radius))
    let private unpickle3 (v : V4d) = Sphere3d(v.XYZ, v.W * v.W)
                     
    type Problem with
        member x.AddParameterBlock(data : Circle2d[]) = x.AddParameterBlock(data, pickle2, unpickle2, read2)
        member x.AddParameterBlock(data : Sphere3d[]) = x.AddParameterBlock(data, pickle3, unpickle3, read3)



type Plane2s(angle : scalar, distance : scalar) =
    let normal =
        V2s(
            cos angle,
            sin angle
        )

    member x.Normal = normal
    member x.Distance = distance
    member x.Height (pt : V2s) = Vec.dot pt normal - distance
    member x.Height (pt : V2d) = Vec.dot (V2s pt) normal - distance
    
type Plane3s(phi : scalar, theta : scalar, distance : scalar) =
    let normal =
        let ct = cos theta
        V3s(
            cos phi * ct,
            sin phi * ct,
            sin theta
        )

    member x.Normal = normal
    member x.Distance = distance
    member x.Height (pt : V3s) = Vec.dot pt normal - distance
    member x.Height (pt : V3d) = Vec.dot (V3s pt) normal - distance
    
   
[<AutoOpen>]
module ``Plane Extensions`` =

    let private pickle2 (p : Plane2d) = 
        let p = p.Normalized
        V2d(atan2 p.Normal.Y p.Normal.X, p.Distance)

    let private unpickle2 (v : V2d) = 
        Plane2d(V2d(cos v.X, sin v.X), v.Y)

    let private read2 (offset : int) (v : V2d) =
        let v = read offset v
        Plane2s(v.X, v.Y)
        

    let private pickle3 (p : Plane3d) = 
        let p = p.Normalized
        let a = p.Normal.SphericalFromCartesian()
        V3d(a.X, a.Y, p.Distance)

    let private unpickle3 (v : V3d) = 
        let n =
            let ct = cos v.Y
            V3d(
                cos v.X * ct,
                sin v.X * ct,
                sin v.Y
            )

        Plane3d(n, v.Z)
        
    let private read3 (offset : int) (v : V3d) =
        let v = read offset v
        Plane3s(v.X, v.Y, v.Z)

    type Problem with
        member x.AddParameterBlock(data : Plane2d[]) = x.AddParameterBlock(data, pickle2, unpickle2, read2)
        member x.AddParameterBlock(data : Plane3d[]) = x.AddParameterBlock(data, pickle3, unpickle3, read3)
