namespace Aardvark.Base


type Circle2s (sqrtradius : scalar, center : V2s) =
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
    
type Sphere3s(sqrtradius : scalar, center : V3s) =
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
    
