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

