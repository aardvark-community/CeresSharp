
open IpOptSharp
open Aardvark.Base

        
let test2() =
    let ta = V3d.IOO
    let tb = V3d.OIO
    let tc = V3d.OOI
    
    let ra = V3d.OIO
    let rb = V3d.IOO
    let rc = V3d.OON
    
    let rot = ref (M33d.Rotation(V3d.III.Normalized, 0.2))
    
    let (status, objective) = 
        ipopt {
            let! rot = rot
         
            IpOpt.MaxIterations 10
            IpOpt.PrintLevel 1
            IpOpt.ConstraintViolationTolerance 1E-10
            
            IpOpt.Orthonormal rot
            IpOpt.Minimize (
                Vec.lengthSquared (rot * V3s ra - ta) +
                Vec.lengthSquared (rot * V3s rb - tb) +
                Vec.lengthSquared (rot * V3s rc - tc)
            )
        }
    
    printfn "status: %A" status 
    printfn "obj:    %.4f" objective 
    printfn "rot:    %s (ortho: %A)" (rot.Value.ToString "0.0000") (rot.Value.IsOrthonormal 1E-7)
    printfn "a:      %.4f" (Vec.length (rot.Value.Transform ra - ta))
    printfn "b:      %.4f" (Vec.length (rot.Value.Transform rb - tb))
    printfn "c:      %.4f" (Vec.length (rot.Value.Transform rc - tc))
    // status: Success
    // obj:    0.0000
    // rot:    [[-0.0000, 1.0000, -0.0000], [1.0000, -0.0000, 0.0000], [0.0000, -0.0000, -1.0000]] (ortho: true)
    // a:      0.0000
    // b:      0.0000
    // c:      0.0000

    
    
  
let test () =
    
    let a = ref (V2d(-1.0, -1.0))
    let b = ref (V2d(1.0, -1.0))
    let c = ref (V2d(0.0, 1.0))
    
    let (status, objective) = 
        ipopt {
            let! a = a
            let! b = b
            let! c = c
            
            // maximize the circumference
            let circumference = Vec.length (b - a) + Vec.length (c - b) + Vec.length (a - c)
            IpOpt.Maximize circumference
            
            // constrain all vertices to the unit circle
            IpOpt.Equal(a.Length, 1.0)
            IpOpt.Equal(b.Length, 1.0)
            IpOpt.Equal(c.Length, 1.0)
        }

    printfn "status: %A" status 
    printfn "obj:    %.4f" objective 
    printfn "a:      %s (length: %.4f)" (a.Value.ToString "0.0000") a.Value.Length
    printfn "b:      %s (length: %.4f)" (b.Value.ToString "0.0000") b.Value.Length
    printfn "c:      %s (length: %.4f)" (c.Value.ToString "0.0000") c.Value.Length
    printfn "aa:     %.4f°" (Vec.AngleBetween(Vec.normalize (b.Value - a.Value), -Vec.normalize (c.Value - b.Value)) * Constant.DegreesPerRadian)
    printfn "ab:     %.4f°" (Vec.AngleBetween(Vec.normalize (c.Value - b.Value), -Vec.normalize (a.Value - c.Value)) * Constant.DegreesPerRadian)
    printfn "ac:     %.4f°" (Vec.AngleBetween(Vec.normalize (a.Value - c.Value), -Vec.normalize (b.Value - a.Value)) * Constant.DegreesPerRadian)
    // status: Success
    // obj:    5.1962
    // a:      [-0.8660, -0.5000] (length: 1.0000)
    // b:      [0.8660, -0.5000] (length: 1.0000)
    // c:      [0.0000, 1.0000] (length: 1.0000)
    // aa:     60.0000°
    // ab:     60.0000°
    // ac:     60.0000°
    
Aardvark.Init()
test()