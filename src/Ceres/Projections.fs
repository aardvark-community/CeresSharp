namespace Aardvark.Base

open CeresSharp

type RadialDistortion2d private(forward : float[], backward : Lazy<float[]>) =
    static let invert4 (k1 : float) (k2 : float) (k3 : float) (k4 : float) =
        let k1_2 = k1   * k1
        let k1_3 = k1_2 * k1
        let k1_4 = k1_3 * k1
        let k1_5 = k1_4 * k1
        let k1_6 = k1_5 * k1
        let k1_7 = k1_6 * k1
        let k1_8 = k1_7 * k1
        let k1_9 = k1_8 * k1
        let k2_2 = k2   * k2
        let k2_3 = k2_2 * k2
        let k2_4 = k2_3 * k2
        let k3_2 = k3   * k3
        let k3_3 = k3_2 * k3
        let k4_2 = k4   * k4

        [|
            // b1
            -k1
            
            // b2
            3.0*k1_2 - k2
            
            // b3
            -12.0*k1_3 + 8.0*k1*k2 - k3
            
            // b4
            55.0*k1_4 - 55.0*k1_2*k2 + 5.0*k2_2 + 10.0*k1*k3 - k4
            
            // b5
            -273.0*k1_5 + 364.0*k1_3*k2 - 78.0*k1*k2_2 - 78.0*k1_2*k3 + 12.0*k2*k3 + 12.0*k1*k4
            
            // b6
            1428.0*k1_6 - 2380.0*k1_4*k2 + 840.0*k1_2*k2_2 - 35.0*k2_3 + 560.0*k1_3*k3 - 210.0*k1*k2*k3 + 7.0*k3_2 - 105.0*k1_2*k4 + 14.0*k2*k4
            
            // b7
            -7752.0*k1_7 + 15504.0*k1_5*k2 - 7752.0*k1_3*k2_2 + 816.0*k1*k2_3 - 3876.0*k1_4*k3 + 2448.0*k1_2*k2*k3 - 136.0*k2_2*k3 - 136.0*k1*k3_2
            + 816.0*k1_3*k4 - 272.0*k1*k2*k4 + 16.0*k3*k4

            // b8
            43263.0*k1_8 - 100947.0*k1_6*k2 + 65835.0*k1_4*k2_2 - 11970.0*k1_2*k2_3 + 
            285.0*k2_4 + 26334.0*k1_5*k3 - 23940.0*k1_3*k2*k3 + 3420.0*k1*k2_2*k3 + 1710.0*k1_2*k3_2 - 
            171.0*k2*k3_2 - 5985.0*k1_4*k4 + 3420.0*k1_2*k2*k4 - 171.0*k2_2*k4 - 342.0*k1*k3*k4 + 9.0*k4_2

            // b9
            -246675.0*k1_9 + 657800.0*k1_7*k2 - 531300.0*k1_5*k2_2 + 141680.0*k1_3*k2_3 - 8855.0*k1*k2_4 - 177100.0*k1_6*k3 + 
            212520.0*k1_4*k2*k3 - 53130.0*k1_2*k2_2*k3 + 1540.0*k2_3*k3 - 17710.0*k1_3*k3_2 + 4620.0*k1*k2*k3_2 - 70.0*k3_3 + 42504.0*k1_5*k4 - 
            35420.0*k1_3*k2*k4 + 4620.0*k1*k2_2*k4 + 4620.0*k1_2*k3*k4 - 420.0*k2*k3*k4 - 210.0*k1*k4_2

        |]

    static let invert3 (k1 : float) (k2 : float) (k3 : float) =
        let k1_2 = k1   * k1
        let k1_3 = k1_2 * k1
        let k1_4 = k1_3 * k1
        let k1_5 = k1_4 * k1
        let k1_6 = k1_5 * k1
        let k1_7 = k1_6 * k1
        let k1_8 = k1_7 * k1
        let k1_9 = k1_8 * k1
        let k2_2 = k2   * k2
        let k2_3 = k2_2 * k2
        let k2_4 = k2_3 * k2
        let k3_2 = k3   * k3
        let k3_3 = k3_2 * k3

        [|
            // b1
            -k1
            
            // b2
            3.0*k1_2 - k2
            
            // b3
            -12.0*k1_3 + 8.0*k1*k2 - k3
            
            // b4
            55.0*k1_4 - 55.0*k1_2*k2 + 5.0*k2_2 + 10.0*k1*k3 
            
            // b5
            -273.0*k1_5 + 364.0*k1_3*k2 - 78.0*k1*k2_2 - 78.0*k1_2*k3 + 12.0*k2*k3
            
            // b6
            1428.0*k1_6 - 2380.0*k1_4*k2 + 840.0*k1_2*k2_2 - 35.0*k2_3 + 560.0*k1_3*k3 - 210.0*k1*k2*k3 + 7.0*k3_2
            
            // b7
            -7752.0*k1_7 + 15504.0*k1_5*k2 - 7752.0*k1_3*k2_2 + 816.0*k1*k2_3 - 3876.0*k1_4*k3 + 2448.0*k1_2*k2*k3 - 136.0*k2_2*k3 - 136.0*k1*k3_2

            // b8
            43263.0*k1_8 - 100947.0*k1_6*k2 + 65835.0*k1_4*k2_2 - 11970.0*k1_2*k2_3 + 
            285.0*k2_4 + 26334.0*k1_5*k3 - 23940.0*k1_3*k2*k3 + 3420.0*k1*k2_2*k3 + 1710.0*k1_2*k3_2 - 
            171.0*k2*k3_2

            // b9
            -246675.0*k1_9 + 657800.0*k1_7*k2 - 531300.0*k1_5*k2_2 + 141680.0*k1_3*k2_3 - 8855.0*k1*k2_4 - 177100.0*k1_6*k3 + 
            212520.0*k1_4*k2*k3 - 53130.0*k1_2*k2_2*k3 + 1540.0*k2_3*k3 - 17710.0*k1_3*k3_2 + 4620.0*k1*k2*k3_2 - 70.0*k3_3
        |]

    static let invert2 (k1 : float) (k2 : float) =
        let k1_2 = k1   * k1
        let k1_3 = k1_2 * k1
        let k1_4 = k1_3 * k1
        let k1_5 = k1_4 * k1
        let k1_6 = k1_5 * k1
        let k1_7 = k1_6 * k1
        let k1_8 = k1_7 * k1
        let k1_9 = k1_8 * k1
        let k2_2 = k2   * k2
        let k2_3 = k2_2 * k2
        let k2_4 = k2_3 * k2

        [|
            // b1
            -k1
            
            // b2
            3.0*k1_2 - k2
            
            // b3
            -12.0*k1_3 + 8.0*k1*k2
            
            // b4
            55.0*k1_4 - 55.0*k1_2*k2 + 5.0*k2_2
            
            // b5
            -273.0*k1_5 + 364.0*k1_3*k2 - 78.0*k1*k2_2
            
            // b6
            1428.0*k1_6 - 2380.0*k1_4*k2 + 840.0*k1_2*k2_2 - 35.0*k2_3
            
            // b7
            -7752.0*k1_7 + 15504.0*k1_5*k2 - 7752.0*k1_3*k2_2 + 816.0*k1*k2_3

            // b8
            43263.0*k1_8 - 100947.0*k1_6*k2 + 65835.0*k1_4*k2_2 - 11970.0*k1_2*k2_3

            // b9
            -246675.0*k1_9 + 657800.0*k1_7*k2 - 531300.0*k1_5*k2_2 + 141680.0*k1_3*k2_3 - 8855.0*k1*k2_4
        |]

    static let invert1 (k1 : float) =
        let k1_2 = k1   * k1
        let k1_3 = k1_2 * k1
        let k1_4 = k1_3 * k1
        let k1_5 = k1_4 * k1
        let k1_6 = k1_5 * k1
        let k1_7 = k1_6 * k1
        let k1_8 = k1_7 * k1
        let k1_9 = k1_8 * k1

        [|
            // b1
            -k1
            
            // b2
            3.0*k1_2
            
            // b3
            -12.0*k1_3
            
            // b4
            55.0*k1_4
            
            // b5
            -273.0*k1_5
            
            // b6
            1428.0*k1_6
            
            // b7
            -7752.0*k1_7

            // b8
            43263.0*k1_8

            // b9
            -246675.0*k1_9
        |]

    static let rec trimTrailing (i : int) (a : float[]) =
        if i = 0 then 
            [||]
        else
            if Fun.IsTiny a.[i] then
                trimTrailing (i-1) a
            else
                Array.take (i + 1) a
    
    static let identity = RadialDistortion2d([||], lazy [||])

    member x.ForwardCoefficients = forward
    member x.BackwardCoefficients = backward
    
    member x.K1 = if forward.Length > 0 then forward.[0] else 0.0
    member x.K2 = if forward.Length > 1 then forward.[1] else 0.0
    member x.K3 = if forward.Length > 2 then forward.[2] else 0.0
    member x.K4 = if forward.Length > 3 then forward.[3] else 0.0

    override x.ToString() =
        let ks = forward |> Seq.mapi (fun i k -> sprintf "k%d=%.3g" (i + 1) k) |> String.concat "; "
        sprintf "RDist { %s }" ks

    member x.Inverse = RadialDistortion2d(backward.Value, lazy forward)

    new(k1 : float, k2 : float, k3 : float, k4 : float) =
        let forward = trimTrailing 3 [| k1; k2; k3; k4 |]
        let backward = lazy (invert4 k1 k2 k3 k4)
        RadialDistortion2d(forward, backward)
    
    new(k1 : float, k2 : float, k3 : float) =
        let forward = trimTrailing 2 [| k1; k2; k3|]
        let backward = lazy (invert3 k1 k2 k3)
        RadialDistortion2d(forward, backward)
    
    new(k1 : float, k2 : float) =
        let forward = trimTrailing 1 [| k1; k2|]
        let backward = lazy (invert2 k1 k2)
        RadialDistortion2d(forward, backward)
    
    new(k1 : float) =
        let forward = trimTrailing 0 [| k1 |]
        let backward = lazy (invert1 k1)
        RadialDistortion2d(forward, backward)

    new() = 
        RadialDistortion2d([||], lazy [||])

    member x.TransformPos(p : V2d) =
        let r2 = p.LengthSquared
        let struct (_,factor) = forward |> Array.fold (fun (struct (rc,s)) ki -> struct (r2 * rc, s + ki * rc)) (struct (r2, 1.0))
        p * factor
        
    member x.InvTransformPos(p : V2d) =
        let r2 = p.LengthSquared
        let struct (_,factor) = backward.Value |> Array.fold (fun (struct (rc,s)) ki -> struct (r2 * rc, s + ki * rc)) (struct (r2, 1.0))
        p * factor

    member x.IsIdentity = forward.Length = 0

    static member Identitiy = identity
    
type RadialDistortion2s private (forward : scalar[], backward : Lazy<scalar[]>) =
    static let invert4 (k1 : scalar) (k2 : scalar) (k3 : scalar) (k4 : scalar) =
        let k1_2 = k1   * k1
        let k1_3 = k1_2 * k1
        let k1_4 = k1_3 * k1
        let k1_5 = k1_4 * k1
        let k1_6 = k1_5 * k1
        let k1_7 = k1_6 * k1
        let k1_8 = k1_7 * k1
        let k1_9 = k1_8 * k1
        let k2_2 = k2   * k2
        let k2_3 = k2_2 * k2
        let k2_4 = k2_3 * k2
        let k3_2 = k3   * k3
        let k3_3 = k3_2 * k3
        let k4_2 = k4   * k4

        [|
            // b1
            -k1
            
            // b2
            3.0*k1_2 - k2
            
            // b3
            -12.0*k1_3 + 8.0*k1*k2 - k3
            
            // b4
            55.0*k1_4 - 55.0*k1_2*k2 + 5.0*k2_2 + 10.0*k1*k3 - k4
            
            // b5
            -273.0*k1_5 + 364.0*k1_3*k2 - 78.0*k1*k2_2 - 78.0*k1_2*k3 + 12.0*k2*k3 + 12.0*k1*k4
            
            // b6
            1428.0*k1_6 - 2380.0*k1_4*k2 + 840.0*k1_2*k2_2 - 35.0*k2_3 + 560.0*k1_3*k3 - 210.0*k1*k2*k3 + 7.0*k3_2 - 105.0*k1_2*k4 + 14.0*k2*k4
            
            // b7
            -7752.0*k1_7 + 15504.0*k1_5*k2 - 7752.0*k1_3*k2_2 + 816.0*k1*k2_3 - 3876.0*k1_4*k3 + 2448.0*k1_2*k2*k3 - 136.0*k2_2*k3 - 136.0*k1*k3_2
            + 816.0*k1_3*k4 - 272.0*k1*k2*k4 + 16.0*k3*k4

            // b8
            43263.0*k1_8 - 100947.0*k1_6*k2 + 65835.0*k1_4*k2_2 - 11970.0*k1_2*k2_3 + 
            285.0*k2_4 + 26334.0*k1_5*k3 - 23940.0*k1_3*k2*k3 + 3420.0*k1*k2_2*k3 + 1710.0*k1_2*k3_2 - 
            171.0*k2*k3_2 - 5985.0*k1_4*k4 + 3420.0*k1_2*k2*k4 - 171.0*k2_2*k4 - 342.0*k1*k3*k4 + 9.0*k4_2

            // b9
            -246675.0*k1_9 + 657800.0*k1_7*k2 - 531300.0*k1_5*k2_2 + 141680.0*k1_3*k2_3 - 8855.0*k1*k2_4 - 177100.0*k1_6*k3 + 
            212520.0*k1_4*k2*k3 - 53130.0*k1_2*k2_2*k3 + 1540.0*k2_3*k3 - 17710.0*k1_3*k3_2 + 4620.0*k1*k2*k3_2 - 70.0*k3_3 + 42504.0*k1_5*k4 - 
            35420.0*k1_3*k2*k4 + 4620.0*k1*k2_2*k4 + 4620.0*k1_2*k3*k4 - 420.0*k2*k3*k4 - 210.0*k1*k4_2

        |]

    static let invert3 (k1 : scalar) (k2 : scalar) (k3 : scalar) =
        let k1_2 = k1   * k1
        let k1_3 = k1_2 * k1
        let k1_4 = k1_3 * k1
        let k1_5 = k1_4 * k1
        let k1_6 = k1_5 * k1
        let k1_7 = k1_6 * k1
        let k1_8 = k1_7 * k1
        let k1_9 = k1_8 * k1
        let k2_2 = k2   * k2
        let k2_3 = k2_2 * k2
        let k2_4 = k2_3 * k2
        let k3_2 = k3   * k3
        let k3_3 = k3_2 * k3

        [|
            // b1
            -k1
            
            // b2
            3.0*k1_2 - k2
            
            // b3
            -12.0*k1_3 + 8.0*k1*k2 - k3
            
            // b4
            55.0*k1_4 - 55.0*k1_2*k2 + 5.0*k2_2 + 10.0*k1*k3 
            
            // b5
            -273.0*k1_5 + 364.0*k1_3*k2 - 78.0*k1*k2_2 - 78.0*k1_2*k3 + 12.0*k2*k3
            
            // b6
            1428.0*k1_6 - 2380.0*k1_4*k2 + 840.0*k1_2*k2_2 - 35.0*k2_3 + 560.0*k1_3*k3 - 210.0*k1*k2*k3 + 7.0*k3_2
            
            // b7
            -7752.0*k1_7 + 15504.0*k1_5*k2 - 7752.0*k1_3*k2_2 + 816.0*k1*k2_3 - 3876.0*k1_4*k3 + 2448.0*k1_2*k2*k3 - 136.0*k2_2*k3 - 136.0*k1*k3_2

            // b8
            43263.0*k1_8 - 100947.0*k1_6*k2 + 65835.0*k1_4*k2_2 - 11970.0*k1_2*k2_3 + 
            285.0*k2_4 + 26334.0*k1_5*k3 - 23940.0*k1_3*k2*k3 + 3420.0*k1*k2_2*k3 + 1710.0*k1_2*k3_2 - 
            171.0*k2*k3_2

            // b9
            -246675.0*k1_9 + 657800.0*k1_7*k2 - 531300.0*k1_5*k2_2 + 141680.0*k1_3*k2_3 - 8855.0*k1*k2_4 - 177100.0*k1_6*k3 + 
            212520.0*k1_4*k2*k3 - 53130.0*k1_2*k2_2*k3 + 1540.0*k2_3*k3 - 17710.0*k1_3*k3_2 + 4620.0*k1*k2*k3_2 - 70.0*k3_3
        |]

    static let invert2 (k1 : scalar) (k2 : scalar) =
        let k1_2 = k1   * k1
        let k1_3 = k1_2 * k1
        let k1_4 = k1_3 * k1
        let k1_5 = k1_4 * k1
        let k1_6 = k1_5 * k1
        let k1_7 = k1_6 * k1
        let k1_8 = k1_7 * k1
        let k1_9 = k1_8 * k1
        let k2_2 = k2   * k2
        let k2_3 = k2_2 * k2
        let k2_4 = k2_3 * k2

        [|
            // b1
            -k1
            
            // b2
            3.0*k1_2 - k2
            
            // b3
            -12.0*k1_3 + 8.0*k1*k2
            
            // b4
            55.0*k1_4 - 55.0*k1_2*k2 + 5.0*k2_2
            
            // b5
            -273.0*k1_5 + 364.0*k1_3*k2 - 78.0*k1*k2_2
            
            // b6
            1428.0*k1_6 - 2380.0*k1_4*k2 + 840.0*k1_2*k2_2 - 35.0*k2_3
            
            // b7
            -7752.0*k1_7 + 15504.0*k1_5*k2 - 7752.0*k1_3*k2_2 + 816.0*k1*k2_3

            // b8
            43263.0*k1_8 - 100947.0*k1_6*k2 + 65835.0*k1_4*k2_2 - 11970.0*k1_2*k2_3

            // b9
            -246675.0*k1_9 + 657800.0*k1_7*k2 - 531300.0*k1_5*k2_2 + 141680.0*k1_3*k2_3 - 8855.0*k1*k2_4
        |]

    static let invert1 (k1 : scalar) =
        let k1_2 = k1   * k1
        let k1_3 = k1_2 * k1
        let k1_4 = k1_3 * k1
        let k1_5 = k1_4 * k1
        let k1_6 = k1_5 * k1
        let k1_7 = k1_6 * k1
        let k1_8 = k1_7 * k1
        let k1_9 = k1_8 * k1

        [|
            // b1
            -k1
            
            // b2
            3.0*k1_2
            
            // b3
            -12.0*k1_3
            
            // b4
            55.0*k1_4
            
            // b5
            -273.0*k1_5
            
            // b6
            1428.0*k1_6
            
            // b7
            -7752.0*k1_7

            // b8
            43263.0*k1_8

            // b9
            -246675.0*k1_9
        |]
        
    static let identity = RadialDistortion2s([||], lazy [||])
    
    member x.ForwardCoefficients = forward
    member x.BackwardCoefficients = backward


    member x.Inverse = RadialDistortion2s(backward.Value, lazy forward)
    
    new(k1 : scalar, k2 : scalar, k3 : scalar, k4 : scalar) =
        let forward = [| k1; k2; k3; k4 |]
        let backward = lazy (invert4 k1 k2 k3 k4)
        RadialDistortion2s(forward, backward)
    
    new(k1 : scalar, k2 : scalar, k3 : scalar) =
        let forward = [| k1; k2; k3|]
        let backward = lazy (invert3 k1 k2 k3)
        RadialDistortion2s(forward, backward)
    
    new(k1 : scalar, k2 : scalar) =
        let forward = [| k1; k2|]
        let backward = lazy (invert2 k1 k2)
        RadialDistortion2s(forward, backward)
    
    new(k1 : scalar) =
        let forward = [| k1 |]
        let backward = lazy (invert1 k1)
        RadialDistortion2s(forward, backward)

    new() = 
        RadialDistortion2s([||], lazy [||])

    member x.TransformPos(p : V2s) =
        let r2 = p.LengthSquared
        let struct (_,factor) = forward |> Array.fold (fun (struct (rc,s)) ki -> struct (r2 * rc, s + ki * rc)) (struct (r2, scalar.One))
        p * factor
        
    member x.InvTransformPos(p : V2s) =
        let r2 = p.LengthSquared
        let struct (_,factor) = backward.Value |> Array.fold (fun (struct (rc,s)) ki -> struct (r2 * rc, s + ki * rc)) (struct (r2, scalar.One))
        p * factor

    member x.TransformPos(p : V2d) =
        let r2 = p.LengthSquared
        let struct (_,factor) = forward |> Array.fold (fun (struct (rc,s)) ki -> struct (r2 * rc, s + ki * rc)) (struct (r2, scalar.One))
        V2s p * factor
        
    member x.InvTransformPos(p : V2d) =
        let r2 = p.LengthSquared
        let struct (_,factor) = backward.Value |> Array.fold (fun (struct (rc,s)) ki -> struct (r2 * rc, s + ki * rc)) (struct (r2, scalar.One))
        V2s p * factor

    static member Identitiy = identity

[<AutoOpen>]
module ``RadialDistortion Extensions`` =
    
    [<Struct>]
    type private RadialDistortionAdapter(k1 : float, k2 : float, k3 : float, k4 : float) =
        member x.K1 = k1
        member x.K2 = k2
        member x.K3 = k3
        member x.K4 = k4
        member x.Forward = [| k1; k2; k3; k4 |]

    let private pickle (d : RadialDistortion2d) = RadialDistortionAdapter(d.K1, d.K2, d.K3, d.K4)
    let private unpickle (d : RadialDistortionAdapter) = RadialDistortion2d(d.K1, d.K2, d.K3, d.K4)
        
    let private read (offset : int) (d : RadialDistortionAdapter) =
        if offset < 0 then
            RadialDistortion2s(
                scalar d.K1,
                scalar d.K2,
                scalar d.K3,
                scalar d.K4
            )
        else
            RadialDistortion2s(
                read (offset + 0) d.K1,
                read (offset + 1) d.K2,
                read (offset + 2) d.K3,
                read (offset + 3) d.K4
            )

    type Problem with
        member x.AddParameterBlock(arr : RadialDistortion2d[]) = x.AddParameterBlock(arr, pickle, unpickle, read)


type Projection3d =
    {
        focalLength         : float
        aspect              : float
        principalPoint      : V2d
        distortion          : RadialDistortion2d
    }
  
    static member Identity =
        {
            aspect = 1.0
            focalLength = 1.0
            principalPoint = V2d.Zero
            distortion = RadialDistortion2d.Identitiy
        }

    member x.Project(p : V3d) =
        let z = p.Z
        let pu = (x.focalLength * p.XY * V2d(1.0, x.aspect) + x.principalPoint * z)
        if pu.X >= -z && pu.X <= z && pu.Y >= -z && pu.Y <= z && z > 0.0 then
            let pu = pu / z
            let pd = 
                if x.distortion.IsIdentity then pu
                else x.distortion.InvTransformPos ((pu / z) - x.principalPoint) + x.principalPoint
            if pd.X >= -1.0 && pd.Y >= -1.0 && pd.X <= 1.0 && pd.Y <= 1.0 then
                Some pd
            else
                None
        else
            None

    member x.ProjectUnsafe(p : V3d) =
        let proj = (x.focalLength * p.XY * V2d(1.0, x.aspect)) / p.Z
        if x.distortion.IsIdentity then
            proj + x.principalPoint
        else 
            x.distortion.InvTransformPos proj + x.principalPoint

    member x.Unproject(p : V2d) =
        let proj = (p - x.principalPoint) / (x.focalLength * V2d(1.0, x.aspect))
        V3d(x.distortion.TransformPos proj, 1.0)

    member x.ToTrafo2d() = 
        Trafo2d(
            M33d(
                x.focalLength,              0.0,                                x.principalPoint.X,
                0.0,                        x.focalLength * x.aspect,           x.principalPoint.Y,
                0.0,                        0.0,                                1.0
            ),
            M33d(
                1.0/x.focalLength,      0.0,                                    -x.principalPoint.X / x.focalLength,
                0.0,                    1.0 / (x.aspect * x.focalLength),       -x.principalPoint.Y / (x.focalLength * x.aspect),
                0.0,                    0.0,                                     1.0
            )
        )


module Projection3d =

    let identity =
        {
            aspect = 1.0
            focalLength = 1.0
            principalPoint = V2d.Zero
            distortion = RadialDistortion2d.Identitiy
        }

    let inline project (p : V3d) (proj : Projection3d) = proj.Project p
    let inline projectUnsafe (p : V3d) (proj : Projection3d) = proj.ProjectUnsafe p
    let inline unproject (p : V2d) (proj : Projection3d) = proj.Unproject p
    let inline toTrafo (proj : Projection3d) = proj.ToTrafo2d()

type Projection3s internal(sqrtFocal : scalar, sqrtaspect : scalar, pp : V2s, dist : RadialDistortion2s) =
    let focal = sqrtFocal * sqrtFocal
    let a = sqrtaspect * sqrtaspect

    member x.focalLength = focal
    member x.aspect = a
    member x.principalPoint = pp
    member x.distortion = dist

    static member Identity = Projection3s(scalar.One, scalar.One, V2s.Zero, RadialDistortion2s.Identitiy)
    
    member x.ProjectUnsafe(p : V3s) =
        let proj = (x.focalLength * p.XY * V2s(scalar.One, x.aspect)) / p.Z
        x.distortion.InvTransformPos proj + x.principalPoint

    member x.Unproject(p : V2s) =
        let proj = (p - x.principalPoint) / (x.focalLength * V2s(scalar.One, x.aspect))
        let pu = x.distortion.TransformPos proj
        V3s(pu.X, pu.Y, scalar.One)

    member x.ToM33s() = 
        let z = scalar.Zero
        let o = scalar.One
        M33s(
            x.focalLength,              z,                                  x.principalPoint.X,
            z,                          x.focalLength * x.aspect,           x.principalPoint.Y,
            z,                          z,                                  o
        )
        
[<AutoOpen>]
module ``Projection Extensions`` =
    
    [<Struct>]
    type private ProjectionAdapter(dist : V4d, sqrtfocal : float, sqrtaspect : float, principalPoint : V2d) =
        member x.Dist = dist
        member x.SqrtFocal = sqrtfocal
        member x.SqrtAspect = sqrtaspect
        member x.PrincipalPoint = principalPoint

    let private pickle (d : Projection3d) =
        ProjectionAdapter(
            V4d(d.distortion.K1, d.distortion.K2, d.distortion.K3, d.distortion.K4),
            sqrt d.focalLength,
            sqrt d.aspect,
            d.principalPoint
        )

    let private unpickle (d : ProjectionAdapter) = 
        {
            Projection3d.focalLength = d.SqrtFocal * d.SqrtFocal
            aspect = d.SqrtAspect * d.SqrtAspect
            principalPoint = d.PrincipalPoint
            distortion = RadialDistortion2d(d.Dist.X, d.Dist.Y, d.Dist.Z, d.Dist.W)
        }

    let private read (offset : int) (d : ProjectionAdapter) =
        if offset < 0 then
            Projection3s(
                scalar d.SqrtFocal,
                scalar d.SqrtAspect,
                V2s d.PrincipalPoint,
                RadialDistortion2s(scalar d.Dist.X, scalar d.Dist.Y, scalar d.Dist.Z, scalar d.Dist.W)
            )

        else
            let dist = 
                RadialDistortion2s(
                    read (offset + 0) d.Dist.X,
                    read (offset + 1) d.Dist.Y,
                    read (offset + 2) d.Dist.Z,
                    read (offset + 3) d.Dist.W
                )
            let sqrtf = read (offset + 4) d.SqrtFocal
            let sqrta = read (offset + 5) d.SqrtAspect
            let pp = read (offset + 6) d.PrincipalPoint
            
            Projection3s(sqrtf, sqrta, pp, dist)

    type Problem with
        member x.AddParameterBlock(arr : Projection3d[]) = x.AddParameterBlock(arr, pickle, unpickle, read)
