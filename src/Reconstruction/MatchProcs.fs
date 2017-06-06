namespace Aardvark.Reconstruction

open System.Collections.Generic
open OpenCvSharp
open OpenCvSharp.XFeatures2D
open Microsoft.FSharp.NativeInterop
open CeresSharp

open Aardvark.Base

module MatchProcs =
    
    module Algo =

        let inline toArray2d arrays =
            Array2D.init (arrays |> Array.length) (arrays.[0] |> Array.length) ( fun i j -> arrays.[i].[j] )

        let inline sum (n : int) (f : int -> 'a) =
            let mutable res = LanguagePrimitives.GenericZero
            for i in 0 .. n - 1 do
                res <- res + f i
            res
        
        let m2v (m : Match2d) = V4d(m.Pos.X, m.Pos.Y, m.Vel.X, m.Vel.Y)

        let bruteforceMatch (t : float) (l : Feature[]) (r : Feature[]) =
            use matcher = new BFMatcher(normType = NormTypes.Hamming)

            use lMat = new Mat(l.Length, l.[0].descriptor.Dimension, MatType.CV_8UC1)
            for i in 0 .. l.Length - 1 do
                let arr = l.[i].descriptor.RawData |> Array.map (fun v -> uint8 (v * 255.0))
                lMat.Row.[i].SetArray(0, 0, arr)
            matcher.Add [lMat]

            use rMat = new Mat(r.Length, r.[0].descriptor.Dimension, MatType.CV_8UC1)
            for i in 0 .. r.Length - 1 do
                let arr = r.[i].descriptor.RawData |> Array.map (fun v -> uint8 (v * 255.0))
                rMat.Row.[i].SetArray(0, 0, arr)

            let matches = matcher.KnnMatch(rMat, 2)

            let reallyGoodMatches =
                matches |> Seq.choose (fun m ->
                    let m0 = m.[0]
                    let m1 = m.[1]

                    if float m0.Distance < t * float m1.Distance then
                        Some (m0.TrainIdx, m0.QueryIdx)
                    else
                        None

                ) |> Seq.toArray

            reallyGoodMatches

        let likelihood (lambda : float) (sigma : float) ( ms : Match2d[] ) =    
            let N = ms.Length

            let G = 
                Array2D.init N N ( fun i j -> 
                    exp (- (ms.[i] - ms.[j]).LengthSquared / sigma ) 
                )

            let M = 
                Array2D.init N N (fun k i ->
                    sum N (fun j -> G.[j,k] * G.[i,j]) + lambda * (2.0 * G.[i,k])
                )

            let r = 
                Array.init N (fun i ->
                    sum N (fun j -> G.[i,j])
                )

            let perm = M.LuFactorize()
            let w = M.LuSolve(perm, r)
        
            let f m =   
                sum N (fun i ->
                    w.[i] * exp (- (m - ms.[i]).LengthSquared / sigma )
                )
            f
            
        let q_dim (lambda : float) (sigma : float) (ms : V4d[]) (qdach : V4d -> float) =
            let N = ms.Length

            let x i = ms.[i].X
            let y i = ms.[i].Y
            let vx i = ms.[i].Z
            let vy i = ms.[i].W
            let e i = qdach ms.[i]

            let G = 
                Array2D.init N N ( fun i j -> 
                    exp (- (ms.[i] - ms.[j]).LengthSquared / sigma ) 
                )

            let rv f = 
                Array.init N ( fun k ->
                    sum N ( fun i -> 
                        f k i 
                    )
                )

            let r_U = rv ( fun k i ->  ( x i * e i ) * G.[i,k] )
            let r_V = rv ( fun k i ->  ( y i * e i ) * G.[i,k] )
            let r_W = rv ( fun k i ->  ( e i ) * G.[i,k] )

            let r_H = sum N ( fun i -> ( x i * e i ) )
            let r_I = sum N ( fun i -> ( y i * e i ) )
            let r_J = sum N ( fun i -> ( e i ) )

            let r = [| r_U; r_V; r_W; [|r_H|]; [|r_I|]; [|r_J|] |] |> Array.concat


            let M =
                [|
                    for k in 0..N-1 do  //d/du
                        yield [|
                            for i in 0..N-1 do      //u
                                yield 2.0 * lambda * G.[i,k] + sum N ( fun j -> x j * x j * G.[i,j] * G.[j,k] )
                            for i in 0..N-1 do      //v
                                yield sum N ( fun j -> x j * y j * G.[i,j] * G.[j,k] )
                            for i in 0..N-1 do      //w
                                yield sum N ( fun j -> x j * G.[i,j] * G.[j,k] )

                            yield sum N ( fun i -> x i * x i * G.[i,k] )  //H
                            yield sum N ( fun i -> x i * y i * G.[i,k] )  //I
                            yield sum N ( fun i -> x i * G.[i,k] )        //J
                        |]


                    for k in 0..N-1 do  //d/dv
                        yield [|
                            for i in 0..N-1 do      //u
                                yield sum N ( fun j -> x j * y j * G.[i,j] * G.[j,k] )
                            for i in 0..N-1 do      //v
                                yield 2.0 * lambda * G.[i,k] + sum N ( fun j -> y j * y j * G.[i,j] * G.[j,k] )
                            for i in 0..N-1 do      //w
                                yield sum N ( fun j -> y j * G.[i,j] * G.[j,k] )

                            yield sum N ( fun i -> y i * x i * G.[i,k] )  //H
                            yield sum N ( fun i -> y i * y i * G.[i,k] )  //I
                            yield sum N ( fun i -> y i * G.[i,k] )        //J
                        |]


                    for k in 0..N-1 do  //d/dw
                        yield [|
                            for i in 0..N-1 do      //u
                                yield sum N ( fun j -> x j * G.[i,j] * G.[j,k] )
                            for i in 0..N-1 do      //v
                                yield sum N ( fun j -> y j * G.[i,j] * G.[j,k] )
                            for i in 0..N-1 do      //w
                                yield 2.0 * lambda * G.[i,k] + sum N ( fun j -> G.[i,j] * G.[j,k] )

                            yield sum N ( fun i -> x i * G.[i,k] )        //H
                            yield sum N ( fun i -> y i * G.[i,k] )        //I
                            yield sum N ( fun i -> G.[i,k] )              //J
                        |]


                    yield [|            //d/dH
                            for i in 0..N-1 do      //u
                                yield sum N ( fun j -> x j * x j * G.[i,j] )
                            for i in 0..N-1 do      //v
                                yield sum N ( fun j -> x j * y j * G.[i,j] )
                            for i in 0..N-1 do      //w
                                yield sum N ( fun j -> x j * G.[i,j] )

                            yield sum N ( fun i -> x i * x i )        //H
                            yield sum N ( fun i -> x i * y i )        //I
                            yield sum N ( fun i -> x i )              //J
                    |]


                    yield [|            //d/dI
                            for i in 0..N-1 do      //u
                                yield sum N ( fun j -> y j * x j * G.[i,j] )
                            for i in 0..N-1 do      //v
                                yield sum N ( fun j -> y j * y j * G.[i,j] )
                            for i in 0..N-1 do      //w
                                yield sum N ( fun j -> y j * G.[i,j] )

                            yield sum N ( fun i -> y i * x i )        //H
                            yield sum N ( fun i -> y i * y i )        //I
                            yield sum N ( fun i -> y i )              //J
                    |]


                    yield [|            //d/dJ
                            for i in 0..N-1 do      //u
                                yield sum N ( fun j -> x j * G.[i,j] )
                            for i in 0..N-1 do      //v
                                yield sum N ( fun j -> y j * G.[i,j] )
                            for i in 0..N-1 do      //w
                                yield sum N ( fun j -> G.[i,j] )

                            yield sum N ( fun i -> x i )        //H
                            yield sum N ( fun i -> y i )        //I
                            yield float N                       //J
                    |]
                |] |> toArray2d

            let perm =  M.LuFactorize()
            let w =     M.LuSolve(perm, r)
        
            let U = [| for i in   0 ..   N-1 do yield w.[i] |]
            let V = [| for i in   N .. 2*N-1 do yield w.[i] |]
            let W = [| for i in 2*N .. 3*N-1 do yield w.[i] |]
            let H = w.[3*N]
            let I = w.[3*N+1]
            let J = w.[3*N+2]

            let f_U (m:V4d) = H + sum N ( fun j -> U.[j] * exp (- (m - ms.[j]).LengthSquared / sigma ) ) 
            let f_V (m:V4d) = I + sum N ( fun j -> V.[j] * exp (- (m - ms.[j]).LengthSquared / sigma ) ) 
            let f_W (m:V4d) = J + sum N ( fun j -> W.[j] * exp (- (m - ms.[j]).LengthSquared / sigma ) ) 

            let q (m:V4d) = (f_U m) * m.X + (f_V m) * m.Y + (f_W m)
        
            let psi =
                sum N ( fun i ->
                    sum N ( fun j ->
                        U.[i] * G.[i,j] * U.[j] + V.[i] * G.[i,j] * V.[j] + W.[i] * G.[i,j] * W.[j]
                    )
                )
        
            let E_dim = sum N ( fun j -> 0.5 * ( e j - q(ms.[j]) ) ** 2.0 ) + lambda * psi

            let e (v : V4d) = 0.5 * ( qdach v - q v ) ** 2.0  + lambda * psi

            q, E_dim, e
    
        let affineDistance (lambda : float) (sigma : float) ( ms : Match2d[] ) =
            let ps = ms |> Array.map m2v
            let N = ps.Length

            let (qx, Ex, ex) = q_dim lambda sigma ps ( fun v -> v.X + v.Z )
            let (qy, Ey, ey) = q_dim lambda sigma ps ( fun v -> v.Y + v.W )

            let d (m : Match2d) = let p = m2v m in V2d((qx p - ( p.X + p.Z )), (qy p - ( p.Y + p.W )))

            d,Ex,Ey,ex,ey
            
    type FeatureMatch = FeatureNode * FeatureNode * Match2d

    module FeatureMatch = 
        let toFeatureNodePair (fm : FeatureMatch) = let (l,r,_) = fm in (l,r)
        
    let bruteforce (t : float) (l : list<FeatureNode>) (r : list<FeatureNode>) : list<FeatureMatch> = 
        let la = l |> List.toArray
        let l = la |> Array.map ( fun fn -> fn.feature )
        
        let ra = r |> List.toArray
        let r = ra |> Array.map ( fun fn -> fn.feature )

        let pairs = Algo.bruteforceMatch t l r

        pairs |> Array.map ( fun (lidx,ridx) -> la.[lidx], ra.[ridx], Match2d.ofFeatures( la.[lidx], ra.[ridx] ) )
              |> Array.toList

    let probability (lambda : float) (sigma : float) (probability : float) (featureMatches : list<FeatureMatch>) : list<FeatureMatch> =
        
        let ms = featureMatches |> List.map (fun (_,_,m2) -> m2 ) |> List.toArray

        let fn = Algo.likelihood lambda sigma ms

        featureMatches |> List.filter (fun (_,_,m) -> 
            fn m >= probability
        )

    let affine lambda sigma thresholdInNdc (featureMatches : list<FeatureMatch>) : list<FeatureMatch> =
        
        let ms = featureMatches |> List.map (fun (_,_,m2) -> m2 ) |> List.toArray

        let (qn,_,_,_,_) = Algo.affineDistance lambda sigma ms

        featureMatches |> List.filter ( fun (_,_,m) ->
            (qn m).LengthSquared < thresholdInNdc
        )

