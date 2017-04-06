namespace Aardvark.Ceres

open System
open Aardvark.Base
open Aardvark.Base.AMD64

[<AutoOpen>]
module Oida =
    let offsetForFocalLength = 0.5

type Camera3d =
    struct
        val mutable public Position         : V3d
        val mutable public AngleAxis        : V3d

        member x.ProjectWithDepth(p : V3d) =
            let view = AngleAxis.RotatePoint(x.AngleAxis, p - x.Position)
            let ndc = view.XY / view.Z

            ndc, view.Z

        member x.Project(p : V3d) =
            let view = AngleAxis.RotatePoint(x.AngleAxis, p - x.Position)
            let ndc = view.XY / view.Z

            ndc

        member x.ViewProjTrafo (far : float) =
            let frustum = { left = -1.0; right = 1.0; top = 1.0; bottom = -1.0; near = 1.0; far = far }

            Trafo3d.Translation(-x.Position) *
            AngleAxis.Trafo(x.AngleAxis) *
            Frustum.projTrafo frustum

        member x.Transformed (t : Trafo3d) =
            let up = AngleAxis.RotatePoint(-x.AngleAxis, V3d.OIO)
            let fw = AngleAxis.RotatePoint(-x.AngleAxis, -V3d.OOI)
            let p =  x.Position         |> t.Forward.TransformPosProj
            let pu = x.Position + up    |> t.Forward.TransformPosProj
            let pf = x.Position + fw    |> t.Forward.TransformPosProj

            let u = pu - p
            let s = Vec.length u

            let mutable res = Camera3d.LookAt(p, pf, (pf - p).Length / s, u / s)
            res

        member x.Unproject (pt : V2d) (depth : float) =
            let ndc = V2d(pt.X, pt.Y)
            let dir = AngleAxis.RotatePoint(-x.AngleAxis, V3d(ndc, 1.0) * depth)
            x.Position + dir

        member x.GetRay (pt : V2d) =
            let ndc = V3d(pt.X, pt.Y, 1.0)
            let dir = AngleAxis.RotatePoint(-x.AngleAxis, ndc) |> Vec.normalize
            Ray3d(x.Position, dir)

        static member LookAt(eye : V3d, center : V3d, f : float, sky : V3d) : Camera3d =
            let forward = Vec.normalize (center - eye)
            let left = Vec.cross sky forward |> Vec.normalize
            let up = Vec.cross forward left |> Vec.normalize

            let rot = M44d.FromBasis(-left, up, -forward, V3d.Zero).UpperLeftM33() |> Rot3d.FromM33d
            let mutable axis = V3d.Zero
            let mutable angle = 0.0
            rot.ToAxisAngle(&axis, &angle)
            let aa = axis * -angle
            let res = Camera3d(eye, aa)

            let test = res.Project center
            res

        static member Delta(src : Camera3d, dst : Camera3d) =
            let src = src.ViewProjTrafo 100.0
            let dst = dst.ViewProjTrafo 100.0
            src * dst.Inverse

        new(pos, angleAxis) = 
            { Position = pos; AngleAxis = angleAxis}
    end

type Camera3s(pos : V3s, aa : V3s) =
    member x.Position = pos
    member x.AngleAxis = aa

    member x.Project(p : V3s) =
        let view = AngleAxis.RotatePoint(x.AngleAxis, p - x.Position)
        let ndc = view.XY / view.Z

        ndc

    member x.ProjectWithDepth(p : V3s) =
        let view = AngleAxis.RotatePoint(x.AngleAxis, p - x.Position)
        let ndc = view.XY / view.Z

        ndc, view.Z

    static member Read(offset : int, v : Camera3d) =
        let p   = V3s(scalar.Variable(offset + 0, v.Position.X), scalar.Variable(offset + 1, v.Position.Y), scalar.Variable(offset + 2, v.Position.Z))
        let aa  = V3s(scalar.Variable(offset + 3, v.AngleAxis.X), scalar.Variable(offset + 4, v.AngleAxis.Y), scalar.Variable(offset + 5, v.AngleAxis.Z))
        Camera3s(p, aa)

    static member Value(cam : Camera3s) =
        Camera3d(cam.Position.Value, cam.AngleAxis.Value)

module V3s =
    
    let getProjectedBy (cam : Camera3d) (p : V3s) =   
        let view = AngleAxis.RotatePoint(cam.AngleAxis, p - cam.Position)
        let ndc = view.XY / view.Z

        ndc

    let getProjectedByWithDepth (cam : Camera3d) (p : V3s) =   
        let view = AngleAxis.RotatePoint(cam.AngleAxis, p - cam.Position)
        let ndc = view.XY / view.Z

        ndc, view.Z

[<Struct>]
type Match2d(pos : V2d, vel : V2d, o : V4d, li : int, ri : int) =
    
    member x.Left = li
    member x.Right = ri

    member x.LengthSquared =
        pos.LengthSquared + vel.LengthSquared + o.LengthSquared

    member x.Length = 
        sqrt x.LengthSquared

    member x.Pos = pos
    member x.Vel = vel
    member x.O = o

    static member (-)(l : Match2d,r : Match2d) =
        Match2d(l.Pos-r.Pos, l.Vel-r.Vel, l.O-r.O, -1, -1)

    static member Dot(l : Match2d,r : Match2d) =
        Vec.dot l.Pos r.Pos + Vec.dot l.Vel r.Vel + Vec.dot l.O r.O
        
type SolverCamera =
    {
        cam : Camera3d
        isFixed : bool
    }

type SolverPoint =  
    {
        point : V3d
        isFixed : bool
    }

module MatchProblem =       
    open CeresSharp

    let inline sum (n : int) (f : int -> 'a) =
        let mutable res = LanguagePrimitives.GenericZero
        for i in 0 .. n - 1 do
            res <- res + f i
        res

    let m2v (m : Match2d) = V4d(m.Pos.X, m.Pos.Y, m.Vel.X, m.Vel.Y)

    let pred_d ( d : int -> float ) (sigma : float)(ps : V2d[]) =
        
        let N = ps.Length

        let px i = ps.[i].X
        let py i = ps.[i].Y

        let e' i (p : V2d) = exp ( -(1.0/sigma) * ((p.X - px i) ** 2.0 + (p.Y - py i) ** 2.0))

        let e i j = e' i ps.[j]

        let r = 
            Array.init N ( fun j ->
                d j * d j * e j j
            )

        let M =
            let arrays = 
                [|
                    for j in 0..N-1 do  //dwj
                        yield [|
                            for i in 0..N-1 do      //wi
                                yield d i * d j * e i j * e j j
                        |]
                |]
            
            Array2D.init (arrays |> Array.length) (arrays.[0] |> Array.length) ( fun i j -> arrays.[i].[j] )
       
        let perm =  M.LuFactorize()
        let w =     M.LuSolve(perm, r)

        let d (p : V2d) = sum N ( fun i -> d i * w.[i] * e' i p )

        d

    let prediction (ds : V2d[]) (ps : V2d[]) (sigma : float) =
        
        let dx = pred_d ( fun i -> ds.[i].X ) sigma ps
        let dy = pred_d ( fun i -> ds.[i].Y ) sigma ps

        let d p = V2d(dx p, dy p)

        d

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
            let arrays = 
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
                |]

            Array2D.init (arrays |> Array.length) (arrays.[0] |> Array.length) ( fun i j -> arrays.[i].[j] )

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
        printfn "!! E=%A " E_dim

        let e (v : V4d) = 0.5 * ( qdach v - q v ) ** 2.0  + lambda * psi

        q, E_dim, e
    
    let affineDistance (lambda : float) (sigma : float) ( ms : Match2d[] ) =
        let ps = ms |> Array.map m2v
        let N = ps.Length

        let (qx, Ex, ex) = q_dim lambda sigma ps ( fun v -> v.X + v.Z )
        let (qy, Ey, ey) = q_dim lambda sigma ps ( fun v -> v.Y + v.W )

        let d (m : Match2d) = let p = m2v m in V2d((qx p - ( p.X + p.Z )), (qy p - ( p.Y + p.W )))

        d,Ex,Ey,ex,ey

    let likelihood (lambda : float) (sigma : float) ( ms : Match2d[] ) =    
        let N = ms.Length

        let G = 
            Array2D.init N N ( fun i j -> 
                exp (- (ms.[i] - ms.[j]).LengthSquared / sigma ) 
            )


        // d/duk E(H,I,J,u,v,w) = sumi( [ xi + x~i -Hxi -xi*sumj(uj*Gij) -yi*sumj(vj*Gij) -J -sumj(wj*Gij) ] * [-xi*Gik] + 2lGik*uk )

        // Mik = sumj [Gjk * Gji] + l * (Gki + Gik)
        // ri = sumj [ Gji ]

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


    let private random = RandomSystem()

    let inline sum' (z : 'a) (n : int) (f : int -> 'a) =
        let mutable res = z
        for i in 0 .. n - 1 do
            res <- res + f i
        res

    let o (rotation : float) =
        V4d(0.0, sin(rotation), 0.0, cos(rotation))


    let lambda = 35.0
    let sigma = 0.2




    let lambda2 = 3.0
    let delta = 0.001
    let huber (a : scalar) =   //huber
        if abs a.Value <= delta then
            0.5 * a * a
        else
            delta * (abs a - 0.5 * delta)



    let likelihoodFuckYouOld ( ms : Match2d[] ) =
        use p = new Problem()

        let w = p.AddParameterBlock(Array.init ms.Length (ignore >> random.UniformDouble))

        let N = ms.Length
        let G =
            Array2D.init N N 
                ( fun i j -> 
                    exp (- (ms.[i] - ms.[j]).LengthSquared / sigma ) )


        p.AddCostFunction ( 1, w, fun w ->
            let f m =   
                sum N (fun i ->
                    w.[i] * exp (- (m - ms.[i]).LengthSquared / sigma )
                )

            let wGw = 
                sum N (fun i ->
                    w.[i] *
                    sum N (fun j ->
                        G.[i,j] * w.[j]
                    )
                )
//
//            [|
//                for i in 0 .. N - 1 do
//                    yield 1.0 - f ms.[i]
//
//                yield lambda * wGw
//            |]

            [| sum N (fun i -> huber(1.0 - f ms.[i])) + lambda * wGw |]
        )

        let cost = p.Solve(CeresOptions(1000, CeresSolverType.DenseSchur, false, 1.0E-3, 1.0E-2, 1.0E-2))
        let w = w.Result
        let f m =   
            sum N (fun i ->
                w.[i] * exp (- (m - ms.[i]).LengthSquared / sigma )
            )

        let E =
            let psi = sum N (fun i -> w.[i] * sum N (fun j -> G.[i,j] * w.[j]))
            lambda * psi + sum N (fun j -> 0.5 * ((1.0 - f(ms.[j])) ** 2.0))

        printfn "old E: %A" E

        f,w

    // h(w) = w . (G * w)
    // dh/dwi (w) = w . (G * dw/dwi) + dw/dwi . (G * w)

    // dw/dwi = (0,0,0,0,...., w'i = 1, .... 0)

    // dh/dwi (w) = w . (G * (0,0,0,0,...., w'i = 1, .... 0)) + (G * w) . (0,0,0,0,...., w'i = 1, .... 0)

    // r = G * w
    // r(i) = G[i][*] . w

    // dh/dwi (w) = G[*][i] . w + G[i][*] . w
    // dh/dwi (w) = (G[*][i] + G[i][*]) . w

    // dh/dwi = sumk [(Gki + Gik) * wk]


    //sum Gij * wj + sum Gji * wj
    //huber x^2/2
    //f(pj)-1


        
    //df / dwi (p) = g(p - pi) 


    // E = sum [ 0.5*(1 - f(pj))^2 ] + l*h(w)
    // dE/dwi = sumj [ (f(pj) - 1) * f'(pj) ] + l *h'(w)
    // dE/dwi = sumj [ (f(pj) - 1) * g(pj - pi)  ] + l *h'(w)

    // dE/dwi = sumj [ (sumk [wk * g(pj - pk)] - 1) * g(pj - pi) ] + l *h'(w)
    
    // dE/dwi = sumj [ sumk [wk * Gjk * Gji] - Gji  ] + l *h'(w)
    // dE/dwi = sumk [ wk * sumj [Gjk * Gji] ] - sumj [ Gji ] + l *h'(w)


     // dE/dwi = sumk [ wk * sumj [Gjk * Gji] ] - sumj [ Gji ] + l * sumk [(Gki + Gik) * wk]
     // dE/dwi = sumk [ wk * sumj [Gjk * Gji] ] + sumk [l * (Gki + Gik) * wk] - sumj [ Gji ]
     // dE/dwi = sumk [ wk * sumj [Gjk * Gji] + l * (Gki + Gik) * wk] - sumj [ Gji ]
     // dE/dwi = sumk [ wk * (sumj [Gjk * Gji] + l * (Gki + Gik))] - sumj [ Gji ]


     // sumk [ wk * (sumj [Gjk * Gji] + l * (Gki + Gik))] = sumj [ Gji ]

     // Mik = sumj [Gjk * Gji] + l * (Gki + Gik)
     // ri = sumj [ Gji ]



    let private affineComponent (ci : int) (G : float[,]) (ps : V4d[]) =
        use px = new Problem()
        let N = ps.Length


        let wx = px.AddParameterBlock(Array.init N (ignore >> random.UniformV3d))
        let Hx = px.AddParameterBlock [| V3d.Zero |]
        
        let g (v : V4d) =
            exp (-v.LengthSquared / sigma)

        let G =
            Array2D.init N N 
                ( fun i j -> g(ps.[i] - ps.[j]) )

        let G' ((i,j) : int*int) =
            

            ()

        let ws = Array.init N (ignore >> random.UniformDouble)
        let dE_d wi i =
            let sumk = 
                sum N ( fun k -> 
                    ws.[k] * sum N ( fun j ->
                        g(ps.[j] - ps.[k])
                    )
                ) 

            let lh' =
                let sumgij =
                    sum N ( fun j ->
                        G.[i,j] * wi
                    )
                let sumgji =
                    sum N ( fun j ->
                        G.[j,i] * wi
                    )

                lambda * (sumgij + sumgji)

            let sumj =
                sum N ( fun j ->
                    float (N-1) * g(ps.[j] - ps.[i])    
                )
                
            sumk + lh' + sumj


        px.AddCostFunction(N + 3, wx, Hx, fun w H ->
            let H = H.[0]
            let f (i : int) (p : V4d) = H.[i] + sum N (fun j -> w.[j].[i] * g(p - ps.[j]))
            let q (p : V4d) = 
                (f 0 p) * p.X + (f 1 p) * p.Y + (f 2 p)

            let psi = 
                sum N (fun i ->
                    w.[i] *
                    sum N (fun j ->
                        G.[i,j] * w.[j]
                    )
                )

            [|
                for j in 0 .. N - 1 do
                    let pj = ps.[j]
                    let qr = pj.[ci] + pj.[ci+2]
                    yield qr - q(pj)
                    
                yield lambda2 * psi.X
                yield lambda2 * psi.Y
                yield lambda2 * psi.Z
                //yield lambda * (psi.X + psi.Y + psi.Z)
            |]


            






            // a * w0 + b*w1 + ... + z = 0
            // 0 0 0 0 0 0 1





            //sumj[f(pj)-1 + (gij + gji) * wj] == 0

//            let E = sum N (fun j -> 
//                let pj = ps.[j]
//                let qr = pj.[ci] + pj.[ci+2]
//                huber(qr - q(pj))
//            )
//
//            [| E + lambda2 * (psi.X + psi.Y + psi.Z) |]
        )

        px.Solve(CeresOptions(1000, CeresSolverType.DenseSchur, true, 1.0E-2, 1.0E-2, 1.0E-2)) |> ignore
        
        let H = Hx.Result.[0]
        let w = wx.Result
        let f (i : int) (p : V4d) = H.[i] + sum N (fun j -> w.[j].[i] * g(p - ps.[j]))
        let q (p : V4d) = (f 0 p) * p.X + (f 1 p) * p.Y + (f 2 p)


        q


    let affine( ms : Match2d[] ) =
        
        let ps = ms |> Array.map (fun m -> V4d(m.Pos.X, m.Pos.Y, m.Vel.X, m.Vel.Y))
        let N = ms.Length
        let ms = ()

        let g (v : V4d) = exp (-v.LengthSquared / sigma)
        let G = Array2D.init N N ( fun i j -> g(ps.[i] - ps.[j]))

        let qx = affineComponent 0 G ps
        let qy = affineComponent 1 G ps

        let q (p : V4d) = V2d(qx p, qy p)
        q


type BundlerInput =
    {
        measurements : Map<int,Map<int, V2d>>
        tracks : (int*int)[][]
    }

type BundlerProblem =
    {
        input           : BundlerInput
        cameras         : Set<int>
    }

type BundlerSolution =
    {
        cost        : float
        problem     : BundlerProblem
        points      : Map<int, SolverPoint>
        cameras     : Map<int, SolverCamera>
    }

type BundlerError =
    {
        cost        : float
        max         : float
        min         : float
        average     : float
        stdev       : float
    }



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BundlerProblem =

    let inline input (p : BundlerProblem) = p.input
    let inline cameras (p : BundlerProblem) = p.cameras
  
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BundlerSolution =
    let inline problem (s : BundlerSolution) = s.problem
    let inline points (s : BundlerSolution) = s.points
    let inline cameras (s : BundlerSolution) = s.cameras
 
    let errorMetrics (s : BundlerSolution) =
        let errors =
            [|
                for pi in 0 .. s.problem.input.tracks.Length-1 do
                    let estimatedPoint = s.points.[pi]

                    let track = s.problem.input.tracks.[pi]
                    for (ci,lpi) in track do
                        
                        let cam = s.cameras.[ci]
                        
                        let observedPoint = s.problem.input.measurements.[ci].[lpi]

                        let projection = cam.cam.Project estimatedPoint.point
                        let diff = projection - observedPoint
                        yield diff, (Vec.length (0.5*diff))
            |] 

            //s.problem.cameras
            //    |> Seq.collect (fun ci ->
            //        let measurements = s.problem.input.measurements.[ci]
            //        let scam = s.cameras.[ci]
            //        let cam = scam.cam

            //        measurements 
            //            |> Map.toSeq 
            //            |> Seq.map (fun (pi, m) -> 
            //                let obs = cam.Project s.points.[pi]
            //                let v = obs - m
            //                v, Vec.length (0.5 * v) // 0.5 because [-1,1]
            //            ) 
            //        )
            //    |> Seq.toArray

        let mutable sumSq = 0.0
        let mutable sum = 0.0
        let mutable emin = Double.PositiveInfinity
        let mutable emax = Double.NegativeInfinity
        for (v,l) in errors do
            sumSq <- sumSq + v.X * v.X + v.Y * v.Y
            sum <- sum + l
            emin <- min emin l
            emax <- max emax l


        let average = sum / float errors.Length
        let variance = Array.sumBy (fun (_,e) -> (e - average) * (e - average)) errors / float (errors.Length - 1)


        {
            cost        = 0.5 * sumSq
            max         = emax
            min         = emin
            average     = average
            stdev       = sqrt variance
        }


    let private rand = RandomSystem()

    let private withCost (s : BundlerSolution) =
        let m = errorMetrics s
        { s with cost = m.cost }


    let transformed (trafo : Trafo3d) (s : BundlerSolution) =
        let fw = trafo.Forward
        { s with 
            points = s.points |> Map.map (fun _ p -> { p with point = fw.TransformPosProj p.point })
            cameras = s.cameras |> Map.map (fun _ sc -> { sc with cam = sc.cam.Transformed trafo })
        }

    let merge (l : BundlerSolution) (r : BundlerSolution) =
        if l.problem.input <> r.problem.input then failwith "cannot merge SubSolutions for different inputs"

        let overlapping = Map.intersect l.points r.points
        if overlapping.Count < 8 then failwith "cannot merge SubSolutions with less than 8 shared points"

        let lPoints, rPoints = overlapping |> Map.toSeq |> Seq.map snd |> Seq.toArray |> Array.map ( fun (lsp,rsp) -> lsp.point, rsp.point ) |> Array.unzip

        let trafo = PointCloud.trafo rPoints lPoints

        let r = transformed trafo r

        withCost {
            cost        = 0.0
            problem     = { l.problem with cameras = Set.union l.problem.cameras r.problem.cameras }
            points      = Map.union r.points l.points
            cameras     = Map.union r.cameras l.cameras
        }

    let crap : V3d[] = 
        if System.IO.File.Exists @"C:\blub\random" then
            let p = MBrace.FsPickler.FsPickler.CreateBinarySerializer()
            File.readAllBytes @"C:\blub\random" |> p.UnPickle
        else
            let rand = RandomSystem()
            let places = 2
            [|
                for i in 0..100000 do
                    yield rand.UniformV3d(Box3d.Unit.Transformed(Trafo3d.Scale 2.0).Translated(-V3d.III))
            |] |> Array.map ( fun v -> V3d(Math.Round(v.X,places),Math.Round(v.Y,places),Math.Round(v.Z,places) ) )
               |> Array.distinct

    let withFixings (fixCams : bool) (fixPoints : bool) n =
        { n with 
            points = n.points |> Map.map ( fun i p -> {p with isFixed = fixPoints} )
            cameras = n.cameras |> Map.map ( fun _ c -> {c with isFixed = fixCams} ) 
        }

    let random (p : BundlerProblem)=
        //let cameras = p.cameras |> Seq.map (fun ci -> ci, Camera3d.LookAt(rand.UniformV3dDirection() * 10.0, V3d.Zero, 1.5, V3d.OOI)) |> Map.ofSeq

        let cameras =
            let cams = p.cameras |> Seq.toArray
            [
                let dist = 20.0
                for i in 0..cams.Length-1 do
                    let ci = cams.[i]
                    match i with
                    | 0 -> 
                        yield ci, { cam = Camera3d.LookAt(V3d.IOI * dist, V3d.Zero, 1.0, V3d.OOI); isFixed = false }
                    | 1 -> 
                        yield ci, { cam = Camera3d.LookAt(V3d.OII * dist, V3d.Zero, 1.0, V3d.OOI); isFixed = false }
                    | 2 -> 
                        yield ci, { cam = Camera3d.LookAt(V3d.IIO * dist, V3d.Zero, 1.0, V3d.OOI); isFixed = false }
                    | 3 -> 
                        yield ci, { cam = Camera3d.LookAt(-V3d.III * dist, V3d.Zero, 1.0, V3d.OOI); isFixed = false }
                    | _ -> 
                        yield ci, { cam = Camera3d.LookAt(rand.UniformV3dDirection() * dist, V3d.Zero, 1.0, V3d.OOI); isFixed = false }
            ] |> Map.ofList

        let points = 
            p.input.tracks |> Array.mapi ( fun i _ -> i, { point = crap.[i]; isFixed = false } ) |> Map.ofArray

        withCost {
            cost = 0.0
            problem = p
            cameras = cameras
            points = points
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BundlerError =
    let inline cost (s : BundlerError) = s.cost
    let inline max (s : BundlerError) = s.max
    let inline min (s : BundlerError) = s.min
    let inline average (s : BundlerError) = s.average
    let inline stdev (s : BundlerError) = s.stdev

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BundlerInput =
    
    let preprocess (input : BundlerInput) =
        let counts = Dict<int, ref<int>>()

        // count how often each point is referenced
        for kvp in input.measurements do
            let m = kvp.Value
            for kvp in m do
                let r = counts.GetOrCreate(kvp.Key, fun _ -> ref 0)
                r := !r + 1

        // points that are visible from only one camera are useless
        let valid = 
            counts 
                |> Dict.toSeq 
                |> Seq.choose (fun (pi,r) -> if !r >= 2 then Some pi else None) 
                |> HashSet.ofSeq
        
        // remove invalid points from all measurements
        let measurements =
            input.measurements |> Map.map (fun _ m ->
                m |> Map.filter (fun pi _ -> valid.Contains pi)
            )

        Log.line "Reduction: (visible from 2 cams)"
        for i in 0 .. measurements.Count - 1 do
            Log.line "%A:  %A -> %A" i input.measurements.[i].Count measurements.[i].Count

        // prune them from the input
        { input with measurements = measurements }

    let toProblem (i : BundlerInput) =
        // cameras that see less than 8 points are not stable
        let cameras = 
            i.measurements |> Map.filter (fun i m -> 
                if m.Count < 8 then
                    Log.warn "Camera %A has less than 8 points (%A) and is discarded." i m.Count
                    
                    false
                    //true
                else
                    true
            )

        if cameras.Count = 0 then Log.error "No stable cameras found, solution impossible."

        let cams = cameras |> Map.toArray |> Array.map fst |> Set.ofArray 

        {
            input = i
            cameras = cams 
        }

