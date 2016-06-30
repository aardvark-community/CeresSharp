namespace Derivatives

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.NativeInterop
open System
open Aardvark.Base

#nowarn "9"


type Jacobian<'a> = Map<int, 'a>

type Jacobian =
    
    static member ZipWith (f : 'a -> 'a -> 'a) (l : Jacobian<'a>) (r : Jacobian<'a>) =
        if Map.isEmpty l then r
        elif Map.isEmpty r then l
        else
            let mutable res = l
            for kvp in r do
                match Map.tryFind kvp.Key res with
                    | Some l -> res <- Map.add kvp.Key (f l kvp.Value) res
                    | _ -> res <- Map.add kvp.Key kvp.Value res
            res

    static member Map (f : 'a -> 'b) (l : Jacobian<'a>) =
        if Map.isEmpty l then Map.empty
        else l |> Map.map (fun _ v -> f v)


    static member inline Add (l : Jacobian<'a>, r : Jacobian<'a>) =
        Jacobian.ZipWith (+) l r

    static member inline Sub (l : Jacobian<'a>, r : Jacobian<'a>) =
        Jacobian.ZipWith (+) l (Jacobian.Map (~-) r)

    static member inline Mul (l : 'b, r : Jacobian<'a>) =
        if Map.isEmpty r then Map.empty
        else Jacobian.Map (fun v -> l * v) r

    static member inline Mul (l : Jacobian<'a>, r : 'b) =
        if Map.isEmpty l then Map.empty
        else Jacobian.Map (fun v -> v * r) l

    static member inline Div (l : Jacobian<'a>, r : 'b) =
        if Map.isEmpty l then Map.empty
        else Jacobian.Map (fun v -> v / r) l

[<CustomComparison; CustomEquality>]
type scalar =
    struct
        val mutable public Value : float
        val mutable public Jacobian : Jacobian<float>

        interface IComparable with
            member x.CompareTo(o) =
                match o with
                    | :? scalar as o -> compare x.Value o.Value
                    | _ -> failwith "uncomparable"

        override x.Equals o =
            match o with
                | :? scalar as o -> o.Value = x.Value
                | _ -> false

        override x.GetHashCode() =
            x.Value.GetHashCode()

        override x.ToString() =
            let d = x.Jacobian |> Map.toSeq |> Seq.map (fun (vi, v) -> sprintf "dv/d%d = %f" vi v) |> String.concat "; "
            if d.Length > 0 then
                sprintf "{ v = %f; %s }" x.Value d
            else
                sprintf "%f" x.Value


        static member Equation(l : scalar, r : scalar) =
            Equation [l - r]

        static member Equation(l : scalar, r : float) =
            if r = 0.0 then Equation [l]
            else Equation [l - scalar r]

        static member Equation(l : float, r : scalar) =
            Equation [scalar l - r]

        static member Zero = scalar(0.0, Map.empty)
        static member One = scalar(1.0, Map.empty)

        static member (~-) (l : scalar) =
            scalar(-l.Value, Jacobian.Map (~-) l.Jacobian)


        static member (+) (l : scalar, r : scalar) =
            scalar(l.Value + r.Value, Jacobian.Add(l.Jacobian, r.Jacobian))

        static member (+) (l : scalar, r : float) =
            scalar(l.Value + r, l.Jacobian)

        static member (+) (l : float, r : scalar) =
            scalar(l + r.Value, r.Jacobian)


        static member (-) (l : scalar, r : scalar) =
            scalar(l.Value - r.Value, Jacobian.Sub(l.Jacobian, r.Jacobian))

        static member (-) (l : scalar, r : float) =
            scalar(l.Value - r, l.Jacobian)

        static member (-) (l : float, r : scalar) =
            scalar(l - r.Value, Jacobian.Map (~-) r.Jacobian)


        static member (*) (l : scalar, r : scalar) =
            scalar(l.Value * r.Value, Jacobian.Add(Jacobian.Mul(l.Jacobian, r.Value), Jacobian.Mul(l.Value, r.Jacobian)))

        static member (*) (l : scalar, r : float) =
            scalar(l.Value * r, Jacobian.Mul(l.Jacobian, r))

        static member (*) (l : float, r : scalar) =
            scalar(l * r.Value, Jacobian.Mul(l, r.Jacobian))



        static member (/) (l : scalar, r : scalar) =
            scalar(l.Value / r.Value, Jacobian.Div(Jacobian.Add(Jacobian.Mul(l.Jacobian, r.Value), Jacobian.Mul(l.Value, r.Jacobian)), r.Value*r.Value))

        static member (/) (l : scalar, r : float) =
            scalar(l.Value / r, Jacobian.Div(l.Jacobian, r))

        static member (/) (l : float, r : scalar) =
            scalar(l / r.Value, Jacobian.Div(Jacobian.Mul(l, r.Jacobian), r.Value*r.Value))

        static member Sin (v : scalar) =
            scalar(sin v.Value, Jacobian.Mul(cos v.Value, v.Jacobian))

        static member Cos (v : scalar) =
            scalar(cos v.Value, Jacobian.Mul(-sin v.Value, v.Jacobian))

        static member Sqrt (v : scalar) =
            scalar(sqrt v.Value, Jacobian.Div(v.Jacobian, -2.0 * sqrt v.Value))

        static member Abs (v : scalar) =
            if v.Value > 0.0 then v
            else -v



        new(v : float, j : Jacobian<float>) = { Value = v; Jacobian = j }

        new(v : int8) = { Value = float v; Jacobian = Map.empty }
        new(v : uint8) = { Value = float v; Jacobian = Map.empty }
        new(v : int16) = { Value = float v; Jacobian = Map.empty }
        new(v : uint16) = { Value = float v; Jacobian = Map.empty }
        new(v : int32) = { Value = float v; Jacobian = Map.empty }
        new(v : uint32) = { Value = float v; Jacobian = Map.empty }
        new(v : int64) = { Value = float v; Jacobian = Map.empty }
        new(v : uint64) = { Value = float v; Jacobian = Map.empty }
        new(v : float32) = { Value = float v; Jacobian = Map.empty }
        new(v : float) = { Value = v; Jacobian = Map.empty }
        new(v : decimal) = { Value = float v; Jacobian = Map.empty }
    end

and Equation =
    struct
        val mutable public Residuals : list<scalar>

        override x.ToString() =
            x.Residuals |> List.map string |> String.concat ";" |> sprintf "[%s]"

        new(r) = { Residuals = r }
    end


module CodeGen =
    let builder = System.Text.StringBuilder()
    let printfn fmt = Printf.kprintf (fun str -> builder.AppendLine(str) |> ignore; Console.WriteLine(str)) fmt

    let scalarTypes =
        [
            "int8"; "uint8"
            "int16"; "uint16"
            "int32"; "uint32"
            "int64"; "uint64"
            "float32"; "float"
            "decimal"
            "scalar"
        ]

    let floatTypes = ["float"; "scalar"]
    let vecFields = [|"X"; "Y"; "Z"; "W"|]
    let vecSuffixes = ["i"; "d"; "f"; "l"]

    let matrixName (rows : int) (cols : int) =
        sprintf "M%d%ds" rows cols

    let vectorName (dim : int) =
        sprintf "V%ds" dim

    let matrix (rows : int) (cols : int) =
        let name = matrixName rows cols
        let normalName = sprintf "M%d%dd" rows cols
        printfn "type %s =" name
        printfn "    struct"
        
        let coords =
            [
                for r in 0..rows-1 do
                    for c in 0..cols-1 do
                        yield (r,c)
            ]

        let argNames =
            [
                for r in 0..rows-1 do
                    for c in 0..cols-1 do
                        yield sprintf "m%d%d" r c
            ]

        let fieldNames =
            [
                for r in 0..rows-1 do
                    for c in 0..cols-1 do
                        yield sprintf "M%d%d" r c
            ]

        // define the fields
        for r in 0..rows-1 do
            for c in 0..cols-1 do
                printfn "        val mutable public M%d%d : scalar" r c

        let residuals = fieldNames |> List.map (fun n -> sprintf "l.%s - r.%s" n n) |> String.concat "; "
        printfn "        static member Equation(l : %s, r : %s) = Equation [%s]" name name residuals
        printfn "        static member Equation(l : %s, r : %s) = Equation [%s]" normalName name residuals
        printfn "        static member Equation(l : %s, r : %s) = Equation [%s]" name normalName residuals
        

        let zeros = coords |> List.map (fun (r,c) -> "0.0") |> String.concat ", "
        printfn "        static member Zero = %s(%s)" name zeros
        let identity = coords |> List.map (fun (r,c) -> if r = c then "1.0" else "0.0") |> String.concat ", "
        printfn "        static member Identity = %s(%s)" name identity
        printfn "        static member One = %s(%s)" name identity


        // elementwise operators
        let args (op) = fieldNames |> List.map (fun f -> sprintf "l.%s %s r.%s" f op f) |> String.concat ", "
        printfn "        static member (+) (l : %s, r : %s) = %s(%s)" name name name (args "+")
        printfn "        static member (-) (l : %s, r : %s) = %s(%s)" name name name (args "-")


        // scalar operators
        let args (f : string -> string) = fieldNames |> List.map f |> String.concat ", "
        for ft in floatTypes do
            printfn "        static member (*) (l : %s, r : %s) = %s(%s)" ft name name (args (sprintf "l * r.%s"))
            printfn "        static member (*) (l : %s, r : %s) = %s(%s)" name ft name (args (sprintf "l.%s * r"))
            printfn "        static member (/) (l : %s, r : %s) = %s(%s)" ft name name (args (sprintf "l / r.%s"))
            printfn "        static member (/) (l : %s, r : %s) = %s(%s)" name ft name (args (sprintf "l.%s / r"))


        // vector multiplication
        let vecType = vectorName cols
        let resType = vectorName rows

        let args = 
            [0..rows-1] |> List.map (fun r -> 
                vecFields 
                    |> Array.take cols 
                    |> Array.mapi (fun i v -> sprintf "m.M%d%d * v.%s" r i v)
                    |> String.concat " + "
            ) |> String.concat ", "

        printfn "        static member (*) (m : %s, v : %s) = %s(%s)" name vecType resType args


        // matrix multiplication
        let matType = matrixName cols cols
        let resType = matrixName rows cols
        let args =
            coords |> List.map (fun (r,c) ->
                [0..cols-1] |> List.map (fun i -> sprintf "l.M%d%d * r.M%d%d" r i i c) |> String.concat " + "
            )
            |> String.concat ", "
        printfn "        static member (*) (l : %s, r : %s) = %s(%s)" name matType resType args
        printfn "        static member (*) (l : %s, r : %s) = %s(%s)" name normalName resType args
        printfn "        static member (*) (l : %s, r : %s) = %s(%s)" normalName name resType args
                

        printfn "        member x.Transposed = %s(%s)" name (coords |> List.map (fun (r,c) -> sprintf "x.M%d%d" c r) |> String.concat ", ")
        
        if rows = cols && rows = 3 then
            printfn "        static member Rotation(angle : scalar) = "
            printfn "            let ca = cos angle"
            printfn "            let sa = sin angle"
            printfn "            M33s(ca, sa, scalar 0.0, -sa, ca, scalar 0.0, scalar 0.0, scalar 0.0, scalar 1.0)"

            printfn "        static member Translation(x : scalar, y : scalar) = M33s(scalar 1.0, scalar 0.0, x, scalar 0.0, scalar 1.0, y, scalar 0.0, scalar 0.0, scalar 1.0)"
            printfn "        static member Translation(v : V2d) = M33s.Translation(scalar v.X, scalar v.Y)"
            printfn "        static member Translation(v : V2s) = M33s.Translation(v.X, v.Y)"
            printfn "        static member Scale(x : scalar, y : scalar) = M33s(x, scalar 0.0, scalar 0.0, scalar 0.0, y, scalar 0.0, scalar 0.0, scalar 0.0, scalar 1.0)"
            printfn "        static member Scale(f : scalar) = M33s.Scale(f,f,f)"
            printfn "        static member Scale(f : V2s) = M33s.Scale(f.X,f.Y)"

        if rows = cols && rows = 4 then
            printfn "        static member Rotation(axis : V3s, angle : scalar) = "
            printfn "            let axis = axis.Normalized"
            printfn "            let halfAngle = angle / 2.0"
            printfn "            let sh = sin halfAngle"
            printfn "            let r = V4s(axis.X * sh, axis.Y * sh, axis.Z * sh, cos halfAngle)"
            printfn "            let xx = r.X * r.X"
            printfn "            let yy = r.Y * r.Y"
            printfn "            let zz = r.Z * r.Z"
            printfn "            let xy = r.X * r.Y"
            printfn "            let xz = r.X * r.Z"
            printfn "            let yz = r.Y * r.Z"
            printfn "            let xw = r.X * r.W"
            printfn "            let yw = r.Y * r.W"
            printfn "            let zw = r.Z * r.W"
            printfn "            M44s(1.0 - 2.0 * (yy + zz), 2.0 * (xy - zw), 2.0 * (xz + yw), scalar 0.0, 2.0 * (xy + zw), 1.0 - 2.0 * (zz + xx), 2.0 * (yz - xw), scalar 0.0, 2.0 * (xz - yw), 2.0 * (yz + xw), 1.0 - 2.0 * (yy + xx), scalar 0.0, scalar 0.0, scalar 0.0, scalar 0.0, scalar 1.0 )"


            printfn "        static member Translation(x : scalar, y : scalar, z : scalar) = M44s(scalar 1.0, scalar 0.0, scalar 0.0, x, scalar 0.0, scalar 1.0, scalar 0.0, y, scalar 0.0, scalar 0.0, scalar 1.0, z, scalar 0.0, scalar 0.0, scalar 0.0, scalar 1.0)"
            printfn "        static member Translation(v : V3d) = M44s.Translation(scalar v.X, scalar v.Y, scalar v.Z)"
            printfn "        static member Translation(v : V3s) = M44s.Translation(v.X, v.Y, v.Z)"
            printfn "        static member Scale(x : scalar, y : scalar, z : scalar) = M44s(x, scalar 0.0, scalar 0.0, scalar 0.0, scalar 0.0, y, scalar 0.0, scalar 0.0, scalar 0.0, scalar 0.0, z, scalar 0.0, scalar 0.0, scalar 0.0, scalar 0.0, scalar 1.0)"
            printfn "        static member Scale(f : scalar) = M44s.Scale(f,f,f)"
            printfn "        static member Scale(f : V3s) = M44s.Scale(f.X,f.Y,f.Z)"

        if rows = cols && rows >= 3 then
            let vec = vectorName (rows - 1)
            let swizzle = vecFields |> Array.toList |> List.take (rows - 1) |> String.concat ""
            let fullVec = sprintf "%s(%s, scalar 0.0)" (vectorName rows) (vecFields |> Array.toList |> List.take (rows - 1) |> List.map (sprintf "v.%s") |> String.concat ", ")
            printfn "        member x.TransformDir(v : %s) = (x * %s).%s"  vec fullVec swizzle


            let fullVec = sprintf "%s(%s, scalar 1.0)" (vectorName rows) (vecFields |> Array.toList |> List.take (rows - 1) |> List.map (sprintf "v.%s") |> String.concat ", ")
            printfn "        member x.TransformPos(v : %s) = (x * %s).%s"  vec fullVec swizzle



        // define constructors
        for st in scalarTypes do
            let args = argNames |> List.map (fun n -> sprintf "%s : %s" n st) |> String.concat ", "
            let assign = 
                if st = "scalar" then List.map2 (fun f a -> sprintf "%s = %s" f a) fieldNames argNames |> String.concat "; "
                else List.map2 (fun f a -> sprintf "%s = scalar %s" f a) fieldNames argNames |> String.concat "; "
            printfn "        new(%s) = { %s }" args assign


        printfn "    end"

    let rec allCombinations (n : int) (l : list<'a>) =
        if n <= 0 then [[]]
        else
            match l with
                | [] -> 
                    if n <= 0 then [[]]
                    else []
                | h::rest ->
                    let a = rest |> allCombinations n
                    let b = rest |> allCombinations (n-1) |> List.map (fun l -> h::l)
                    a @ b

    let rec zeroOne (n : int) =
        if n <= 0 then [[]]
        else
            let rest = zeroOne (n - 1)
            (rest |> List.map (fun l -> ("O", "0.0") :: l)) @
            (rest |> List.map (fun l -> ("I", "1.0") :: l))

    let vector (dim : int) =
        let name = vectorName dim
        let normalName = sprintf "V%dd" dim
        printfn "type %s =" name
        printfn "    struct"
        let fieldNames = Array.take dim vecFields |> Array.toList
        let argNames = fieldNames |> List.map (fun str -> str.ToLower()) 

        // define the fields
        for i in 0..dim-1 do
            printfn "        val mutable public %s : scalar" vecFields.[i]

        
        let residuals = fieldNames |> List.map (fun n -> sprintf "l.%s - r.%s" n n) |> String.concat "; "
        printfn "        static member Equation(l : %s, r : %s) = Equation [%s]" name name residuals
        printfn "        static member Equation(l : %s, r : %s) = Equation [%s]" normalName name residuals
        printfn "        static member Equation(l : %s, r : %s) = Equation [%s]" name normalName residuals
        

        printfn "        static member Zero = %s(%s)" name (fieldNames |> List.map (fun _ -> "0.0") |> String.concat ", ")
        printfn "        static member One = %s(%s)" name (fieldNames |> List.map (fun _ -> "1.0") |> String.concat ", ")


        for values in zeroOne dim do
            let n = values |> List.map fst |> String.concat ""
            let values = values |> List.map snd |> String.concat ", "
            printfn "        static member %s = %s(%s)" n name values


        let res = fieldNames |> List.collect (fun n -> [n,"scalar 0.0";n,"scalar 1.0"]) |> allCombinations dim
        
        
        printfn "        member x.LengthSquared = %s" (fieldNames |> List.map (fun n -> sprintf "x.%s * x.%s" n n) |> String.concat " + ")
        printfn "        member x.Length = sqrt(%s)" (fieldNames |> List.map (fun n -> sprintf "x.%s * x.%s" n n) |> String.concat " + ")
        printfn "        member x.Normalized = x / x.Length"

        for d in 2..dim-1 do
            for c in allCombinations d fieldNames do
                let name = c |> String.concat ""
                let ctor = c |> List.map (sprintf "x.%s") |> String.concat ", "
                let n = vectorName d
                printfn "        member x.%s = %s(%s)"  name n ctor


        let args (f) = fieldNames |> List.map f |> String.concat ", "
        printfn "        static member (~-) (v : %s) = %s(%s)" name name (args (sprintf "-v.%s"))

        let args (op) = fieldNames |> List.map (fun f -> sprintf "l.%s %s r.%s" f op f) |> String.concat ", "
        printfn "        static member (+) (l : %s, r : %s) = %s(%s)" name name name (args "+")
        printfn "        static member (-) (l : %s, r : %s) = %s(%s)" name name name (args "-")
        printfn "        static member (*) (l : %s, r : %s) = %s(%s)" name name name (args "*")
        printfn "        static member (/) (l : %s, r : %s) = %s(%s)" name name name (args "/")

        printfn "        static member (+) (l : %s, r : %s) = %s(%s)" name normalName name (args "+")
        printfn "        static member (-) (l : %s, r : %s) = %s(%s)" name normalName name (args "-")
        printfn "        static member (*) (l : %s, r : %s) = %s(%s)" name normalName name (args "*")
        printfn "        static member (/) (l : %s, r : %s) = %s(%s)" name normalName name (args "/")
        printfn "        static member (+) (l : %s, r : %s) = %s(%s)" normalName name name (args "+")
        printfn "        static member (-) (l : %s, r : %s) = %s(%s)" normalName name name (args "-")
        printfn "        static member (*) (l : %s, r : %s) = %s(%s)" normalName name name (args "*")
        printfn "        static member (/) (l : %s, r : %s) = %s(%s)" normalName name name (args "/")

        let args = fieldNames |> List.map (fun f -> sprintf "l.%s * r.%s" f f) |> String.concat " + "
        printfn "        static member Dot(l : %s, r : %s) = %s" name name args
        printfn "        static member Dot(l : %s, r : %s) = %s" name normalName args
        printfn "        static member Dot(l : %s, r : %s) = %s" normalName name args

        if dim = 3 then
            let args = "l.Y * r.Z - l.Z * r.Y, l.Z * r.X - l.X * r.Z, l.X * r.Y - l.Y * r.X"
            printfn "        static member Cross(l : %s, r : %s) = %s(%s)" name name name args
            printfn "        static member Cross(l : %s, r : %s) = %s(%s)" name normalName name args
            printfn "        static member Cross(l : %s, r : %s) = %s(%s)" normalName name name args
      

        let args (f) = fieldNames |> List.map f |> String.concat ", "
        for ft in floatTypes do
            printfn "        static member (*) (l : %s, r : %s) = %s(%s)" ft name name (args (sprintf "l * r.%s"))
            printfn "        static member (*) (l : %s, r : %s) = %s(%s)" name ft name (args (sprintf "l.%s * r"))
            printfn "        static member (/) (l : %s, r : %s) = %s(%s)" ft name name (args (sprintf "l / r.%s"))
            printfn "        static member (/) (l : %s, r : %s) = %s(%s)" name ft name (args (sprintf "l.%s / r"))

        for st in scalarTypes do
            let args = argNames |> List.map (fun n -> sprintf "%s : %s" n st) |> String.concat ", "
            let assign = 
                if st = "scalar" then List.map2 (fun a f -> sprintf "%s = %s" f a) argNames fieldNames |> String.concat "; "
                else List.map2 (fun a f -> sprintf "%s = scalar %s" f a) argNames fieldNames |> String.concat "; "
            printfn "        new(%s) = { %s }" args assign

        for vs in vecSuffixes do
            let assign = 
                fieldNames |> List.map (fun f -> sprintf "%s = scalar v.%s" f f) |> String.concat "; "
            printfn "        new(v : V%d%s) = { %s }" dim vs assign


        printfn "    end"


    let run () =
        builder.Clear() |> ignore

        printfn "namespace Aardvark.Base"
        printfn ""

        let dims = [2..4]
        for d in dims do
            vector d

        for d in dims do
            matrix d d
        let str = builder.ToString()
        System.IO.File.WriteAllText(@"C:\Users\schorsch\Desktop\sepp.fs", str)

[<AutoOpen>]
module Operators =
    let inline (=~=) (l : 'a) (r : 'b) =
        ((^a or ^b) : (static member Equation : 'a * 'b -> Equation) (l,r))


[<AutoOpen>]
module Testing =

    type V2s =
        struct
            val mutable public X : scalar
            val mutable public Y : scalar
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

module Ceres =
    open System.Runtime.InteropServices
    open Microsoft.FSharp.Reflection
    type CeresFun = delegate of nativeptr<nativeptr<float>> * nativeptr<float> * nativeptr<nativeptr<float>> -> bool


    module Compiler =
        open System.Reflection
        open System.Reflection.Emit

        type private Marker = Marker

        let private jacobian (i : int) : Jacobian<float> =
            Map.ofList [i, 1.0]

        let emptyJacobian : Jacobian<float> = Map.empty

        let private flags = BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Static
        let private scalarCtor = typeof<scalar>.GetConstructor(flags, Type.DefaultBinder, [| typeof<float>; typeof<Jacobian<float>> |], null)
        let private v2Ctor = typeof<V2s>.GetConstructor(flags, Type.DefaultBinder, [| typeof<scalar>; typeof<scalar> |], null)
        let private v3Ctor = typeof<V3s>.GetConstructor(flags, Type.DefaultBinder, [| typeof<scalar>; typeof<scalar>; typeof<scalar> |], null)
        let private jac = typeof<Marker>.DeclaringType.GetMethod("jacobian", flags, Type.DefaultBinder, [| typeof<int> |], null)
        let private jac0 = typeof<Marker>.DeclaringType.GetProperty("emptyJacobian", flags)

        type private Compiler<'a> private() =
            
            static let trampoline, parameterCount =
                
                let invoke = typeof<'a -> list<Equation>>.GetMethod("Invoke")

                let meth = 
                    DynamicMethod(
                        sprintf "Ceres_Compile_%s" typeof<'a>.FullName,
                        MethodAttributes.Static ||| MethodAttributes.Public,
                        CallingConventions.Standard,
                        typeof<list<Equation>>, 
                        [| typeof<'a -> list<Equation>>; typeof<nativeint>; typeof<bool> |],
                        typeof<obj>,
                        true
                    )

                let il = meth.GetILGenerator()

                let mutable i = 0
                let mutable offset = 0

                let loadScalar() =
                    // a = ptr + nativeint offset
                    il.Emit(OpCodes.Ldarg_1)
                    il.Emit(OpCodes.Ldc_I4, offset)
                    il.Emit(OpCodes.Conv_I)
                    il.Emit(OpCodes.Add)

              
                    il.Emit(OpCodes.Ldobj, typeof<float>)

                    let e = il.DefineLabel()
                    let noJac = il.DefineLabel()

                    il.Emit(OpCodes.Ldarg_2)
                    il.Emit(OpCodes.Brfalse, noJac)

                    il.Emit(OpCodes.Ldc_I4, i)
                    il.EmitCall(OpCodes.Call, jac, null)
                    il.Emit(OpCodes.Br, e)

                    il.MarkLabel(noJac)
                    il.EmitCall(OpCodes.Call, jac0.GetMethod, null)

                    il.MarkLabel(e)


                    il.Emit(OpCodes.Newobj, scalarCtor)
                    offset <- offset + sizeof<float>
                    i <- i + 1
                
                let load (a : Type) =
                    if a = typeof<scalar> then 
                        loadScalar()
                    elif a = typeof<V2s> then
                        loadScalar()
                        loadScalar()
                        il.Emit(OpCodes.Newobj, v2Ctor)
                    elif a = typeof<V3s> then
                        loadScalar()
                        loadScalar()
                        loadScalar()
                        il.Emit(OpCodes.Newobj, v3Ctor)
                    else
                        failwithf "[Ceres] arguments of type %A not implemented" a

                il.Emit(OpCodes.Ldarg_0)

                if FSharpType.IsTuple typeof<'a> then
                    let args = FSharpType.GetTupleElements typeof<'a>
                    let ctor = typeof<'a>.GetConstructor args

                    for a in args do load a

                    il.Emit(OpCodes.Newobj, ctor)
                else
                    load typeof<'a>


                il.EmitCall(OpCodes.Callvirt, invoke, null)
                il.Emit(OpCodes.Ret)

                let f = meth.CreateDelegate(typeof<Func<'a -> list<Equation>, nativeint, bool, list<Equation>>>) |> unbox<Func<'a -> list<Equation>, nativeint, bool, list<Equation>>>
                f, i

            static let reader =
                let meth = 
                    DynamicMethod(
                        sprintf "Ceres_Read_%s" typeof<'a>.FullName,
                        MethodAttributes.Static ||| MethodAttributes.Public,
                        CallingConventions.Standard,
                        typeof<'a>, 
                        [| typeof<nativeint> |],
                        typeof<obj>,
                        true
                    )

                let il = meth.GetILGenerator()

                let mutable i = 0
                let mutable offset = 0

                let loadScalar() =
                    // a = ptr + nativeint offset
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Ldc_I4, offset)
                    il.Emit(OpCodes.Conv_I)
                    il.Emit(OpCodes.Add)

                    
                    il.Emit(OpCodes.Ldobj, typeof<float>)
                    il.EmitCall(OpCodes.Call, jac0.GetMethod, null)

                    il.Emit(OpCodes.Newobj, scalarCtor)
                    offset <- offset + sizeof<float>
                    i <- i + 1
                
                let load (a : Type) =
                    if a = typeof<scalar> then 
                        loadScalar()
                    elif a = typeof<V2s> then
                        loadScalar()
                        loadScalar()
                        il.Emit(OpCodes.Newobj, v2Ctor)
                    elif a = typeof<V3s> then
                        loadScalar()
                        loadScalar()
                        loadScalar()
                        il.Emit(OpCodes.Newobj, v3Ctor)
                    else
                        failwithf "[Ceres] arguments of type %A not implemented" a


                if FSharpType.IsTuple typeof<'a> then
                    let args = FSharpType.GetTupleElements typeof<'a>
                    let ctor = typeof<'a>.GetConstructor args

                    for a in args do load a

                    il.Emit(OpCodes.Newobj, ctor)
                else
                    load typeof<'a> 

                il.Emit(OpCodes.Ret)

                meth.CreateDelegate(typeof<Func<nativeint, 'a>>) |> unbox<Func<nativeint, 'a>>

            static let writer =
                let meth = 
                    DynamicMethod(
                        sprintf "Ceres_Write_%s" typeof<'a>.FullName,
                        MethodAttributes.Static ||| MethodAttributes.Public,
                        CallingConventions.Standard,
                        typeof<System.Void>, 
                        [| typeof<'a>; typeof<nativeint> |],
                        typeof<obj>,
                        true
                    )

                let il = meth.GetILGenerator()

                let mutable i = 0
                let mutable offset = 0

                let l = il.DeclareLocal(typeof<scalar>)

                let storeScalar() =
                    il.Emit(OpCodes.Stloc, l)

                    // a = ptr + nativeint offset
                    il.Emit(OpCodes.Ldarg_1)
                    il.Emit(OpCodes.Ldc_I4, offset)
                    il.Emit(OpCodes.Conv_I)
                    il.Emit(OpCodes.Add)

                    // *a = l.Value
                    il.Emit(OpCodes.Ldloc, l)
                    il.Emit(OpCodes.Ldfld, typeof<scalar>.GetField "Value")
                    il.Emit(OpCodes.Stobj, typeof<float>)

                    offset <- offset + sizeof<float>
                    i <- i + 1

                let store (a : Type) =
                    if a = typeof<scalar> then
                        storeScalar()
                    elif a = typeof<V2s> then
                        il.Emit(OpCodes.Dup)
                        il.Emit(OpCodes.Ldfld, typeof<V2s>.GetField "X")
                        storeScalar()
                        il.Emit(OpCodes.Ldfld, typeof<V2s>.GetField "Y")
                        storeScalar()
                    elif a = typeof<V3s> then
                        il.Emit(OpCodes.Dup)
                        il.Emit(OpCodes.Dup)
                        il.Emit(OpCodes.Ldfld, typeof<V2s>.GetField "X")
                        storeScalar()
                        il.Emit(OpCodes.Ldfld, typeof<V2s>.GetField "Y")
                        storeScalar()
                        il.Emit(OpCodes.Ldfld, typeof<V2s>.GetField "Z")
                        storeScalar()
                    else
                        failwithf "[Ceres] arguments of type %A not implemented" a


                if FSharpType.IsTuple typeof<'a> then
                    let args = FSharpType.GetTupleElements typeof<'a>
                    
                    for i in 0..args.Length-1 do
                        let prop = typeof<'a>.GetProperty (sprintf "Item%d" (i + 1))
                        il.Emit(OpCodes.Ldarg_0)
                        il.EmitCall(OpCodes.Call, prop.GetMethod, null)
                        store args.[i]


                else
                    il.Emit(OpCodes.Ldarg_0)
                    store typeof<'a>

                il.Emit(OpCodes.Ret)

                meth.CreateDelegate(typeof<Action<'a, nativeint>>) |> unbox<Action<'a, nativeint>>


            static member ParameterCount = parameterCount

            static member Read(ptr : nativeint) =
                reader.Invoke(ptr)

            static member Write(value : 'a, ptr : nativeint) =
                writer.Invoke(value, ptr)

            static member Invoke(f : 'a -> list<Equation>, ptr : nativeint, jac : bool) =
                trampoline.Invoke(f, ptr, jac)

        let invoke (f : 'a -> list<Equation>) (ptr : nativeint) (jac : bool)=
            Compiler<'a>.Invoke(f, ptr, jac)

        let parameterCount<'a> : int =
            Compiler<'a>.ParameterCount

        let write (value : 'a) (ptr : nativeint) =
            Compiler<'a>.Write(value, ptr)


        let read<'a> (ptr : nativeint) =
            Compiler<'a>.Read(ptr)

    type scalar with
        static member Reader (offset : byref<int>) =
            let o = offset
            offset <- o + 1
            fun (ptr : nativeptr<float>) -> scalar(NativePtr.get ptr o, Map.ofList [o, 1.0])

    type V2s with
        static member Reader (offset : byref<int>) =
            let x = scalar.Reader(&offset)
            let y = scalar.Reader(&offset)
            fun ptr -> V2s(x ptr, y ptr)

    type V3s with
        static member Reader (offset : byref<int>) =
            let x = scalar.Reader(&offset)
            let y = scalar.Reader(&offset)
            let z = scalar.Reader(&offset)
            fun ptr -> V3s(x ptr, y ptr, z ptr)

    type V4s with
        static member Reader (offset : byref<int>) =
            let x = scalar.Reader(&offset)
            let y = scalar.Reader(&offset)
            let z = scalar.Reader(&offset)
            let w = scalar.Reader(&offset)
            fun ptr -> V4s(x ptr, y ptr, z ptr, w ptr)


    let private toObj (f : 'a -> 'b) =
        fun a -> f a :> obj

    let rec invoke<'a> : int * ('a -> nativeptr<float> -> list<Equation>) =
        let t = typeof<'a>
        let rec deconstruct (t : Type) =
            if FSharpType.IsFunction t then
                let arg, ret = FSharpType.GetFunctionElements t
                let args, res = deconstruct ret
                arg::args, res
            else
                [], t

        let args, ret = deconstruct t


        let argReaders = System.Collections.Generic.List()
        let mutable offset = 0
        for a in args do
            let reader =
                if a = typeof<scalar> then scalar.Reader(&offset) |> toObj
                elif a = typeof<V2s> then V2s.Reader(&offset) |> toObj
                elif a = typeof<V3s> then V3s.Reader(&offset) |> toObj
                elif a = typeof<V4s> then V4s.Reader(&offset) |> toObj
                else failwithf "bad parameter type: %A" a

            argReaders.Add(reader)
            ()

        let invoke (f : 'a) (ptr : nativeptr<float>) =
            
            let args = argReaders |> Seq.map (fun f -> f ptr) |> Seq.toList
            let rec run (args : list<obj>) (f : obj) =
                match args with
                    | [] -> f |> unbox<list<Equation>>
                    | a::args ->
                        let i = f.GetType().GetMethod("Invoke", [|a.GetType()|])
                        let rest = i.Invoke(f, [| a |])
                        run args rest
            run args f
            

        offset, invoke

    type CeresCostFunction(parameterCount : int, residualCount : int, f : CeresFun) =
        member x.ParameterCount = parameterCount
        member x.ResidualCount = residualCount
        member x.Function = f
 
        static member Compile (f : 'a) =
            
            let parameterCount, invoke = invoke<'a>
            let temp = NativePtr.stackalloc parameterCount
            let residualCount = invoke f temp |> List.collect (fun (e : Equation) -> e.Residuals) |> List.length
            let func = 
                CeresFun (fun parameters residuals jacobians ->
                    let p = NativePtr.read parameters
                    let res = invoke f p |> List.collect (fun (e : Equation) -> e.Residuals) |> List.toArray

                    for i in 0..residualCount-1 do
                        NativePtr.set residuals i res.[i].Value

                    if NativePtr.toNativeInt jacobians <> 0n then
                        let j = NativePtr.read jacobians
                        if NativePtr.toNativeInt j <> 0n then
                            let mutable j = j
                            for pi in 0..parameterCount-1 do
                                for i in 0..residualCount-1 do
                                    match Map.tryFind pi res.[i].Jacobian with
                                        | Some v -> NativePtr.write j v
                                        | None -> NativePtr.write j 0.0
                                    j <- NativePtr.add j 1
                  
                    true
                )
            CeresCostFunction(parameterCount, residualCount, func)

        static member Compile2 (f : 'a -> list<Equation>) =
            let parameterCount = Compiler.parameterCount<'a>
            let temp : nativeptr<float> = NativePtr.stackalloc parameterCount
            let residualCount = Compiler.invoke f (NativePtr.toNativeInt temp) false |> List.collect (fun (e : Equation) -> e.Residuals) |> List.length
            let func = 
                CeresFun (fun parameters residuals jacobians ->
                    let wantJacobian =
                        if NativePtr.toNativeInt jacobians <> 0n then
                            let j = NativePtr.read jacobians
                            NativePtr.toNativeInt j <> 0n
                        else
                            false

                    let p = NativePtr.read parameters
                    let res = Compiler.invoke f (NativePtr.toNativeInt p) wantJacobian |> List.collect (fun (e : Equation) -> e.Residuals) |> List.toArray

                    for i in 0..residualCount-1 do
                        NativePtr.set residuals i res.[i].Value

                    if wantJacobian then
                        let mutable j = NativePtr.read jacobians
                        for i in 0..residualCount-1 do
                            for pi in 0..parameterCount-1 do
                                match Map.tryFind pi res.[i].Jacobian with
                                    | Some v -> NativePtr.write j v
                                    | None -> NativePtr.write j 0.0
                                j <- NativePtr.add j 1
                    true
                )
            CeresCostFunction(parameterCount, residualCount, func)



    [<DllImport("CeresCpp.dll", EntryPoint="solve")>]
    extern float ceres_solve(int parameterCount, int residualCount, void* evaluate, void* parameter)

    let solve (initial : 'a) (equations : 'a -> list<Equation>) =
        let f = CeresCostFunction.Compile2 equations
        let gcf = GCHandle.Alloc(f.Function, GCHandleType.Normal)
        let fptr = Marshal.GetFunctionPointerForDelegate(f.Function)
        let res : float[] = Array.zeroCreate f.ParameterCount

        let gc = GCHandle.Alloc(res, GCHandleType.Pinned)
        try 
            Compiler.write initial (gc.AddrOfPinnedObject())
            let res = ceres_solve(f.ParameterCount, f.ResidualCount, fptr, gc.AddrOfPinnedObject())
            printfn "residual: %A" res
            Compiler.read<'a> (gc.AddrOfPinnedObject())
        finally 
            gc.Free()
            gcf.Free()


    let sinCos() =
        let system (b : scalar) =
            [
                //sin a + cos b =~= cos a + sin b
                b * b =~= 0.6168500184
                cos b =~= sin b
            ]

        solve (scalar 0.0) system

    let exampleSystem (x1 : scalar, x2 : scalar, x3 : scalar, x4 : scalar) =
        [
            x1 + 10.0 * x2                  =~= 0.0
            sqrt 5.0 * (x3 - x4)            =~= 0.0
            pown (x2 - 2.0 * x3) 2          =~= 0.0
            sqrt 10.0 * pown (x1 - x4) 2    =~= 0.0
        ]

    let exampleSystem2 (x1 : scalar, x2 : scalar) =
        let a = x1 * x1 + x2 * x2 - 1.0
        let b = x1 - x2 

        printfn "{ a = %A; b = %A }" a b

        [
            a =~= 0.0
            b =~= 0.0
        ]

    let schwefel2 (x1 : scalar, x2 : scalar) =
        [
            418.9829 * 2.0 - (x1 * sin (sqrt (abs x1)) + x2 * sin (sqrt (abs x2))) =~= 0.0
        ]

    let rot (phi : scalar, theta : scalar, r : scalar) =
        [
            V3s(cos phi * cos theta, sin phi * cos theta, sin theta) * r =~= V3s(1,2,3)
        ]



    let example() =
//        let sol = solve (scalar 3.0, scalar -1.0, scalar 0.0, scalar 1.0) exampleSystem
//        printfn "%A" sol

        let (phi, theta, r) = solve (scalar 0.0, scalar 0.0, scalar 1.0) rot

        let res = V3d(cos phi.Value * cos theta.Value, sin phi.Value * cos theta.Value, sin theta.Value) * r.Value


        printfn "%A" res
