open System
open System.IO

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

    let matrix (rows : int)  =
        let cols = rows
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

        do
            let dim = rows * cols
            printfn "        static member CeresDim = %d" dim
            printfn "        static member CeresRead(ptr : nativeptr<float>, id : int) ="
            let args = List.init dim (fun i -> sprintf "scalar.Variable(id + %d, NativePtr.get ptr (id + %d))" i i) |> String.concat ", "
            printfn "            %s(%s)" name args

            printfn "        static member CeresWrite(value : %s, ptr : nativeptr<float>, id : int) =" name
            let args = List.init dim (fun i -> sprintf "NativePtr.set ptr (id + %d) value.%s.Value" i fieldNames.[i]) |> String.concat "; "
            printfn "            %s" args


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
 
        let args = fieldNames |> List.map (sprintf "x.%s.Value") |> String.concat ", " 
        printfn "        member x.Value = %s(%s)" normalName args
        
        if rows = cols && rows = 3 then
            printfn "        static member Rotation(angle : scalar) = "
            printfn "            let ca = cos angle"
            printfn "            let sa = sin angle"
            printfn "            M33s(ca, sa, scalar 0.0, -sa, ca, scalar 0.0, scalar 0.0, scalar 0.0, scalar 1.0)"

            printfn "        static member Translation(x : scalar, y : scalar) = M33s(scalar 1.0, scalar 0.0, x, scalar 0.0, scalar 1.0, y, scalar 0.0, scalar 0.0, scalar 1.0)"
            printfn "        static member Translation(v : V2d) = M33s.Translation(scalar v.X, scalar v.Y)"
            printfn "        static member Translation(v : V2s) = M33s.Translation(v.X, v.Y)"
            printfn "        static member Scale(x : scalar, y : scalar) = M33s(x, scalar 0.0, scalar 0.0, scalar 0.0, y, scalar 0.0, scalar 0.0, scalar 0.0, scalar 1.0)"
            printfn "        static member Scale(f : scalar) = M33s.Scale(f,f)"
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

        printfn "        static member CeresDim = %d" dim
        printfn "        static member CeresRead(ptr : nativeptr<float>, id : int) ="
        let args = List.init dim (fun i -> sprintf "scalar.Variable(id + %d, NativePtr.get ptr (id + %d))" i i) |> String.concat ", "
        printfn "            %s(%s)" name args

        printfn "        static member CeresWrite(value : %s, ptr : nativeptr<float>, id : int) =" name
        let args = List.init dim (fun i -> sprintf "NativePtr.set ptr (id + %d) value.%s.Value" i fieldNames.[i]) |> String.concat "; "
        printfn "            %s" args
        
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
         
        let args = fieldNames |> List.map (sprintf "x.%s.Value") |> String.concat ", " 
        printfn "        member x.Value = %s(%s)" normalName args

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
        printfn "#nowarn \"9\""
        printfn ""
        printfn "open Microsoft.FSharp.NativeInterop"

        let dims = [2..4]
        for d in dims do
            vector d

        for d in dims do
            matrix d
        let str = builder.ToString()
        let path = Path.Combine(__SOURCE_DIRECTORY__, "Math.fs")
        System.IO.File.WriteAllText(path, str)
