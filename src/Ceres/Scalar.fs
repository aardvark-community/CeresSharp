﻿namespace Aardvark.Base

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

        static member CeresDim = 1
        static member CeresRead(ptr : nativeptr<float>, id : int) = scalar.Variable(id, NativePtr.get ptr id)
        static member CeresWrite(value : scalar, ptr : nativeptr<float>, id : int) = NativePtr.set ptr id value.Value

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
            scalar(sqrt v.Value, Jacobian.Div(v.Jacobian, 2.0 * sqrt v.Value))

        static member Abs (v : scalar) =
            if v.Value > 0.0 then v
            else -v

        static member Variable(id : int, v : float) =
            scalar(v, Map.ofList [id, 1.0])



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

