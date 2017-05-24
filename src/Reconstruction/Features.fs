namespace Aardvark.Reconstruction

open System
open Aardvark.Base

open System.Collections.Generic

type FeatureDescriptor internal(raw : float[]) =
    let dim = raw.Length
    let scale = sqrt (float dim)
    let vector = raw |> Array.map (fun v -> v / scale)

    member x.Dimension = dim
    member x.Data = vector
    member x.RawData = raw

    static member Distance(l : FeatureDescriptor, r : FeatureDescriptor) =
        if l.Dimension <> r.Dimension then
            failwithf "cannot compare features with different dimensions: %A vs %A" l.Dimension r.Dimension

        let l = l.Data
        let r = r.Data
        let mutable res = 0.0
        for i in 0 .. l.Length - 1 do
            let v = l.[i] - r.[i]
            res <- res + v * v
        sqrt res
        
type Feature =
    {
        ndc         : V2d
        angle       : float
        size        : float
        response    : float
        descriptor  : FeatureDescriptor
    }

[<CustomEquality; NoComparison>]
type FeatureNode =
    {
        feature         : Feature
        corresponding   : Dict<CameraId, HashSet<FeatureNode>>
    }
    override x.GetHashCode() = x.feature.GetHashCode()
    override x.Equals o =
        match o with
            | :? FeatureNode as o -> x.feature = o.feature
            | _ -> false

    member x.Add(image : CameraId, f : FeatureNode) =
        let set = x.corresponding.GetOrCreate(image, fun _ -> HashSet())
        set.Add f |> ignore

module Features =
    
    let ofImages (imgs : PixImage<byte>) : MapExt<CameraId, list<FeatureNode>> * MapExt<CameraId, PixImage<byte>> =
        failwith ""

    