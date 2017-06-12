namespace Aardvark.Reconstruction

open System
open System.IO
open System.Collections.Generic

open Aardvark.Base


module BundlerInput =
    
    let mk featureType matcher (images : list<PixImage<byte>>) =
        let (fs, pix) = Feature.ofImages featureType images

        for (cid,fs) in fs |> MapExt.toList do
            let b = fs |> List.distinctBy ( fun f -> f.feature ) |> List.length
            let a = fs |> List.length
            let c = a - b
            printfn "[fs] Cam %A: original fs %A - distinct fs %A (diff = %A)" cid.Id a b c

        let ts = Match.mkTracks fs matcher

        ts, pix
    