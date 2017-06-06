namespace Aardvark.Reconstruction

open System
open System.IO
open System.Collections.Generic

open Aardvark.Base


module BundlerInput =
    
    let mk featureType matcher (images : list<PixImage<byte>>) =
        let (fs, pix) = Feature.ofImages featureType images

        let ts = Match.mkTracks fs matcher

        ts, pix
    