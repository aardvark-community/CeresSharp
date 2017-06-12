namespace Aardvark.Reconstruction

open System
open Aardvark.Base

open System.Collections.Generic


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Feature =
    
    module Akaze =
        open OpenCvSharp
        open OpenCvSharp.XFeatures2D
        open Microsoft.FSharp.NativeInterop
        
        let private ofMat (input : Mat) =
            let akaze = AKAZE.Create()
            let features = akaze.Detect(input)

            let mutable features = features
            use descriptors = new Mat()
            akaze.Compute(InputArray.op_Implicit input, &features, OutputArray.op_Implicit descriptors)

            features |> Array.mapi (fun i f ->
                let desc = Array.create descriptors.Cols 0uy
                descriptors.Row.[i].GetArray(0, 0, desc)

                let x = float f.Pt.X / float input.Cols
                let y = float f.Pt.Y / float input.Rows

                {
                    ndc = V2d(2.0 * x - 1.0, 1.0 - 2.0 * y)
                    angle = float f.Angle * Constant.RadiansPerDegree
                    size = float f.Size
                    response = float f.Response
                    descriptor = FeatureDescriptor (desc |> Array.map (fun v -> float v / 255.0))
                }
            )

        let ofFile (file : string) =
            use input = new Mat(file, ImreadModes.Color)
            ofMat input

        let ofImage (img : PixImage) =
            use input = img.ToMat()
            ofMat input
            
    module Orb =
        open OpenCvSharp
        open OpenCvSharp.XFeatures2D
        open Microsoft.FSharp.NativeInterop
    
        let private ofMat (input : Mat) =
        
            let orb = ORB.Create( nFeatures = 5000 )


            let features = orb.Detect(input)
        

            let mutable features = features
            use descriptors = new Mat()
            orb.Compute(InputArray.op_Implicit input, &features, OutputArray.op_Implicit descriptors)

            features |> Array.mapi (fun i f ->
                let desc = Array.create descriptors.Cols 0uy
                descriptors.Row.[i].GetArray(0, 0, desc)

                let x = float f.Pt.X / float input.Cols
                let y = float f.Pt.Y / float input.Rows
                
                {
                    ndc = V2d(2.0 * x - 1.0,  1.0 - 2.0 * y )
                    angle = float f.Angle
                    size = float f.Size
                    response = float f.Response
                    descriptor = FeatureDescriptor (desc |> Array.map (fun v -> float v / 255.0))
                }
            )

        let ofFile (file : string) =
            use input = new Mat(file)
            ofMat input

        let ofImage (img : PixImage) =
            use input = img.ToMat()
            ofMat input

    module Brisk =
        open OpenCvSharp
        open OpenCvSharp.XFeatures2D
        open Microsoft.FSharp.NativeInterop
    
        let private ofMat (input : Mat) =
            let brisk = BRISK.Create()
            let features = brisk.Detect(input)
        
            let mutable features = features
            use descriptors = new Mat()
            brisk.Compute(InputArray.op_Implicit input, &features, OutputArray.op_Implicit descriptors)

            features |> Array.mapi (fun i f ->
                let desc = Array.create descriptors.Cols 0uy
                descriptors.Row.[i].GetArray(0, 0, desc)

                let x = float f.Pt.X / float input.Cols
                let y = float f.Pt.Y / float input.Rows

                {
                    ndc = V2d(2.0 * x - 1.0, 1.0 - 2.0 * y)
                    angle = float f.Angle
                    size = float f.Size
                    response = float f.Response
                    descriptor = FeatureDescriptor (desc |> Array.map (fun v -> float v / 255.0))
                }
            )

        let ofFile (file : string) =
            use input = new Mat(file)
            ofMat input

        let ofImage (img : PixImage) =
            use input = img.ToMat()
            ofMat input

    type FeatureExtract =
        | Orb
        | Brisk
        | Akaze

    let ofImages (feature : FeatureExtract) (imgs : list<PixImage<byte>>) : MapExt<CameraId, list<FeatureNode>> * MapExt<CameraId, PixImage<byte>> =
        
        let getFeatures img =
            match feature with
                | Orb -> Orb.ofImage img
                | Brisk -> Brisk.ofImage img
                | Akaze -> Akaze.ofImage img
            |> Array.map FeatureNode.ofFeature
            |> Array.toList

        let cams = imgs |> List.mapi ( fun cid img -> 
            let ftr = getFeatures img 
            printfn "[FeatureExtract] img %A of %A: #%A features" cid ((imgs |> List.length) - 1) ftr.Length
            CameraId(cid), img, ftr 
            ) 

        (cams |> List.map ( fun (cid,_,ftr) -> cid,ftr ) |> MapExt.ofList ),
        (cams |> List.map ( fun (cid,img,_) -> cid,img ) |> MapExt.ofList )
    