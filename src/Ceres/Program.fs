namespace Ceres
open System
open System.Runtime.InteropServices
#nowarn "9"

module Test =

    [<Literal>]
    let lib = "CeresCPP.dll"

    [<DllImport(lib)>]
    extern int main()

module CameraCalibration =
    [<Literal>]
    let lib = "CeresCPP.dll"

    [<DllImport(lib)>]
    extern int private bundle_adjustment(
        int flags,
        int observation_count, double[] observations, int[] pointIndices, int[] extrinsicIndices, int[] intrinsicIndices,
        int point_count, double[] pointArray,
        int extrinsics_count, double[] extrinsicsArray,
        int intrinsics_count, double[] focalPrincipalArray, double[] distortionArray
        )

    let BundleAdjustment
        (flags: int)
        (observationCount: int)
        (observationArray: double[])
        (pointIndices: int[])
        (extrinsicsIndices: int[])
        (intrinsicsIndices: int[])
        (pointCount: int)
        (pointArray: double[])
        (extrinsicsCount: int)
        (extrinsics: double[])
        (intrinsicsCount: int)
        (focalPrincipalArray: double[])
        (distortionArray: double[])
        : int =
        bundle_adjustment(flags, observationCount, observationArray, pointIndices, extrinsicsIndices, intrinsicsIndices,
                          pointCount, pointArray,
                          extrinsicsCount, extrinsics,
                          intrinsicsCount, focalPrincipalArray, distortionArray)

//module Entry =
//    [<EntryPoint>]
//    let main argv = 
//        Test.main() |> printfn "ceres said: %d"
//        0 
