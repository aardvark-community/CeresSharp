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
        int pointCount, int fixedPointCount, double[] pointArray,
        int extrinsicsCount, int fixedExtrinsicsCount, double[] extrinsicsArray,
        int intrinsicsCount, int fixedFocalPrincipalCount, double[] focalPrincipalArray,
        int fixedDistortionCount, double[] distortionArray
        )

    let BundleAdjustment
        (flags: int)
        (observationCount: int)
        (observationArray: double[])
        (pointIndices: int[])
        (extrinsicsIndices: int[])
        (intrinsicsIndices: int[])
        (pointCount: int)
        (fixedPointCount: int)
        (pointArray: double[])
        (extrinsicsCount: int)
        (fixedExtrinsicsCount: int)
        (extrinsics: double[])
        (intrinsicsCount: int)
        (fixedFocalPrincipalCount: int)
        (focalPrincipalArray: double[])
        (fixedDistortionCount: int)
        (distortionArray: double[])
        : int =
        bundle_adjustment(flags, observationCount, observationArray, pointIndices, extrinsicsIndices, intrinsicsIndices,
                          pointCount, fixedPointCount, pointArray,
                          extrinsicsCount, fixedExtrinsicsCount, extrinsics,
                          intrinsicsCount, fixedFocalPrincipalCount, focalPrincipalArray, fixedDistortionCount, distortionArray)

//module Entry =
//    [<EntryPoint>]
//    let main argv = 
//        Test.main() |> printfn "ceres said: %d"
//        0 
