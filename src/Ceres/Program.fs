namespace Ceres
open System
open System.Runtime.InteropServices
#nowarn "9"

module Test =

    [<Literal>]
    let lib = "CeresCPP.dll"

    [<DllImport(lib)>]
    extern int main()

[<StructLayout(LayoutKind.Sequential)>]
type CameraLocation =
    struct
        val mutable public AxisAngleX : float
        val mutable public AxisAngleY : float
        val mutable public AxisAngleZ : float

        val mutable public OriginX : float
        val mutable public OriginY : float
        val mutable public OriginZ : float

        new(ax,ay,az,ox,oy,oz) = { AxisAngleX = ax; AxisAngleY = ay; AxisAngleZ = az;
                                   OriginX = ox; OriginY = oy; OriginZ = oz; }
    end

[<StructLayout(LayoutKind.Sequential)>]
type CameraInternal =
    struct
        val mutable public FocalLength : float
        val mutable public PrincipalX : float
        val mutable public PrincipalY : float

        new(f,px,py) = { FocalLength = f; PrincipalX = px; PrincipalY = py; }
    end

[<StructLayout(LayoutKind.Sequential)>]
type CameraDistortion =
    struct
        val mutable public Distortion2 : float
        val mutable public Distortion4 : float

        new(d2,d4) = {Distortion2 = d2; Distortion4 = d4; }
    end

[<StructLayout(LayoutKind.Sequential)>]
type Point2d =
    struct
        val mutable public X : float
        val mutable public Y : float

        new(x,y) = { X = x; Y = y }
    end

[<StructLayout(LayoutKind.Sequential)>]
type Point3d = 
    struct
        val mutable public X : float
        val mutable public Y : float
        val mutable public Z : float

        new(x,y,z) = { X = x; Y = y; Z = z }
    end

[<StructLayout(LayoutKind.Sequential)>]
type Observed2dPredicted2d =
    struct
        val mutable public ObservedX : float
        val mutable public ObservedY : float
        val mutable public PredictedX : float
        val mutable public PredictedY : float

        new(ox,oy,px,py) = { ObservedX = ox; ObservedY = oy; PredictedX = px; PredictedY = py }
    end

module CameraCalibration =
    [<Literal>]
    let lib = "CeresCPP.dll"

    [<DllImport(lib)>]
    extern int private calibrate_camera_single_image_no_distortion_optimization(
            int observation_count, Observed2dPredicted2d[] observations,
            CameraLocation[] cameraLocation, CameraInternal[] cameraInternal, CameraDistortion[] cameraDistortion)

    let CalibrateCameraSingleImageNoDistortionOptimization
        (observations: Observed2dPredicted2d[])
        (cameraLocation: CameraLocation[])
        (cameraInternal: CameraInternal[])
        (cameraDistortion: CameraDistortion[])
        : int =
        calibrate_camera_single_image_no_distortion_optimization(observations.Length, observations,
                                                    cameraLocation, cameraInternal, cameraDistortion)

    [<DllImport(lib)>]
    extern int private calibrate_camera_single_image(
            int observation_count, Observed2dPredicted2d[] observations,
            CameraLocation[] cameraLocation, CameraInternal[] cameraInternal, CameraDistortion[] cameraDistortion)

    let CalibrateCameraSingleImage
        (observations: Observed2dPredicted2d[])
        (cameraLocation: CameraLocation[])
        (cameraInternal: CameraInternal[])
        (cameraDistortion: CameraDistortion[])
        : int =
        calibrate_camera_single_image(observations.Length, observations,
                                      cameraLocation, cameraInternal, cameraDistortion)

    [<DllImport(lib)>]
    extern int private bundle_adjustment(
            int observation_count, Point2d[] point2ds, int[] locationIndices, int[] cameraIndices, int[] pointIndices,
            int point_count, Point3d[] point3ds,
            int location_count, CameraLocation[] locations,
            int camera_count, CameraInternal[] cameraInternal, CameraDistortion[] cameraDistortion)

    let BundleAdjustment
        (point2ds: Point2d[])
        (locationIndices: int[])
        (cameraIndices: int[])
        (pointIndices: int[])
        (point3ds: Point3d[])
        (locations: CameraLocation[])
        (cameraInternal: CameraInternal[])
        (cameraDistortion: CameraDistortion[])
        : int =
        bundle_adjustment(point2ds.Length, point2ds, locationIndices, cameraIndices, pointIndices,
                          point3ds.Length, point3ds,
                          locations.Length, locations,
                          cameraInternal.Length, cameraInternal, cameraDistortion)

    [<DllImport(lib)>]
    extern int private bundle_adjustment_no_distortion_optimization(
            int observation_count, Point2d[] point2ds, int[] locationIndices, int[] cameraIndices, int[] pointIndices,
            int point_count, Point3d[] point3ds,
            int location_count, CameraLocation[] locations,
            int camera_count, CameraInternal[] cameraInternal, CameraDistortion[] cameraDistortion)

    let BundleAdjustmentNoDistortionOptimization
        (point2ds: Point2d[])
        (locationIndices: int[])
        (cameraIndices: int[])
        (pointIndices: int[])
        (point3ds: Point3d[])
        (locations: CameraLocation[])
        (cameraInternal: CameraInternal[])
        (cameraDistortion: CameraDistortion[])
        : int =
        bundle_adjustment(point2ds.Length, point2ds, locationIndices, cameraIndices, pointIndices,
                          point3ds.Length, point3ds,
                          locations.Length, locations,
                          cameraInternal.Length, cameraInternal, cameraDistortion)

    [<DllImport(lib)>]
    extern int private bundle_adjustment_no_camera_optimization(
            int observation_count, Point2d[] point2ds, int[] locationIndices, int[] cameraIndices, int[] pointIndices,
            int point_count, Point3d[] point3ds,
            int location_count, CameraLocation[] locations,
            int camera_count, CameraInternal[] cameraInternal, CameraDistortion[] cameraDistortion)

    let BundleAdjustmentNoCameraOptimization
        (point2ds: Point2d[])
        (locationIndices: int[])
        (cameraIndices: int[])
        (pointIndices: int[])
        (point3ds: Point3d[])
        (locations: CameraLocation[])
        (cameraInternal: CameraInternal[])
        (cameraDistortion: CameraDistortion[])
        : int =
        bundle_adjustment_no_camera_optimization(point2ds.Length, point2ds, locationIndices, cameraIndices, pointIndices,
                          point3ds.Length, point3ds,
                          locations.Length, locations,
                          cameraInternal.Length, cameraInternal, cameraDistortion)

//module Entry =
//    [<EntryPoint>]
//    let main argv = 
//        Test.main() |> printfn "ceres said: %d"
//        0 
