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
type Observed2d =
    struct
        val mutable public ObservedX : float
        val mutable public ObservedY : float

        new(ox,oy) = { ObservedX = ox; ObservedY = oy }
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
    extern int private calibrate_camera_single_image_no_distortion(
            int observation_count, Observed2dPredicted2d[] observations,
            CameraLocation[] cameraLocation, CameraInternal[] cameraInternal)

    let CalibrateCameraSingleImageNoDistortion
        (observations: Observed2dPredicted2d[])
        (cameraLocation: CameraLocation[])
        (cameraInternal: CameraInternal[])
        : int =
        calibrate_camera_single_image_no_distortion(observations.Length, observations,
                                                    cameraLocation, cameraInternal)

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

module Entry =
    [<EntryPoint>]
    let main argv = 
        Test.main() |> printfn "ceres said: %d"
        0 
