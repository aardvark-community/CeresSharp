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
type CameraInternalNoDistortion =
    struct
        val mutable public FocalLength : float
        val mutable public PrincipalX : float
        val mutable public PrincipalY : float

        new(f,px,py) = { FocalLength = f; PrincipalX = px; PrincipalY = py; }
    end

[<StructLayout(LayoutKind.Sequential)>]
type CameraInternal =
    struct
        val mutable public FocalLength : float
        val mutable public PrincipalX : float
        val mutable public PrincipalY : float

        val mutable public Distortion2 : float
        val mutable public Distortion4 : float

        new(f,px,py,d2,d4) = { FocalLength = f; PrincipalX = px; PrincipalY = py; Distortion2 = d2; Distortion4 = d4; }
    end

[<StructLayout(LayoutKind.Sequential)>]
type Camera =
    struct
        val mutable public Location : CameraLocation
        val mutable public Internal : CameraInternal
        new (ax, ay, az, ox, oy, oz, f, px, py, d2, d4) =
            { Location = CameraLocation(ax, ay, az, ox, oy, oz);
              Internal = CameraInternal(f, px, py, d2, d4); }
    end

[<StructLayout(LayoutKind.Sequential)>]
type CameraNoDistortion =
    struct
        val mutable public Location : CameraLocation
        val mutable public Internal : CameraInternalNoDistortion
        new (ax, ay, az, ox, oy, oz, f, px, py) =
            { Location = CameraLocation(ax, ay, az, ox, oy, oz);
              Internal = CameraInternalNoDistortion(f, px, py); }
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
            CameraLocation[] cameraLocation, CameraInternalNoDistortion[] cameraInternal)

    let CalibrateCameraSingleImageNoDistortion
        (observations: Observed2dPredicted2d[])
        (cameraLocation: CameraLocation[])
        (cameraInternal: CameraInternalNoDistortion[])
        : int =
        calibrate_camera_single_image_no_distortion(observations.Length, observations,
                                                    cameraLocation, cameraInternal)

    [<DllImport(lib)>]
    extern int private calibrate_camera_single_image(
            int observation_count, Observed2dPredicted2d[] observations,
            CameraLocation[] cameraLocation, CameraInternal[] cameraInternal)

    let CalibrateCameraSingleImage
        (observations: Observed2dPredicted2d[])
        (cameraLocation: CameraLocation[])
        (cameraInternal: CameraInternal[])
        : int =
        calibrate_camera_single_image(observations.Length, observations,
                                      cameraLocation, cameraInternal)

module Entry =
    [<EntryPoint>]
    let main argv = 
        Test.main() |> printfn "ceres said: %d"
        0 
