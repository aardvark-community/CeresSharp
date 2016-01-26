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
type Observation =
    struct
        val mutable public ObsX : float
        val mutable public ObsY : float
        val mutable public RefX : float
        val mutable public RefY : float

        new(ox,oy,rx,ry) = { ObsX = ox; ObsY = oy; RefX= rx; RefY = ry }
    end

module CameraCalibration =
    [<Literal>]
    let lib = "CeresCPP.dll"

    [<DllImport(lib)>]
    extern int private calibrate_camera(int observation_count, Observation[] observations, double[] camera)
        
    let CalibrateCamera 
        (observations: Observation[])
        (camera: double[]) : int =
        calibrate_camera(observations.Length, observations, camera)

//[<EntryPoint>]
//let main argv = 
//    Test.main() |> printfn "ceres said: %d"
//    0 
