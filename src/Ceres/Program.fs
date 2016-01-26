namespace Ceres

module Test =

    open System
    open System.Runtime.InteropServices

    [<Literal>]
    let lib = "CeresCPP.dll"

    [<DllImport(lib)>]
    extern int main()


//[<EntryPoint>]
//let main argv = 
//    Test.main() |> printfn "ceres said: %d"
//    0 
