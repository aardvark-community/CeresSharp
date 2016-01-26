// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module Test =

    open System
    open System.Runtime.InteropServices

    [<Literal>]
    let lib = "CeresCPP.dll"

    [<DllImport(lib)>]
    extern int main()


[<EntryPoint>]
let main argv = 
    Test.main() |> printfn "ceres said: %d"
    0 
