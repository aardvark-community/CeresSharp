open System.Security.AccessControl


let res = System.Text.StringBuilder()

let printf fmt = Printf.kprintf (fun str -> res.Append str |> ignore) fmt
let printfn fmt = Printf.kprintf (fun str -> res.AppendLine str |> ignore) fmt

type ReturnType =
    | Scalar
    | Vec of int

type Kind =
    | MutateArray
    | ReturnArray
    | Indexed

let run() =
    res.Clear() |> ignore

    printfn "namespace CeresSharp"

    printfn "open Aardvark.Base"

    printfn "[<AutoOpen>]"
    printfn "module ``Problem CostFunction Extensions`` = "
    printfn "    type Problem with"

    let blockCounts = [1; 2; 3; 4]
    let typeArgs = ["b"; "c"; "d"; "e"]
    let kinds = [ MutateArray; ReturnArray; Indexed ]
    let types = [ Scalar; Vec 2; Vec 3; Vec 4 ]
    let loss = [ true; false ]

    
    for cnt in blockCounts do
        let typeArgs = List.take cnt typeArgs |> List.toArray
        for k in kinds do
            for t in types do
                for l in loss do
                    
                    let resName =
                        match t with
                            | Scalar -> "scalar"
                            | Vec d -> sprintf "V%ds" d

                    let costFunArgs =
                        match k with
                            | MutateArray -> 
                                List.concat [
                                    typeArgs |> Array.toList |> List.map (sprintf "'%s[]")
                                    [sprintf "%s[]" resName]

                                    ["unit"]
                                ]
                            | ReturnArray ->
                                List.concat [
                                    typeArgs |> Array.toList |> List.map (sprintf "'%s[]")

                                    [sprintf "%s[]" resName]
                                ] 
                                
                            | Indexed ->
                                List.concat [
                                    typeArgs |> Array.toList |> List.map (sprintf "'%s[]")
                                    ["int"]

                                    [resName]
                                ]
                                
                    let costFunType = String.concat " -> " costFunArgs

                    let args = 
                        String.concat ", " [
                            yield "residualCount : int"
                            yield! typeArgs |> Array.mapi (fun i t -> sprintf "p%d : Block<'%s>"  i t)
                            if l then yield "loss : LossFunction"
                            yield sprintf "cost : %s" costFunType
                        ]
                        

                    
                    match k with
                        | Indexed ->
                            printfn "        member x.AddCostFunction(%s) =" args
                            
                            for i in 0 .. typeArgs.Length - 1 do
                                printfn "            let a%d = Array.zeroCreate p%d.Count" i i

                            printfn "            let read ="
                            printfn "                [|"
                            for i in 0 .. typeArgs.Length - 1 do
                                printfn "                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p%d.Read(o, ptr, d, a%d)" i i
                            printfn "                |]"

                            
                            printfn "            let blocks ="
                            printfn "                [|"
                            for i in 0 .. typeArgs.Length - 1 do
                                printfn "                    p%d :> Block" i
                            printfn "                |]"


                            let dim =
                                match t with
                                    | Scalar -> 1
                                    | Vec d -> d

                            let loss =
                                if l then "loss"
                                else "TrivialLoss"

                            printfn "            x.AddCostFunction(residualCount, %d, blocks, read, %s, fun i t ->" dim loss

                            let args = typeArgs |> Array.mapi (fun i t -> sprintf "a%d" i) |> String.concat " "

                            printfn "                let v = cost %s i" args
                            if dim = 1 then
                                printfn "                t.[0] <- v"
                            else
                                let dimNames = [| "X"; "Y"; "Z"; "W" |]
                                for i in 0 .. dim - 1 do
                                    printfn "                t.[%d] <- v.%s" i dimNames.[i]

                            printfn "            )"

                        | MutateArray ->
                            let name =
                                match t with
                                    | Scalar -> "Scalar"
                                    | Vec d -> sprintf "V%ds" d
                            printfn "        member x.AddCostFunction%s(%s) =" name args
                            
                            for i in 0 .. typeArgs.Length - 1 do
                                printfn "            let a%d = Array.zeroCreate p%d.Count" i i

                            printfn "            let read ="
                            printfn "                [|"
                            for i in 0 .. typeArgs.Length - 1 do
                                printfn "                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p%d.Read(o, ptr, d, a%d)" i i
                            printfn "                |]"

                            
                            printfn "            let blocks ="
                            printfn "                [|"
                            for i in 0 .. typeArgs.Length - 1 do
                                printfn "                    p%d :> Block" i
                            printfn "                |]"


                            let dim =
                                match t with
                                    | Scalar -> 1
                                    | Vec d -> d

                            let loss =
                                if l then "loss"
                                else "TrivialLoss"
                            
                            if dim > 1 then
                                printfn "            let myRes = Array.zeroCreate residualCount"

                            printfn "            x.AddCostFunction(%d * residualCount, blocks, read, %s, fun t ->" dim loss

                            let args = typeArgs |> Array.mapi (fun i t -> sprintf "a%d" i) |> String.concat " "

                            if dim = 1 then
                                printfn "                cost %s t" args
                            else
                                printfn "                cost %s myRes" args
                                printfn "                let mutable ri = 0"
                                printfn "                for i in 0 .. residualCount - 1 do"
                                
                                let dimNames = [| "X"; "Y"; "Z"; "W" |]
                                for i in 0 .. dim - 1 do
                                    printfn "                    t.[ri + %d] <- myRes.[i].%s" i dimNames.[i]
                                

                                printfn "                    ri <- ri + %d" dim

                            
                            printfn "            )"
                        
                        | ReturnArray -> 
                            printfn "        member x.AddCostFunction(%s) =" args
                            
                            for i in 0 .. typeArgs.Length - 1 do
                                printfn "            let a%d = Array.zeroCreate p%d.Count" i i

                            printfn "            let read ="
                            printfn "                [|"
                            for i in 0 .. typeArgs.Length - 1 do
                                printfn "                    fun (o : int) (d : bool) (ptr : nativeptr<float>) -> p%d.Read(o, ptr, d, a%d)" i i
                            printfn "                |]"

                            
                            printfn "            let blocks ="
                            printfn "                [|"
                            for i in 0 .. typeArgs.Length - 1 do
                                printfn "                    p%d :> Block" i
                            printfn "                |]"


                            let dim =
                                match t with
                                    | Scalar -> 1
                                    | Vec d -> d

                            let loss =
                                if l then "loss"
                                else "TrivialLoss"
                            
                            printfn "            x.AddCostFunction(%d * residualCount, blocks, read, %s, fun t ->" dim loss

                            let args = typeArgs |> Array.mapi (fun i t -> sprintf "a%d" i) |> String.concat " "

                            printfn "                let myRes = cost %s" args
                            if dim > 1 then 
                                printfn "                let mutable ri = 0"
                            printfn "                for i in 0 .. residualCount - 1 do"

                            if dim = 1 then
                                printfn "                    t.[i] <- myRes.[i]"
                            else
                                let dimNames = [| "X"; "Y"; "Z"; "W" |]
                                for i in 0 .. dim - 1 do
                                    printfn "                    t.[ri + %d] <- myRes.[i].%s" i dimNames.[i]
                                
                            if dim > 1 then 
                                printfn "                    ri <- ri + %d" dim

                            
                            printfn "            )"

    
    let str = res.ToString()

    System.IO.File.WriteAllText(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "ProblemExtensionsGenerated.fs"), str)

