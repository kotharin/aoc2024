namespace Day11

module Part1 =

    open System.IO
    open System.Numerics

    let applyRule (num:int64) =
        let numLength = num.ToString().Length
        if (num = 0L) then
            [|1L|]
        else
            if (numLength%2 = 0) then
                let ns = num.ToString()
                let x = (int)(ns.Substring(0, ns.Length/2))
                let y = (int)(ns.Substring(ns.Length/2))
                [|x;y|]
            else
                [|num*2024L|]

    let traverse (nums:int64 array) =

        
        let x =
            nums
            |> Array.map(applyRule)
            |> Array.concat
        x
    let solution file = 

        let data = 
            File.ReadAllText file
            |> (fun txt -> txt.Split([|' '|]))
            |> Array.map int64


        let x =
            [0..24]
            |> List.fold(fun state _ ->
                let nd = traverse state
                //printfn "s:%A" nd
                nd
            ) data
        x.Length