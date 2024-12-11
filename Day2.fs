namespace Day2

module Part1 =
    open System
    open System.IO

    type Direction =
        | Upward
        | Downward
        | NoChange

    type Status =
        | Safe
        | UnSafe

    let diffAndDirection x y =
        let diff = x - y
        let direction =
            if (diff < 0) then
                Upward
            else if (diff > 0) then
                Downward
            else NoChange
        diff, direction

    let rowSafeOrUnsafe numbers =
        let rec safeOrUnsafe pairsList direction =
            match pairsList with
            | [] -> 
                Safe
            | hd::tl ->
                let x,y = hd
                let diff, dirxn = diffAndDirection x y
                let absDiff = abs(diff)
                match direction with
                | None ->
                    if (absDiff > 3) then
                        UnSafe
                    else
                        safeOrUnsafe tl (Some(dirxn))
                | Some dxn ->
                    if (dxn <> dirxn) then
                        UnSafe
                    else
                        let absDiff = abs(diff)
                        if ((absDiff < 1) || (absDiff > 3)) then
                            UnSafe
                        else
                            safeOrUnsafe tl (Some(dirxn))
        let pairs = Array.pairwise numbers |> List.ofArray
        safeOrUnsafe pairs None
    let solution file =

        let lines = File.ReadAllLines file

        lines
        |> Array.mapi(fun i line ->
            let nums =
                line.Split([|' '|])
                |> Array.map(int)
            let safety = rowSafeOrUnsafe nums
            //printfn "row:%i, Safety:%A" i safety
            safety
        )
        |> Array.filter (fun status -> status = Safe)
        |> Array.length
