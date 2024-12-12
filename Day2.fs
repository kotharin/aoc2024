namespace Day2

module Shared =
    type Direction =
        | Upward
        | Downward
        | NoChange

    type Status =
        | Safe
        | UnSafe


module Part1 =
    open System
    open System.IO
    open Shared

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

module Part2 =

    open System
    open System.IO
    open Shared
    type RowInfo = {
        Numbers: int array
        UpTransitions: List<(int*int)>
        DownTransitions: List<(int*int)>
        NoTransitions: List<(int*int)>
        DiffViolations: List<(int*int)>
    }

    let transformRow numbers =

        let rec transform row index  upt downt notsn diffV=
            if (index = (Array.length row) ) then
                // fill out the row info object
                {
                    Numbers = row;
                    UpTransitions = upt;
                    DownTransitions = downt;
                    NoTransitions = notsn
                    DiffViolations = diffV
                }
            else

                let x,y = row.[index-1], row.[index]

                // get the diff
                let diff = abs(x - y)
                let newDiffV =
                    if ((diff < 1) ||(diff > 3)) then   
                        List.append diffV [(index-1, index)]
                    else diffV

                // get the transitions
                let newUpT =
                    if (y > x) then
                        List.append upt [(index-1, index)]
                    else upt

                let newDnT =
                    if (x > y) then
                        List.append downt [(index-1, index)]
                    else downt

                let newNoT =
                    if (diff = 0) then
                        List.append notsn [(index-1, index)]
                    else notsn
    
                // move to next index
                transform row (index+1) newUpT newDnT newNoT newDiffV

        transform numbers 1 List.empty List.Empty List.empty List.Empty
    
    let hasAnyViolation (rowInfo:RowInfo) =
        if ((rowInfo.DiffViolations.Length > 0) || (rowInfo.NoTransitions.Length > 0) || ((rowInfo.DownTransitions.Length > 0) && (rowInfo.UpTransitions.Length > 0) )) then
            true
        else false

    (*
    let has2orMoreViolations (rowInfo:RowInfo) =
        if (rowInfo.NoTransitions.Length > 0) then
    *)
    let solution file =

        let lines = File.ReadAllLines file

        let rowsInfo =
            lines
            |> Array.mapi(fun i line ->
                let nums =
                    line.Split([|' '|])
                    |> Array.map(int)
                nums
            )
            |> Array.map(transformRow)

        let safeRows =
            rowsInfo
            |> Array.filter(fun rowInfo -> not (hasAnyViolation rowInfo))

        safeRows.Length
        
