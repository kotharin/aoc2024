namespace Day5


module Shared = 

    open System
    let parsePageOrdering (ordering:Map<int, Set<int>>) (line:string) =
        // parse the line
        let nums = line.Split([|'|'|])
        let shouldComeBefore, currentNum = int(nums.[0]), int(nums.[1])

        // get existing set from map
        let currentNumOrdering =
            Map.tryFind currentNum ordering
            |> Option.defaultValue Set.empty
        // Add to the ordering set
        let newSet = Set.add shouldComeBefore currentNumOrdering
        Map.add currentNum newSet ordering


    let parseUpdates (updates:List<int array>) (line:string) =
        // parse line into numbers
        let nums =
            line.Split([|','|])
            |> Array.map(int)

        // add to the list
        List.append updates [nums]

    let getMiddleNumber (line:int array) =
        if (line.Length > 0) then
            let index = (int)(Math.Floor((decimal)line.Length/(2.0m)))
            line.[index]
        else
            0

module Part1 = 
    open System.IO
    open Shared
    let validateLine (ordering:Map<int, Set<int>>) (line:int array) =
        let startingSet = Set.ofArray line
        // check each number. If the numbers that remain after removing the current
        // number, has an overlap with the page ordering for the current number, then
        // the line is invalid
        let lineIsValid, _ =
            line
            |> Array.fold( fun (isValid, ss) num ->
                if (isValid) then
                    // remove the current number form the set to get the remining numbers
                    // in the line that come AFTER the current num.
                    let newSS = Set.remove num ss
                    // get the page ordering for the current number
                    let po = Map.tryFind num ordering |> Option.defaultValue Set.empty
                    // Check if there is any overlap with the page order set for the current num
                    let overlap = Set.intersect po newSS
                    let newIsValid = isValid && (Set.count overlap = 0)
                    (newIsValid, newSS)
                else
                    // don't need to look at remaning numbers as the line is invalid already
                    isValid, ss
            ) (true, startingSet)

        lineIsValid
    let solution file =

        let lines = File.ReadAllLines file

        let _, pageOrdering, pageUpdates = 
            lines
            |> Array.fold(fun (isOrdering, ordering, updates) line ->
                if (isOrdering) then
                    // check if the line is empty
                    if (line.Trim().Length = 0) then
                        // move to next line
                        (false, ordering, updates)
                    else
                        let newOrdering = parsePageOrdering ordering line
                        (true, newOrdering, updates)
                else
                    let newUpdates = parseUpdates updates line
                    (false, ordering, newUpdates)
            ) (true, Map.empty, List.empty)

        pageUpdates
        |> List.filter (fun pu ->
            validateLine pageOrdering pu
        )
        |> List.map(getMiddleNumber)
        |> List.sum
