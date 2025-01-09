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

module Part2 =

    open System.IO
    open Shared


    let parsePageOrdering (ordering:Map<int, Set<int>>) (invertedOrdering:Map<int, Set<int>>) (line:string) =
        // parse the line
        let nums = line.Split([|'|'|])
        let numA, numB = int(nums.[0]), int(nums.[1])

        // get existing set from map
        let currentNumOrdering =
            Map.tryFind numB ordering
            |> Option.defaultValue Set.empty
        // Add to the ordering set
        let newSet = Set.add numA currentNumOrdering
        let newOrdering = Map.add numB newSet ordering

        // get existing set for invesrted map
        let currentInvertedOrdering =
            Map.tryFind numA invertedOrdering
            |> Option.defaultValue Set.empty

        // Add to the invertedOrdering
        let newInvertedSet = Set.add numB currentInvertedOrdering
        let newInvertedOrdering = Map.add numA newInvertedSet invertedOrdering

        newOrdering, newInvertedOrdering


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

    let rec insertAtRightPosition (ordering:Map<int, Set<int>>)  (unprocessed: Set<int>) (rightOrder:int array) =

        if (unprocessed.Count > 0) then
            // get the ordering data for each number
            // the set will contain numbers that this number should preceed
            // remove ANY number from each set that isn't in this number line
            let mapO, setU, arrRO = 
                unprocessed
                |> Set.fold(fun (relevantOrdering, unp,  ro) num ->
                    let numbersAfter = Map.tryFind num ordering |> Option.defaultValue Set.empty

                    // remove numbers that are not in the list
                    // and remove ones that are already in the right order
                    let relevantOrderingForNum = 
                        Set.difference
                            (Set.intersect numbersAfter unp)
                            (Set.ofArray ro)    
                    let newO, newUP, newRO =
                        if (relevantOrderingForNum.Count = 0) then
                            // add to no dependency set (should become part of right order)
                            let x = Array.append [|num|] ro
                            let y = Set.remove num unp
                            let z = Map.remove num relevantOrdering
                            z, y,x
                        else
                            let z = Map.add num relevantOrderingForNum relevantOrdering
                            z,unp, ro

                    newO, newUP, newRO

                )(ordering, unprocessed, rightOrder)
            insertAtRightPosition mapO setU arrRO 
        else
            rightOrder

        
    let correctInvalidLine (ordering:Map<int, Set<int>>) (line:int array) =
        insertAtRightPosition ordering (Set.ofArray line) Array.empty

    let solution file =
        let lines = File.ReadAllLines file

        let _, pageOrdering, pageInvertedOrdering, pageUpdates = 
            lines
            |> Array.fold(fun (isOrdering, ordering, invertedOrdering, updates) line ->
                if (isOrdering) then
                    // check if the line is empty
                    if (line.Trim().Length = 0) then
                        // move to next line
                        (false, ordering,invertedOrdering, updates)
                    else
                        let newOrdering, newInvertedOrdering = parsePageOrdering ordering invertedOrdering line
                        (true, newOrdering,newInvertedOrdering, updates)
                else
                    let newUpdates = parseUpdates updates line
                    (false, ordering, invertedOrdering, newUpdates)
            ) (true, Map.empty, Map.empty, List.empty)

        let sum =
            pageUpdates
            |> List.filter (fun pu ->
                not (validateLine pageOrdering pu)
            )
            |> List.map(fun line ->
                correctInvalidLine pageInvertedOrdering line
            )
            |> List.map getMiddleNumber
            |> List.sum

        sum