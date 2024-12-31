namespace Day3


module Shared =

    open System
    let parseInt (s:string) =
        let mutable (result:int32) = 0
        if (s.Trim().Length = s.Length) then
            if (Int32.TryParse(s, &result)) then
                Some result
            else
                None
        else None

    let extractNextMul (textOfChars:string) startAt =
        let text = textOfChars.Substring(startAt)
        let instanceIndex = text.IndexOf("mul(")
        if ((instanceIndex >=0)) then
            // check if there is a comma in the next 4 characters
            let commaIndex = text.Substring(instanceIndex + 4, 4).IndexOf(",")
            if (commaIndex > 0) then
                // get the first number, if its parsable
                let startIndex = instanceIndex + 4
                let numcs = text.Substring(startIndex,commaIndex )
                match (parseInt numcs) with
                | Some num1 ->
                    // try and get the next number
                    let nextNumStartIndex = startIndex + commaIndex + 1
                    let endIndex = 
                        if (nextNumStartIndex + 4 <= text.Length - 1) then
                            nextNumStartIndex + 4
                        else
                            text.Length - 1
                    let endBraceIndex = (text.Substring(nextNumStartIndex, (endIndex - nextNumStartIndex + 1)).IndexOf(")"))
                    if (endBraceIndex >=0) then
                        let nextNumcs = text.Substring(nextNumStartIndex, endBraceIndex)
                        match (parseInt nextNumcs) with
                        | Some num2 ->
                            Some(num1*num2), (nextNumStartIndex + endBraceIndex + 1 + startAt), (instanceIndex + startAt)
                        | None ->
                            (None, endBraceIndex + startAt, -1)
                    else
                        (None, nextNumStartIndex + startAt, -1)

                | None ->
                    (None, startIndex + commaIndex + startAt, -1)

            else
                (None, (instanceIndex + 4 + startAt), -1)
        else
            (None, text.Length + startAt, -1)

    let rec extractAllMuls (text:string) startAt mulNums =
        if (startAt >= text.Length) then
            mulNums
        else
            let newMuls, newStartAt, mulPosIndex = extractNextMul text startAt
            match newMuls with
            | Some nm ->
                extractAllMuls text newStartAt (List.append mulNums [nm,mulPosIndex])
            | _ -> 
                extractAllMuls text newStartAt mulNums

module Part1 =

    open System.IO
    open Shared

    let solution file =

        let lines = File.ReadAllText file

        [|lines|]
        |> Array.fold (fun numsSoFar line  ->
            let nums = extractAllMuls line 0 List.empty
            List.append numsSoFar nums
        ) List.empty
        |> List.sumBy(fst)

module Part2 =

    open System
    open System.IO
    open Shared
    let rec getOccurrences (fragment:string) (text:string) startAt instances =

        if (startAt >= text.Length ) then
            instances
        else
            let fragLength = fragment.Length
            let subText = text.Substring(startAt)
            let nextIndex = subText.IndexOf(fragment)
            if (nextIndex >= 0) then
                let newInstances = (startAt + nextIndex)::instances
                getOccurrences fragment text (startAt + nextIndex + fragLength) newInstances
            else
                instances

    let getDos text =
        getOccurrences "do()" text 0 []
        
    let getDonts text =
        getOccurrences "don't()" text 0 []

    let getClosestLocation (lstOfLocations:int list) location =
        let _, lower = List.partition (fun loc -> loc> location) lstOfLocations
        if (lower.IsEmpty) then
            Int32.MinValue
        else
            lower.Head

    let checkIfAllowed (dosList:int list) (dontsList:int list) (mulLocation: (int32*int)) =
        let loc = snd(mulLocation)
        // check if the dos or don't is closest to the location
        //dos
        let closestDo = getClosestLocation dosList loc
        // donts
        let closestDont = getClosestLocation dontsList loc

        if (closestDo = closestDont) then
            true
        else
            closestDo > closestDont
    let solution file =



        let lines = File.ReadAllText file

        let dos = getDos lines
        let donts = getDonts lines

        let allowed = checkIfAllowed dos donts

        [|lines|]
        |> Array.fold (fun numsSoFar line  ->
            let nums = extractAllMuls line 0 List.empty
            List.append numsSoFar nums
        ) List.empty
        |> List.filter (allowed)
        |> List.sumBy(fst)


