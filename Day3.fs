namespace Day3

module Part1 =

    open System
    open System.IO


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
                            Some(num1*num2), nextNumStartIndex + endBraceIndex + 1 + startAt
                        | None ->
                            (None, endBraceIndex + startAt)
                    else
                        (None, nextNumStartIndex + startAt )

                | None ->
                    (None, startIndex + commaIndex + startAt)

            else
                (None, (instanceIndex + 4 + startAt))
        else
            (None, text.Length + startAt)

    let rec extractAllMuls (text:string) startAt mulNums =
        if (startAt >= text.Length) then
            mulNums
        else
            let newMuls, newStartAt = extractNextMul text startAt
            match newMuls with
            | Some nm ->
                extractAllMuls text newStartAt (List.append mulNums [nm])
            | _ -> 
                extractAllMuls text newStartAt mulNums
    let solution file =

        let lines = File.ReadAllLines file

        lines
        |> Array.fold (fun numsSoFar line  ->
            let nums = extractAllMuls line 0 List.empty
            List.append numsSoFar nums
        ) List.empty
        |> List.sum

