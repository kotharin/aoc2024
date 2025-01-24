namespace Day7

module Part1 = 

    
    open System
    open System.IO

    type Equation = {
        Inputs: int64 array
        Output: int64
    } with
        static member parse(s:string) =
            let parts = s.Split([|':'|])

            let output = Int64.Parse( parts.[0].Trim())

            let inputs = parts.[1].Trim().Split([|' '|]) |> Array.map(int64)

            {Inputs=inputs; Output = output}

    let getBounds (nums:int64 array) =
        let lowerBound = Array.sum nums

        let upperBound =
            nums
            |> Array.fold(fun product n ->
                product * n
            ) 1L

        lowerBound, upperBound

    let rec check total (remainingTotal:int64) (remainingNumbers: int64 array) usedNumbers =
        if (remainingTotal = 0) then
            Some (total, usedNumbers)
        else
            if ((remainingNumbers.Length > 0) && (remainingTotal > 0)) then
                // get the last number from the end
                let rest, last = 
                    if remainingNumbers.Length = 1 then
                        Array.empty, remainingNumbers
                    else
                        Array.splitAt (remainingNumbers.Length - 1) remainingNumbers

                let newRemTotal, newOp =
                    if (remainingTotal%last.[0] = 0L) then
                        let nrt = 
                            if (remainingTotal/last.[0] = 1) then
                                0L
                            else remainingTotal/last.[0]
                        let nop = "M"
                        nrt, nop
                    else
                        let nrt = remainingTotal - last.[0]
                        let nop = "A"
                        nrt, nop
                let newUsedNumbers = 
                    if (remainingNumbers.Length = 1) then
                        last.[0].ToString() + usedNumbers
                    else
                        ":" + newOp + ":" +  last.[0].ToString() + ":" + usedNumbers

                check total newRemTotal rest newUsedNumbers
            else
                None

            // 
    let solution file = 

        let equations = 
            File.ReadAllLines file
            |> Array.map(Equation.parse)

        let inRangeEquations = 
            equations
            |> Array.fold (fun aeq eq ->
                let lb, ub = getBounds eq.Inputs
                if ((eq.Output >= lb) && (eq.Output <= ub)) then
                    Array.append aeq [|eq|]
                else
                    aeq
            ) Array.empty

        let x = 
            inRangeEquations
            |> Array.fold (fun validEq eq ->
                let veq = check eq.Output eq.Output eq.Inputs ""
                match veq with
                | Some (s) ->
                    Array.append validEq [|s|]
                | None ->
                    validEq
            ) Array.empty

        printfn "x:%A" x

        let sum = x |> Array.sumBy(fst)

        printfn "sum:%A" sum
        sum  