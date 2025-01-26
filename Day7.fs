namespace Day7

module Shared = 

    
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

module Part1 =

    open System.IO
    open Shared
    let rec check (total:int64) (numbers:int64 array) =

        if (numbers.Length = 1) then
            total = numbers.[0]
        else
            let first = Array.head numbers
            let second = numbers.[1]
            let rest = 
                if (numbers.Length > 2) then
                    Array.sub numbers 2 (numbers.Length - 2)
                else Array.empty
            if (check total (Array.append [|first + second|] rest)) then
                true
            elif (check total (Array.append [|first * second|] rest) ) then
                true
            else false

    let solution file = 

        let equations = 
            File.ReadAllLines file
            |> Array.map(Equation.parse)
        
        let sum = 
            equations
            |> Array.fold (fun veq eq ->
                if check eq.Output eq.Inputs then
                    Array.append veq [|(eq.Output)|]
                else
                    veq
            ) Array.empty
            |> Array.sum
        

        //printfn "sum:%A" sum
        //2299702440363
        //2299720547558
        //2299996598890L - correct
        //
        // 2299720547558

        //2299978491192L
        sum

module Part2 =

    open System.IO
    open Shared

    let rec check (total:int64) (numbers:int64 array) =

        if (numbers.Length = 1) then
            total = numbers.[0]
        else
            let first = Array.head numbers
            let second = numbers.[1]
            let combined = int64 (first.ToString() + second.ToString())
            let rest = 
                if (numbers.Length > 2) then
                    Array.sub numbers 2 (numbers.Length - 2)
                else Array.empty
            if (check total (Array.append [|first + second|] rest)) then
                true
            elif (check total (Array.append [|first * second|] rest) ) then
                true
            elif (check total (Array.append [|combined|] rest)) then
                true
            else false

    let solution file =

        let equations = 
            File.ReadAllLines file
            |> Array.map(Equation.parse)

        let sum = 
            equations
            |> Array.fold (fun veq eq ->
                if check eq.Output eq.Inputs then
                    Array.append veq [|(eq.Output)|]
                else
                    veq
            ) Array.empty
            |> Array.sum

        sum