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

    let rec check (total:int64) (numbers:int64 array) =

        if (numbers.Length = 1) then
            total = numbers.[0]
        else
            let last = Array.last numbers
            let rest = 
                if (numbers.Length > 1) then
                    Array.sub numbers 0 (numbers.Length - 1)
                else Array.empty
            if (total > 0) then
                (check (total - last) rest) || ((total%last = 0) && (check (total/last) rest))
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