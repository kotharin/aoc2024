namespace Day1

module Part1 = 

    open System.IO
    open System
    let solution file =
        
        let lines = File.ReadAllLines file


        let xx =
            lines
            |> Array.map(fun line ->
                let vals = line.Split([|' '|])
                int(vals.[0].Trim()), int(vals.[3].Trim())
            ) 
            |> Array.unzip
            |> (fun (x, y) ->
                Array.sort x, Array.sort y
            )
            |> (fun (left, right) ->

                Array.map2 (fun x y -> abs(x - y)) left right            
            )
            |> Array.sum

        xx