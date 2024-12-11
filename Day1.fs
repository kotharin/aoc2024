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

module Part2 =

    open System
    open System.IO

    let solution file =
        let lines = File.ReadAllLines file

        let left, right =
            lines
            |> Array.map(fun line ->
                let vals = line.Split([|' '|])
                int(vals.[0].Trim()), int(vals.[3].Trim())
            ) 
            |> Array.unzip

        let rightGroups =
            right
            |> Array.fold(fun map num ->
                // check if the number exists inthe map, if so increment count
                let currentCount =
                    Map.tryFind num map
                    |> Option.defaultValue 0
                Map.add num (currentCount + 1) map
            ) Map.empty

        let similarityScore =
            left
            |> Array.map(fun leftNum ->
                // check if the number exists in the rightGroups
                let occurence =
                    Map.tryFind leftNum rightGroups
                    |> Option.defaultValue 0
                leftNum, (leftNum * occurence)
            )
            |> Array.sumBy snd
        similarityScore