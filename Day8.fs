namespace Day8

module Part1 = 

    open System.IO

    type Location = {
        X: int
        Y: int
    }

    let isValidLocation maxRows maxCols (x,y) =
        if ((x >=0) && (x <=maxCols) && (y>=0)&&(y<=maxRows)) then
            true
        else false

    let possibleAntinodes maxRows maxCols (location1: Location) (location2:Location) =

        // get the vertical and hosrizontal diff between the points
        let hdiff = location1.X - location2.X
        let vdiff = location1.Y - location2.Y

        [location1.X + hdiff, location1.Y + vdiff;
          location2.X + (-1*hdiff), location2.Y + (-1*vdiff)]
        |> List.map(fun (x,y) ->
            if isValidLocation maxRows maxCols (x,y) then
                Some ({Location.X =x; Location.Y = y})
            else None
        )
        |> List.choose id

    let getAllPosibleAntinodes maxRows maxCols (locs:List<Location>) =
        let rec allNodes (locations:Location list) (possibleLocations:Location list) =
            let next, rest = locations.Head, locations.Tail
            match rest with 
            | [] -> possibleLocations
            | _ ->
                let newPoss = 
                    rest
                    |> List.fold(fun pls loc2 ->
                        let plsForPair = 
                            possibleAntinodes maxRows maxCols next loc2
                        List.append pls plsForPair
                    ) List.empty
                    |> List.append possibleLocations
                allNodes rest newPoss
        
        allNodes locs List.empty


    let parse (row:int) (line:string) (existingData:Map<char, List<Location>>) =
        line.ToCharArray()
        |> Array.fold(fun (col, state) c ->
            if (c <> '.') then
                let vals = Map.tryFind c state |> Option.defaultValue List.empty
                let newVals = List.append vals [{Location.X = col; Location.Y = row}]
                (col+1), (Map.add c newVals state)
            else
                (col+1), state

        ) (0, existingData)
    let solution file =

        let lines = File.ReadAllLines file

        let maxCols = lines.[0].Length - 1
        let maxRows = lines.Length - 1

        let _, antennas =
            lines
            |> Array.fold(fun (row, state) line ->
                let _, newMap = parse row line state
                (row+1), newMap
            ) (0, Map.empty)
        
        let types = antennas.Keys |> List.ofSeq


        types
        |> List.map(fun atype ->
            // get antenna locations of this type
            let currentAntennas = Map.find atype antennas
            // get the possible locations for this list
            getAllPosibleAntinodes maxRows maxCols currentAntennas
        )
        |> List.concat
        |> Set.ofList
        |> Set.count
