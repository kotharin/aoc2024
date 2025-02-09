namespace Day10

module Part1 =

    open System.IO

    type TrailMap = {
        Map: Map<(int*int), int>
        TrailHeads: List<int*int>
        MaxCol: int
        MaxRow: int
    }
    with 
        static member parseRow row (line:string) (map:Map<(int*int), int>) (trailHeads:List<int*int>) =
        
            line.ToCharArray()
            |> Array.fold( fun (col, m, t) n ->
                let num = (int)n - 48
                let newMap = Map.add (col,row) num m
                let newTrailHeads =
                    if (num = 0) then
                        // Add to trail heads
                        (col, row)::t
                    else t
                (col+1, newMap, newTrailHeads)
            ) (0,map, trailHeads)
        
        static member parse (rows:string array) =
            let maxRow,trailMap, trailHeads =
                rows
                |> Array.fold( fun (row, m, t) line ->
                    let _,newMap, newT = TrailMap.parseRow row line m t 
                    (row+1, newMap, newT)
                ) (0,Map.empty, List.empty)
            { TrailMap.Map=trailMap
              TrailHeads = trailHeads
              MaxCol = (rows.[0].Length - 1)
              MaxRow = maxRow - 1 }


    let getAdjacentPositions maxRow maxCol (trailMap:Map<(int*int), int>) currRow currCol =
        [
            (currCol, currRow + 1);
            (currCol, currRow - 1);
            (currCol + 1, currRow);
            (currCol - 1, currRow);
        ]
        |> List.choose (fun (x,y) ->
            if ((x<=maxCol) && (x>=0) && (y <= maxRow) && (y >=0)) then
                // ensure that the height diff is 1
                let currentHeight = Map.find (currCol, currRow) trailMap
                let adjHeight = Map.find (x,y) trailMap
                if (adjHeight - currentHeight = 1) then
                    Some (x,y)
                else None
            else None
        )


    let rec traverse maxRow maxCol (positions:List<int*int>) (startingTrailHead:(int*int)) (trailMap:Map<(int*int), int>) (trailHeadData:Map<(int*int), Set<int*int>>) =
        
        match positions with
        | [] ->
            trailHeadData
        | head::tail ->
            let currCol, currRow = head
            // if the current head's height is 9, its the end of the trail
            let currentHeight = Map.find head trailMap
            let newTrailHeadData, newPositions, newStartingTrailHead  =
                if (currentHeight = 9) then
                    // Add this ot the trail heads data
                    let trailEnds = Map.tryFind startingTrailHead trailHeadData |> Option.defaultValue Set.empty
                    let newTrailEnds = Set.add head trailEnds
                    let newTHD = Map.add startingTrailHead newTrailEnds trailHeadData
                    newTHD, tail, startingTrailHead
                else
                    // if nto trail end, get adjacent nodes and add
                    let adjacentPositions = getAdjacentPositions maxRow maxCol trailMap currRow currCol
                    let newP = List.append adjacentPositions tail
                    let newSTH =
                        if (currentHeight = 0) then
                            head
                        else startingTrailHead
                    trailHeadData, newP, newSTH
            
            traverse maxRow maxCol newPositions newStartingTrailHead trailMap newTrailHeadData


    let solution file =

        let lines = File.ReadAllLines file

        let trailMap = TrailMap.parse lines

        let totalCount =
            traverse trailMap.MaxRow trailMap.MaxCol trailMap.TrailHeads trailMap.TrailHeads.Head trailMap.Map Map.empty
            |> Map.fold(fun state key v ->
                state + v.Count
            ) 0
        totalCount