namespace Day6

module Part1 =

    open System
    open System.IO

    type Direction =
        | North
        | East
        | South
        | West

    type LocationDetail = {
        X: int
        Y:int
        HasObstacle: bool
    }

    
    let loadMap (lines:string array) =

        let _, _ ,locationMap, startingPoint =
            lines
            |> Array.fold(fun (cx, cy, cmap, csp) line ->
                let locData = line.ToCharArray()
                let x,y, map, sp =
                    locData
                    |> Array.fold( fun (currX, currY, currMap, currSP) ld ->
                        let hasObstacle = (ld = '#')
                        let newSP =
                            if (ld = '^') then
                                Set.singleton (currX, currY)
                            else currSP
                        let newMap = Map.add (currX, currY) {X = currX; Y = currY; HasObstacle = hasObstacle } currMap
                        (currX + 1, currY, newMap, newSP)
                    )(0,cy,cmap,csp)
                x,y+1,map,sp
            ) (0,0,Map.empty, Set.empty)
        locationMap, startingPoint

    let getNewDirection direction =
        match direction with
        | North -> East
        | East -> South
        | South -> West
        | West -> North
    let getNextMoveInDirection (map:Map<(int*int), LocationDetail>) x y direction =
        match direction with 
        | North ->
            (x, y-1)
        | East ->
            (x+1, y)
        | South ->
            (x, y+1)
        | West ->
            (x-1, y)
    let rec traverse maxX maxY (map:Map<(int*int), LocationDetail>) x y direction visited =
        
        let nextPossibleX, nextPossibleY = getNextMoveInDirection map x y direction

        if ((nextPossibleX > maxX) || (nextPossibleY > maxY) || (nextPossibleX < 0) || (nextPossibleX < 0)) then
            // reached the end of the road. Exit!
            Set.count visited
        else
            // check if there is a block at that location
            let ld = Map.find (nextPossibleX, nextPossibleY) map
            if (ld.HasObstacle) then
                // turn and try that direction
                let newDirection = getNewDirection direction
                traverse maxX maxY map x y newDirection visited
            else
                // Add to visited set
                let newVisited = Set.add (nextPossibleX, nextPossibleY ) visited
                traverse maxX maxY map nextPossibleX nextPossibleY direction newVisited
        
    let traverseMap maxX maxY startingPoint (startingDirection:Direction) (map:Map<(int*int), LocationDetail>) =
        let startX, startY = startingPoint

        traverse maxX maxY map startX startY startingDirection (Set.singleton (startX, startY))
    let solution file =

        let lines = File.ReadAllLines file

        let locationMap, startingPoint = loadMap lines

        let maxY = lines.Length - 1
        let maxX = lines.[0].Length - 1

        let startX, startY = Set.minElement startingPoint

        let visits = traverseMap maxX maxY (startX,startY) North locationMap

        visits