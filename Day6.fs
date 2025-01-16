namespace Day6

module Shared =

    type Direction =
        | North
        | East
        | South
        | West


    type LocationDetail = {
        X: int
        Y:int
        HasObstacle: bool
        VisitedInfo: Direction option
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
                        let newMap = Map.add (currX, currY) {X = currX; Y = currY; HasObstacle = hasObstacle; VisitedInfo = None } currMap
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


module Part1 =
    open System
    open System.IO
    open Shared

    let rec traverse maxX maxY (map:Map<(int*int), LocationDetail>) x y direction visited =
        
        let nextPossibleX, nextPossibleY = getNextMoveInDirection map x y direction

        if ((nextPossibleX > maxX) || (nextPossibleY > maxY) || (nextPossibleX < 0) || (nextPossibleX < 0)) then
            // reached the end of the road. Exit!
            visited
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

        Set.count visits

module Part2 =
    open System.IO
    open Shared

    let rec traverseForRecordingVisits maxX maxY (map:Map<(int*int), LocationDetail>) x y direction visited =
        
        let nextPossibleX, nextPossibleY = getNextMoveInDirection map x y direction

        if ((nextPossibleX > maxX) || (nextPossibleY > maxY) || (nextPossibleX < 0) || (nextPossibleY < 0)) then
            // reached the end of the road. Exit!
            visited
        else
            // check if there is a block at that location
            let ld = Map.find (nextPossibleX, nextPossibleY) map
            if (ld.HasObstacle) then
                // turn and try that direction
                let newDirection = getNewDirection direction
                traverseForRecordingVisits maxX maxY map x y newDirection visited
            else
                // Add to visited set
                let newVisited = Set.add (nextPossibleX, nextPossibleY ) visited
                // Update the map to record the visit with the right direction
                let oldLD = Map.find (nextPossibleX, nextPossibleY) map
                let newMap = Map.add (nextPossibleX, nextPossibleY) {oldLD with VisitedInfo = Some(direction)} map
                traverseForRecordingVisits maxX maxY newMap nextPossibleX nextPossibleY direction newVisited

    let rec traverseLoops maxX maxY (map:Map<(int*int), LocationDetail>) x y direction visited =
        
        let nextPossibleX, nextPossibleY = getNextMoveInDirection map x y direction
        //let newMap = Map.add (x,y) {X=x; Y = y; HasObstacle = oldLD.HasObstacle; VisitedInfo = Some(direction)} map

        if ((nextPossibleX > maxX) || (nextPossibleY > maxY) || (nextPossibleX < 0) || (nextPossibleY < 0)) then
            // reached the end of the road. Exit!
            false, visited, map
        else
            // check if there is a block at that location
            let ld = Map.find (nextPossibleX, nextPossibleY) map
            if (ld.HasObstacle) then
                // turn and try that direction
                let newDirection = getNewDirection direction
                traverseLoops maxX maxY map x y newDirection visited
            else
                // if we reach a spot that has already been traversed and in the same direction, we are in a loop
                let newVisited = Set.add (nextPossibleX, nextPossibleY ) visited
                // Update the map to record the visit with the right direction
                let oldLD = Map.find (nextPossibleX, nextPossibleY) map
                let newMap = Map.add (nextPossibleX, nextPossibleY ) {oldLD with VisitedInfo = Some(direction)} map
                match (ld.VisitedInfo) with
                | Some d ->
                    if (d = direction) then
                        true, newVisited, map
                    else
                        traverseLoops maxX maxY newMap nextPossibleX nextPossibleY direction newVisited
                | None ->
                    traverseLoops maxX maxY newMap nextPossibleX nextPossibleY direction newVisited
    let findLoops maxX maxY (map:Map<(int*int), LocationDetail>) startX startY  (visitedPoints: (int*int) list) =

        visitedPoints
        |> List.map(fun (x,y) ->
            // Add an obstacle at this point and see if the traversal loops
            let oldLD = Map.find (x,y) map
            let newLD = {oldLD with HasObstacle=true}

            let newLocationMap = Map.add (x,y) newLD map
            let itLoops, _, _ = traverseLoops maxX maxY newLocationMap startX startY North Set.empty
            (x,y), itLoops
        )
    let solution file =

        let lines = File.ReadAllLines file

        let locationMap, startingPoint = loadMap lines

        let maxY = lines.Length - 1
        let maxX = lines.[0].Length - 1

        let startX, startY = Set.minElement startingPoint

        let visitedPoints = traverseForRecordingVisits maxX maxY  locationMap startX startY North  startingPoint

        // get the traversed map and add blocks on the path traversed to see if it causes a loop
        let looped =
            findLoops maxX maxY locationMap startX startY (List.ofSeq visitedPoints)
            |> List.filter(fun ((x,y), isLoop) -> 
                isLoop
            )
            |> List.length

        looped
        