namespace Day12

module Part1 = 

    open System.IO

    type GroupInfo = {
        Value: string
        Nodes: List<int*int>
    }

    let getAdjacentCells maxRows maxCols currRow currCol =
        [|
            currCol - 1, currRow;
            currCol + 1, currRow;
            currCol, currRow - 1;
            currCol, currRow + 1
        |]
        |> Array.choose(fun (x,y) ->
            if ((x >= 0) && (x <= maxCols) && (y >= 0) && (y <= maxRows)) then
                Some (x, y)
            else None
        )
    let getPerimeterForCell maxRows maxCols (map:Map<(int*int), string>) currCol currRow =
        // get the current cell value
        let currentCellValue = Map.find (currCol, currRow) map
        // get the viable adjacent cells
        let adjCells = getAdjacentCells maxRows maxCols currRow currCol
        // check how many have the same value as the current cell.
        let sameValue =
            adjCells
            |> Array.choose(fun (x,y) ->
                let cellValue = Map.find (x,y) map
                if (cellValue = currentCellValue) then
                    Some (x,y)
                else None
            )
            |> Array.length
        // For perimeter, subtract the number of cells with te same value rom 4 (max) to get the perimeter for that cell
        4 - sameValue

    let rec traverse maxRows maxCols (valueMap:Map<(int*int), string>) (map:Map<(int*int), (string*bool)>) (node:(int*int)) (group: GroupInfo) count =
        // get current node value
        let nodeVal, visited = Map.find node map
        let currCol, currRow = node
        if ((nodeVal <> group.Value) || (visited)) then
            map, count, group
        else
            // mark this cell as visited
            let newMap = Map.add node (nodeVal, true) map
            let newVisitedCells  = List.append group.Nodes [node]
            let newGroup =  {group with Nodes = newVisitedCells}
            // get adjacent cells
            getAdjacentCells maxRows maxCols currRow currCol
            |> Array.fold(fun (m, c, g) cell ->
                let nm, nc, ng = traverse maxRows maxCols valueMap m cell g (c + 1)
                nm,nc,ng
            ) (newMap, count, newGroup)
    let solution file =
        let lines = File.ReadAllLines file

        let maxRows = lines.Length - 1
        let maxCols = lines.[0].Length - 1

        let _, map, travMap =
            lines
            |> Array.fold (fun (currY, state, traversalMap) line ->
                let _, nmap, txmap =
                    line.ToCharArray()
                    |> Array.fold(fun (currX, layoutMap, tmap) data ->
                        let newMap =  Map.add (currX, currY) (data.ToString()) layoutMap
                        let newTraversalMap = Map.add (currX, currY) (data.ToString(), false) tmap
                        (currX+1), newMap, newTraversalMap
                    ) (0,state, traversalMap)
                (currY + 1), nmap, txmap
            ) (0,Map.empty, Map.empty)

        let _, listOfVegs =
            travMap
            |> Map.fold(fun (tmap, groups) k v ->
                let ns, _ = v
                let groupInfo = {GroupInfo.Nodes = List.empty; Value = ns}
                let newMap, newCount, newGroup = traverse maxRows maxCols map tmap k groupInfo 0
                let newGroups = 
                    if newGroup.Nodes.Length > 0 then
                        List.append groups [newGroup]
                    else groups
                newMap, newGroups
            ) (travMap, List.empty)


        // compute pricinf for list of vegs
        let totalCost =
            listOfVegs
            |> List.map(fun grp ->
                let groupPerimeter =
                    grp.Nodes
                    |> List.fold(fun state node ->
                        let currCol, currRow = node
                        let perimeter = getPerimeterForCell maxRows maxCols map currCol currRow
                        state + perimeter
                    ) 0
                (grp.Value, (groupPerimeter*grp.Nodes.Length))
            )
            |> List.sumBy snd 

        totalCost