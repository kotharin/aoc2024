namespace Day4

module Part1 =

    open System
    open System.IO

    // Check if XMAS is possible to the  horizontal left (column values decreasing)
    let checkLeft (grid:Map<(int*int), string>) position =
        // check if the max left (currentCol - 3) is within bounds
        let currentRow, currentCol = position
        if (currentCol - 3) >= 0 then
            ((grid.[(currentRow, currentCol - 1)] = "M") && (grid.[(currentRow, currentCol - 2)] = "A") && (grid.[(currentRow, currentCol - 3)] = "S"))
        else
            false

    let checkRight maxCols (grid:Map<(int*int), string>) position =
        // check if the max right (currentCol + 3) is within bounds
        let currentRow, currentCol = position
        if (currentCol + 3) <= maxCols then
            ((grid.[(currentRow, currentCol + 1)] = "M") && (grid.[(currentRow, currentCol + 2)] = "A") && (grid.[(currentRow, currentCol + 3)] = "S"))
        else
            false

    let checkTop  (grid:Map<(int*int), string>) position =
        // check if the max top (currentRow - 3) is within bounds
        let currentRow, currentCol = position
        if (currentRow - 3) >= 0 then
            ((grid.[(currentRow - 1, currentCol)] = "M") && (grid.[(currentRow - 2, currentCol)] = "A") && (grid.[(currentRow - 3, currentCol)] = "S"))
        else
            false

    let checkBottom maxRows  (grid:Map<(int*int), string>) position =
        // check if the max bottom (currentRow + 3) is within bounds
        let currentRow, currentCol = position
        if (currentRow + 3) <= maxRows then
            ((grid.[(currentRow + 1, currentCol)] = "M") && (grid.[(currentRow + 2, currentCol)] = "A") && (grid.[(currentRow + 3, currentCol)] = "S"))
        else
            false

    let checkDiagLeftUp (grid:Map<(int*int), string>) position =
        // check if it falls outside max rows/columns allowed
        let currentRow, currentCol = position
        if ((currentCol - 3 >= 0) && (currentRow - 3 >=0)) then
            ((grid.[(currentRow - 1, currentCol - 1)] = "M") && (grid.[(currentRow - 2, currentCol - 2)] = "A") && (grid.[(currentRow - 3, currentCol - 3)] = "S"))
        else
            false

    let checkDiagRightUp maxCols (grid:Map<(int*int), string>) position =
        // check if it falls outside max rows/columns allowed
        let currentRow, currentCol = position
        if ((currentCol + 3 <= maxCols) && (currentRow - 3 >=0)) then
            ((grid.[(currentRow - 1, currentCol + 1)] = "M") && (grid.[(currentRow - 2, currentCol + 2)] = "A") && (grid.[(currentRow - 3, currentCol + 3)] = "S"))
        else
            false

    let checkDiagLeftDown maxRows (grid:Map<(int*int), string>) position =
        // check if it falls outside max rows/columns allowed
        let currentRow, currentCol = position
        if ((currentCol - 3 >= 0) && (currentRow + 3 <= maxRows)) then
            ((grid.[(currentRow + 1, currentCol - 1)] = "M") && (grid.[(currentRow + 2, currentCol - 2)] = "A") && (grid.[(currentRow + 3, currentCol - 3)] = "S"))
        else
            false

    let checkDiagRightDown maxRows maxCols (grid:Map<(int*int), string>) position =
        // check if it falls outside max rows/columns allowed
        let currentRow, currentCol = position
        if ((currentCol + 3 <= maxCols) && (currentRow + 3 <= maxRows)) then
            ((grid.[(currentRow + 1, currentCol + 1)] = "M") && (grid.[(currentRow + 2, currentCol + 2)] = "A") && (grid.[(currentRow + 3, currentCol + 3)] = "S"))
        else
            false

    let solution file =
        
        let lines = File.ReadAllLines file

        let rowCounter, positionMap, allXPositions =
            lines
            |> Array.fold (fun (curRow, curMap, curXList) line ->
            
                let _, newGrid, newXs =
                    line.ToCharArray()
                    |> Array.fold(fun (curCol, cMap, cxList) currentVal ->
                        let newMap = Map.add (curRow, curCol) (currentVal.ToString()) cMap
                        let newXList =
                            if (currentVal = 'X') then
                                (curRow, curCol) :: cxList
                            else
                                cxList
                        (curCol + 1, newMap, newXList)
                    ) (0, curMap, curXList)
                (curRow + 1, newGrid, newXs)
            ) (0, Map.empty, List.empty)
        

        let maxRows = rowCounter - 1

        let maxCols = lines.[0].Length - 1

        [|
            checkLeft positionMap;
            checkRight maxCols positionMap;
            checkTop positionMap;
            checkBottom maxRows positionMap;
            checkDiagLeftUp positionMap;
            checkDiagRightUp maxCols positionMap;
            checkDiagLeftDown maxRows positionMap;
            checkDiagRightDown maxRows maxCols positionMap

        |]
        |> Array.map(fun fn ->
            List.filter fn allXPositions
            |> List.length
        )
        |> Array.sum

module Part2 = 
    open System.IO

    let checkDiagRightDown maxRows maxCols (grid:Map<(int*int), string>) position =
        // check if it falls outside max rows/columns allowed
        let currentRow, currentCol = position

        if ((currentRow + 2 <= maxRows)&& (currentCol + 2 <= maxCols)) then
            if ((grid.[(currentRow + 1, currentCol + 1)] = "A") && (grid.[(currentRow + 2, currentCol + 2)] = "S")) then
                Some((currentRow + 1, currentCol + 1))
            else
                None
        else
            None

    let checkDiagLeftUp  (grid:Map<(int*int), string>) position =
        // check if it falls outside max rows/columns allowed
        let currentRow, currentCol = position

        if ((currentRow - 2 >= 0) && (currentCol - 2 >= 0)) then
            if ((grid.[(currentRow - 1, currentCol - 1)] = "A") && (grid.[(currentRow - 2, currentCol - 2)] = "S")) then
                Some((currentRow - 1, currentCol - 1))
            else
                None
        else
            None

    let checkDiagLeftDown maxRows (grid:Map<(int*int), string>) position =
        // check if it falls outside max rows/columns allowed
        let currentRow, currentCol = position

        if ((currentRow + 2 <= maxRows)&& (currentCol - 2 >= 0)) then
            if ((grid.[(currentRow + 1, currentCol - 1)] = "A") && (grid.[(currentRow + 2, currentCol - 2)] = "S")) then
                Some((currentRow + 1, currentCol - 1))
            else
                None
        else
            None

    let checkDiagRightUp maxCols (grid:Map<(int*int), string>) position =
        // check if it falls outside max rows/columns allowed
        let currentRow, currentCol = position

        if ((currentRow - 2 >= 0) && (currentCol + 2 <= maxCols)) then
            if ((grid.[(currentRow - 1, currentCol + 1)] = "A") && (grid.[(currentRow - 2, currentCol + 2)] = "S")) then
                Some((currentRow - 1, currentCol + 1))
            else
                None
        else
            None

    let solution file =

        let lines = File.ReadAllLines file

        let rowCounter, positionMap, allMPositions =
            lines
            |> Array.fold (fun (curRow, curMap, curXList) line ->
            
                let _, newGrid, newXs =
                    line.ToCharArray()
                    |> Array.fold(fun (curCol, cMap, cxList) currentVal ->
                        let newMap = Map.add (curRow, curCol) (currentVal.ToString()) cMap
                        let newXList =
                            if (currentVal = 'M') then
                                (curRow, curCol) :: cxList
                            else
                                cxList
                        (curCol + 1, newMap, newXList)
                    ) (0, curMap, curXList)
                (curRow + 1, newGrid, newXs)
            ) (0, Map.empty, List.empty)
        

        let maxRows = rowCounter - 1

        let maxCols = lines.[0].Length - 1

        let x = 
            [|
                checkDiagLeftUp positionMap;
                checkDiagRightDown maxRows maxCols positionMap
            |]
            |> Array.map(fun fn ->
                List.map fn allMPositions
                |> List.choose id
            )
            |> List.concat
            |> Set.ofList

        let y = 
            [|
                checkDiagLeftDown maxRows positionMap;
                checkDiagRightUp  maxCols positionMap
            |]
            |> Array.map(fun fn ->
                List.map fn allMPositions
                |> List.choose id
            )
            |> List.concat
            |> Set.ofList
        
        Set.intersect x y
        |> Set.count

