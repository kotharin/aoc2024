namespace Day9

module Shared =
    let repeat (n:string) times =
        Array.init times (fun _ -> n)
        
module Part1 =
    open System.IO
    open Shared
    let parse (line:string) =
        let _,_,x =
            line.ToCharArray()
            |> Array.fold(fun (i, fi, state) num ->
                let times =  (int)num - 48
                if (i%2 = 0) then
                    let fileIds = repeat (fi.ToString()) times
                    let newState = Array.append state fileIds
                    (i+1), (fi+1), newState
                else
                    let gaps = repeat "." times
                    let newState = Array.append state gaps
                    (i+1), fi, newState
            ) (0,0,Array.empty)
        x


    let traverse (data:string array) =
        
        let rec fill l r (state:string array) (result:string array) =
            if (l<=r) then
                if (state.[l] <> ".") then
                    fill (l+1) r state (Array.append result [|state.[l]|])
                else
                    if (state.[r] <> ".") then
                        let newResult = (Array.append result [|state.[r]|])
                        state.[l] <- state.[r]
                        state.[r] <- "."
                        fill (l+1) (r - 1) state newResult
                    else
                        fill l (r-1) state result
            else
                state, result
        fill 0 (data.Length - 1) data Array.empty  
    let solution file =

        let lines = File.ReadAllLines file
        parse lines.[0]
        |> traverse
        |> fst
        |> Array.fold(fun (index, total) num -> 
                if (num = ".") then
                    (index+1L, total)
                else
                    let newTotal = total + (int64(num) * index)
                    (index+1L, newTotal)
            ) (0L,0L)
        |> snd

module Part2 =

    open System.IO
    open Shared
    let parse (line:string) =
        let x =
            line.ToCharArray()
            |> Array.fold(fun (i, fi, state, fiList, gapMap) num ->
                let times =  (int)num - 48
                if (i%2 = 0) then
                    let fileIds = repeat (fi.ToString()) times
                    let newState = Array.append state fileIds
                    let newfiList = List.append fiList [((state.Length),fileIds)]
                    (i+1), (fi+1), newState, newfiList, gapMap
                else
                    let gaps = repeat "." times
                    let newState = Array.append state gaps
                    let newGapMap = Map.add (state.Length) gaps.Length gapMap
                    (i+1), fi, newState, fiList, newGapMap
            ) (0,0,Array.empty, List.empty, Map.empty)
        x

    let traverse (d:string array) fidList gapMap  =

        let rec fill (data:string array) (fileIdList:List<int*string array>) (gapMap:Map<int,int>) =

            match fileIdList with
            | [] -> data
            | head::tail ->
                // check the length of the fileIds
                let fidLen = snd(head).Length
                let fidPos = fst head
                // see if there is a gap that matches that length
                let x = 
                    [0..fidPos]
                    |> List.tryPick(fun position ->
                        let gap = 
                            Map.tryFind position gapMap
                            |> Option.map(fun gapAtPosition -> 
                                if (gapAtPosition >= fidLen) then
                                    Some (position, fidLen)
                                else
                                    None
                            )
                            |> Option.defaultValue (Some(-1, -1))
                            |> Option.defaultValue  (-1, -1)
                        if (gap = (-1, -1)) then
                            None
                        else
                            Some gap
                    )
                // matchihng gap
                match x with
                | Some (pos, fl) ->
                    // update array
                    let fileId = (snd(head)).[0]
                    let fileIdStartPos = fst(head)
                    let newData =
                        [0..(fl - 1)]
                        |> List.fold(fun (state:string array) i ->
                            state.[pos + i] <- fileId
                            state.[fileIdStartPos + i] <- "."
                            state
                        ) data
                    // update the gap
                    let currentGap = Map.find pos gapMap
                    // remove current one
                    let ngm = Map.remove pos gapMap
                    // if gap is not completely used, add new gap
                    let newGapMap =
                        if (currentGap - fl) > 0 then
                            Map.add (pos + fl) (currentGap - fl) ngm
                        else ngm
                    fill newData tail newGapMap
                | None ->
                    // mpove to the next fileId
                    fill data tail gapMap

        fill d fidList gapMap
        

    let solution file =

        let lines = File.ReadAllLines file

        let _,_,data,fiList, gapMap = parse lines.[0]

        let sortedFilList = List.rev fiList

        traverse data sortedFilList gapMap
        |> Array.fold(fun (index, total) num -> 
            if (num = ".") then
                (index+1L, total)
            else
                let newTotal = total + (int64(num) * index)
                (index+1L, newTotal)
        ) (0L,0L)
        |> snd