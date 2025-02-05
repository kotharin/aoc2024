namespace Day9

module Part1 =

    open System.IO

    let repeat (n:string) times =
        Array.init times (fun _ -> n)
        
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
    