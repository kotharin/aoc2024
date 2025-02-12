module Tests

    open Xunit

    [<Fact>]
    let ``Day1 Part1`` () =
        let answer = Day1.Part1.solution "Day1-1.txt"

        Assert.Equal(2375403, answer)
        // 23082277

    [<Fact>]
    let ``Day1 Part2`` () =
        let answer = Day1.Part2.solution "Day1-1.txt"

        Assert.Equal(23082277, answer)

    [<Fact>]
    let ``Day2 Part1`` () =
        let answer = Day2.Part1.solution "Day2-1.txt"

        Assert.Equal(306, answer)

    [<Fact>]
    let ``Day3 Part1`` () =
        let answer = Day3.Part1.solution "Day3-1.txt"

        Assert.Equal(178794710, answer)

    [<Fact>]
    let ``Day3 Part2`` () =
        let answer = Day3.Part2.solution "Day3-1.txt"

        Assert.Equal(76729637, answer)

    [<Fact>]
    let ``Day4 Part1`` () =
        let answer = Day4.Part1.solution "Day4-1.txt"

        Assert.Equal(2344, answer)

    [<Fact>]
    let ``Day4 Part2`` () =
        let answer = Day4.Part2.solution "Day4-1.txt"

        Assert.Equal(1815, answer)

    [<Fact>]
    let ``Day5 Part1`` () =
        let answer = Day5.Part1.solution "Day5-1.txt"

        Assert.Equal(5108, answer)

    [<Fact>]
    let ``Day5 Part2`` () =
        let answer = Day5.Part2.solution "Day5-1.txt"

        Assert.Equal(7380, answer)

    [<Fact>]
    let ``Day6 Part1`` () =
        let answer = Day6.Part1.solution "Day6-1.txt"

        Assert.Equal(5208, answer)

    [<Fact>]
    let ``Day6 Part2`` () =
        let answer = Day6.Part2.solution "Day6-1.txt"

        Assert.Equal(1972, answer)

    [<Fact>]
    let ``Day7 Part1`` () =
        let answer = Day7.Part1.solution "Day7-1.txt"

        Assert.Equal(2299996598890L, answer)

    [<Fact>]
    let ``Day7 Part2`` () =
        let answer = Day7.Part2.solution "Day7-1.txt"

        Assert.Equal(362646859298554L, answer)

    [<Fact>]
    let ``Day8 Part1`` () =
        let answer = Day8.Part1.solution "Day8-1.txt"

        Assert.Equal(398, answer)

    [<Fact>]
    let ``Day8 Part2`` () =
        let answer = Day8.Part2.solution "Day8-1.txt"

        Assert.Equal(1333, answer)


    [<Fact>]
    let ``Day9 Part1`` () =
        let answer = Day9.Part1.solution "Day9-1.txt"

        Assert.Equal(6307275788409L, answer)

    [<Fact>]
    let ``Day9 Part2`` () =
        let answer = Day9.Part2.solution "Day9-1.txt"

        Assert.Equal(6327174563252L, answer)

    [<Fact>]
    let ``Day10 Part1`` () =
        let answer = Day10.Part1.solution "Day10-1.txt"

        Assert.Equal(760, answer)

    [<Fact>]
    let ``Day10 Part2`` () =
        let answer = Day10.Part2.solution "Day10-1.txt"

        Assert.Equal(1764, answer)
