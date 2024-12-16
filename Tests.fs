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
