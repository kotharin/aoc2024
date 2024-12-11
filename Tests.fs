module Tests

    open Xunit

    [<Fact>]

    let ``Day1 Part1`` () =
        let answer = Day1.Part1.solution "Day1-1.txt"

        Assert.Equal(2375403, answer)