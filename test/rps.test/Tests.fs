module Tests

open System
open Xunit
open RPS.Core

let testBeats loser winner = 
    let result = outcome {P1 = winner; P2 = loser}
    Assert.Equal(Victor(P1), result)  
    let result = outcome {P2 = winner; P1 = loser}
    Assert.Equal(Victor(P2), result)

[<Fact>]
let ``Rock beats scissors``() =
    Rock |> testBeats Scissors

[<Fact>]
let ``Paper beats rock`` () = 
    Paper |> testBeats Rock

[<Fact>]
let ``Scissors beats paper`` () = 
    Scissors |> testBeats Paper

[<Fact>]
let ``Always strategy is always the supplied value`` () = 
    let strategy = Always Rock
    let sample = seq { for i in 1..100 do yield strategy(Seq.empty) }
    Assert.All(sample, (fun x -> Assert.Equal(Rock, x)))
    
[<Fact>]
let ``Random strategy yields each value about a third of the time`` () =
    let strategy = RandomStrategy 
    let sample = seq { for i in 1..300 do yield strategy(Seq.empty) }
    let numRocks = sample |> Seq.countBy (id) |> Seq.map (snd)
    numRocks |> Seq.iter (fun num -> Assert.InRange(num, 50, 150))

[<Fact>]
let ``TFT strategy repeats previous move of opponent`` () =
    let history =  seq [{P1=Rock; P2=Scissors}]
    let strategy = TitForTat P1 history
    let nextMove = strategy(history) 
    Assert.Equal(Rock, nextMove)
    

