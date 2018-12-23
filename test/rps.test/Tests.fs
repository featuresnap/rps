module Tests

open System
open Xunit
open RPS.Core

let testBeats loser winner = 
    let result = outcome {P1 = winner; P2 = loser}
    Assert.Equal(Victor(P1), result)  
    let result = outcome {P2 = winner; P1 = loser}
    Assert.Equal(Victor(P2), result)

let moveOf player = function 
    |((moves, _) : IndividualOutcome) -> 
        match player with 
        |P1 -> moves.P1 
        |_ -> moves.P2

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
    let history =  seq [{P1=Rock; P2=Scissors}, Victor P1]
    let strategy = TitForTat P1 
    let nextMove = strategy(history) 
    Assert.Equal(Scissors, nextMove)


[<Fact>]
let ``TFT strategy repeats a random sequence`` () = 
    let opponentStrategy = RandomStrategy
    let myStrategy = TitForTat P2
    let result = play opponentStrategy myStrategy |> Seq.take 10 |> List.ofSeq
    let opponentMoves = result |> Seq.map (moveOf P1) 
    let myResponses = result |> Seq.map (moveOf P2) |> Seq.tail
    let comparison = Seq.zip opponentMoves myResponses
    Assert.True(comparison |> Seq.forall (fun (opp, rsp) -> rsp = opp))


    
    

