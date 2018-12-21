open Tests
open RPS.Core
module game = 

    let play (s1:Strategy) (s2:Strategy) = 

        let history = []

        let rec play' (s1:Strategy) (s2:Strategy) history = seq {
            let hseq = history |> Seq.ofList
            let moves = {P1=s1(hseq); P2=s2(hseq)}
            let result = moves |> outcome
            yield result
            yield! play' s1 s2 ((moves, result)::history)
        }

        play' s1 s2 history
        

            



module Program = 
    let [<EntryPoint>] main _ = 
        let s1 = RandomStrategy
        let s2 = Always Rock
        game.play s1 s2
        |> Seq.take 100
        |> Seq.iter (printfn "%A")
        |> ignore
        0
