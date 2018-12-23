open Tests
open RPS.Core

            



module Program = 
    let [<EntryPoint>] main _ = 
        let s1 = RandomStrategy
        let s2 = Always Rock
        play s1 s2
        |> Seq.take 100
        |> Seq.iter (printfn "%A")
        |> ignore
        0
