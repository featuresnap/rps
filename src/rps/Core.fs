namespace RPS

open System.IO
open System.IO

module Core =
    type Move =
        | Rock
        | Paper
        | Scissors

    type Player =
        | P1
        | P2

    type Match =
        { P1 : Move
          P2 : Move }

    type Result =
        | Victor of Player
        | Draw

    type History = seq<Match>

    type Strategy = History -> Move
    let Always x : Strategy = fun (history) -> x

    let RandomStrategy : Strategy = 
        let rand = new System.Random()
        fun (history) ->
            match rand.Next(3) with 
            |0 -> Rock
            |1 -> Paper
            |_ -> Scissors
        
               

    let TitForTat player history : Strategy =
        let moveOf p m = match p with |P1 -> m.P1 |P2 -> m.P2
        fun (history) -> 
            history
            |> Seq.tryHead
            |> Option.defaultValue {P1=Rock; P2=Rock}
            |> moveOf player

    let outcome { P1 = move1; P2=move2 } =
        match move1, move2 with
        | Paper, Rock | Scissors, Paper | Rock, Scissors -> Victor(P1)
        | Rock, Paper | Paper, Scissors | Scissors, Rock -> Victor(P2)
        | _ -> Draw

   