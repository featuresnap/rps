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

    type IndividualOutcome = Match * Result

    type History = seq<IndividualOutcome>

    type Strategy = History -> Move
    let Always x : Strategy = fun (history) -> x

    let RandomStrategy : Strategy = 
        let rand = new System.Random()
        fun (history) ->
            match rand.Next(3) with 
            |0 -> Rock
            |1 -> Paper
            |_ -> Scissors
        
               

    let TitForTat player : Strategy =
        let moveOf p m = match p with |P1 -> m.P1 |P2 -> m.P2
        let otherPlayer = match player with |P1 -> P2 |P2 -> P1
        fun (history) -> 
            history
            |> Seq.tryHead
            |> Option.map fst
            |> Option.defaultValue {P1=Rock; P2=Rock}
            |> moveOf otherPlayer

    let outcome { P1 = move1; P2=move2 } =
        match move1, move2 with
        | Paper, Rock | Scissors, Paper | Rock, Scissors -> Victor(P1)
        | Rock, Paper | Paper, Scissors | Scissors, Rock -> Victor(P2)
        | _ -> Draw

    let play (s1:Strategy) (s2:Strategy) : seq<IndividualOutcome> = 

        let history = []

        let rec play' (s1:Strategy) (s2:Strategy) history = seq {
            let hseq = history |> Seq.ofList
            let moves = {P1=s1(hseq); P2=s2(hseq)}
            let result = moves |> outcome
            yield (moves, result)
            yield! play' s1 s2 ((moves, result)::history)
        }

        play' s1 s2 history
        



   