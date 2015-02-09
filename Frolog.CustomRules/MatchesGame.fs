namespace Frolog.CustomRules

open Frolog
open Frolog.DefineRule

module MatchesGame =
    let move x = tryDefFact (sprintf "move(%i)" x) |> Option.get
    let winMoves = [move 1; move 2; move 3]
    let badMoves = [move 4; move 5; move 6]
    let goodMoves = 
        let call (s: string) = term s |> Option.bind sign |> Option.get
        let def = call "good_move(X, M)"
        [defConcatRule def (defBody [call "move(M)"; call "-(X, M, 1)"]);
            defConcatRule def (defBody [call "move(M)"; call "-(X, M, X1)"; call "not(good_move(X1, _))"])]

    let gMoves = 
        let call c = Lexem(Call(term c |> Option.bind sign |> Option.get))
        let not c = Not(call c)
        let def = "good_move(X, M)" |> term |> Option.bind sign |> Option.get 
        [   defFact def |> combine (call "move(M)") |> combine (call "-(X, M, 1)");
            defFact def |> combine (call "move(M)") |> combine (call "-(X, M, X1)") |> combine (not "good_move(X1, _)")]