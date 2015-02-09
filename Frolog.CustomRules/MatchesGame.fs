namespace Frolog.CustomRules

open Frolog
open Frolog.DefineRule

module MatchesGame =
    let move x = tryDefFact (sprintf "move(%i)" x) |> Option.get
    let winMoves = [move 1; move 2; move 3]
    let goodMoves = 
        let call (s: string) = term s |> Option.bind sign |> Option.get
        let def = call "good_move(X, M)"
        [defConcatRule def (defBody [call "move(M)"; call "-(X, M, 1)"]);
            defConcatRule def (defBody [call "move(M)"; call "-(X, M, X1)"; call "not(good_move(X1, _))"])]


