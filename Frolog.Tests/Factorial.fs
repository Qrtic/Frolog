namespace Frolog.Tests

open Frolog
open Frolog.DefineRule

[<RequireQualifiedAccess>]
module Factorial =
    let fromZero = tryDefFact "factorial(0, 1)" |> Option.get
    let fromN = 
        let call (s: string) = term s |> Option.bind sign |> Option.get
        let def = call "factorial(X, F)"
        defConcatRule (def) 
            (defBody [
                call ">(X, 0)";
                call "--(X, X1)";
                call "factorial(X1, F1)";
                call "*(X, F1, F)"])

    let appendFactorial(m: ISearchMachine) =
        m.AddRule fromZero
        m.AddRule fromN