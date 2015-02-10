namespace Frolog.CustomRules

open Frolog
open Frolog.DefineRule
open Frolog.DefineRule.DefPublic

[<RequireQualifiedAccess>]
module Factorial =
    let fromZero = tryDefFact "factorial(0, 1)" |> Option.get
    let fromN = 
        let call (s: string) = term s |> Option.bind sign |> Option.get
        let dc (s: string) = term s |> Option.bind sign |> Option.get |> defCallBody
        let def = call "factorial(X, F)"
        defCall def (call ">(X, 0)") |> combine (dc "--(X, X1)") |> combine (dc "factorial(X1, F1)") |> combine (dc "*(X, F1, F)")

    let appendFactorial(m: ISearchMachine) =
        m.AddRule fromZero
        m.AddRule fromN