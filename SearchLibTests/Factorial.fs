namespace SearchLibTests

open SearchLib
open SearchLib.Context
open SearchLib.SearchMachine
open SearchLib.Rule

[<RequireQualifiedAccess>]
module Factorial =
    let FromZero = rule.fact(Signature.define("factorial", ["0"; "1"]))
    let FromN = 
        let def = Signature.define("factorial", ["X"; "F"])
        let calls = [
            Signature.call("greater", ["X"; "0"]);
            Signature.call("dec", ["X"; "X1"]);
            Signature.call("factorial", ["X1"; "F1"]);
            Signature.call("mul", ["X"; "F1"; "F"])
        ]
        rule.ConcatenatedRule(def, calls)

    let appendFactorial(m: ISearchMachine) =
        m.AddRule FromZero
        m.AddRule FromN