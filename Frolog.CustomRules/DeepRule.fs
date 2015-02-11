namespace Frolog.CustomRules

open Frolog
open Frolog.DefineRule
open Frolog.DefineRule.DefPublic

module DeepRule =
    let private call' = sprintf "fact(%s, %s)"
    let getFact a b = 
        tryDefFact (call' a b) |> Option.get
    let getRule n =
        if n <= 0 then
            defFact (signf <| termf "drule(X, X)")
        else
            let def = signf <| termf (sprintf "rule%i(X0, Y)" n)
            let rec _getRule curP n res =
                let curPs = curP.ToString()
                if n = 1 then
                    res |> combine (defCallBodyf (call' ("X" + curPs) "Y"))
                else
                    let nextP = curP + 1
                    let nextPs = nextP.ToString()
                    _getRule nextP (n-1) (res |> combine (defCallBodyf (call' ("X" + curPs) ("X" + nextPs))))
            _getRule 0 n (defFact def)