namespace Frolog.CustomRules

open Frolog
open Frolog.DefineRule

[<RequireQualifiedAccess>]
module Grandparent =
    let parent p c = tryDefFact (sprintf "parent(%s, %s)" p c) |> Option.get
    let grandparent =
        let call (s: string) = term s |> Option.bind sign |> Option.get
        let def = call "grandparent(G, C)"
        defConcatRule (def)
            (defBody [call "parent(G, P)"; call "parent(P, C)"])

    let appendParent (m: ISearchMachine) p c = m.AddRule <| parent p c
    let appendGrandparent (m: ISearchMachine) = m.AddRule grandparent