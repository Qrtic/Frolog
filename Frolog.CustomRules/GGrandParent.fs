namespace Frolog.CustomRules

open Frolog
open Frolog.DefineRule
open Frolog.DefineRule.DefPublic

[<RequireQualifiedAccess>]
module GGrandparent =
    let parent p c = tryDefFact (sprintf "parent(%s, %s)" p c) |> Option.get
    let ggrandparent =
        let call (s: string) = term s |> Option.bind sign |> Option.get
        let def = call "ggrandparent(GG, C)"
        defCall def (call "parent(GG, G)") |> combine (defCallBodyf "parent(G, P)") |> combine (defCallBodyf "parent(P, C)")

    let appendGGrandparent (m: ISearchMachine) = m.AddRule ggrandparent