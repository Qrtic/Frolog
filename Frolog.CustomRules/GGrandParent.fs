namespace Frolog.CustomRules

open Frolog
open Frolog.DefineRule

[<RequireQualifiedAccess>]
module GGrandparent =
    let parent p c = tryDefFact (sprintf "parent(%s, %s)" p c) |> Option.get
    let ggrandparent =
        let call (s: string) = term s |> Option.bind sign |> Option.get
        let def = call "ggrandparent(GG, C)"
        defConcatRule (def)
            (defBody [call "parent(GG, G)"; call "parent(G, P)"; call "parent(P, C)"])

    let appendGGrandparent (m: ISearchMachine) = m.AddRule ggrandparent