module SearchLib.SearchMachine

open SearchLib.Common
open SearchLib.Argument
open SearchLib.Signature
open SearchLib.Rule
open SearchLib.Context
open SearchLib.Knowledgebase

type SearchMachine(kb: knowledgebase, c: context) =
    new() = SearchMachine(knowledgebase.Empty, Map.empty)
    member this.Execute(s: signature) = Seq.map (fun c -> SearchMachine(kb, c)) (find kb c s)
    member this.AddRule(r: rule) = SearchMachine(append kb r, c)
    member this.AddFact(r: rule) = SearchMachine(append kb r, c)
    member this.Context = c
    member this.PrintAll(s: signature) =
        printfn "Call %s(%A)" s.name s.parameters
        let contexts = this.Execute s

        if Seq.isEmpty contexts then
            printfn "false"
        elif not (List.exists(fun p -> isVar p) s.parameters) then
                printfn "true"
        else
            for c in contexts do printfn "%A" c.Context