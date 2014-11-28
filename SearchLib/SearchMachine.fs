module SearchLib.SearchMachine

open SearchLib.Common
open SearchLib.Argument
open SearchLib.Signature
open SearchLib.Rule
open SearchLib.Context
open SearchLib.Knowledgebase

type ISearchMachine = 
    abstract member Execute: signature -> ISearchMachine seq
    abstract member AddRule: rule -> ISearchMachine
    abstract member PrintAll: signature -> unit
    abstract member Context: context

type SearchMachine(kb: knowledgebase, c: context) =
    new() = SearchMachine(EmptyKB, Map.empty)
    static member Empty = SearchMachine()
    member this.AddFact(r: rule) = SearchMachine(append kb r, c)
    
    interface ISearchMachine with
        member this.Execute(s: signature) = Seq.map (fun c -> SearchMachine(kb, c) :> ISearchMachine) (find kb c s)
        member this.AddRule(r: rule) = SearchMachine(append kb r, c) :> ISearchMachine
        member this.PrintAll(s: signature) =
            let ithis = this :> ISearchMachine
            printfn "Call %s(%A)" s.name s.parameters
            let contexts = ithis.Execute s

            if Seq.isEmpty contexts then
                printfn "false"
            elif not (List.exists(fun p -> isVar p) s.parameters) then
                    printfn "true"
            else
                for c in contexts do printfn "%A" c.Context
        member this.Context = c

type CacheParameters = { maxPrecedences : int }

type CacheSearchMachine(kb: knowledgebase, c: context, cacheprms: CacheParameters, precedence: Map<signature, seq<ISearchMachine>>) =
    new(kb, c, cacheprms) = CacheSearchMachine(kb, c, cacheprms, Map.empty)
    new(cacheprms) = CacheSearchMachine(EmptyKB, Map.empty, cacheprms, Map.empty)
    static member Empty = CacheSearchMachine({maxPrecedences = 100})
    member s.machine = SearchMachine(kb, c) :> ISearchMachine
    member val precedences = precedence with get, set

    interface ISearchMachine with
        member this.Execute(s: signature) = 
            let res = this.precedences.TryFind(s)
            if res.IsSome then
                res.Value
            else
                let res = Seq.map (fun c -> SearchMachine(kb, c) :> ISearchMachine) (find kb c s)
                this.precedences <- this.precedences.Add(s, res)
                res
        member this.AddRule(r: rule) = CacheSearchMachine(append kb r, c, cacheprms, this.precedences) :> ISearchMachine
        member this.Context = this.machine.Context
        member this.PrintAll(s: signature) = this.machine.PrintAll s