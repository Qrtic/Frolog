module SearchLib.SearchMachine

open SearchLib.Common
open SearchLib.Signature
open SearchLib.Rule
open SearchLib.Context
open SearchLib.Knowledgebase

// Temporary removed all immutablity

type CacheParameters = { maxPrecedences : int }

type ISearchMachine = 
    abstract member Execute: Call -> context seq
    abstract member AddRule: rule -> unit
    abstract member PrintAll: Call -> unit

module SearchMachines =
    type Simple =
        private new() = {kb = EmptyKB; c = Map.empty;}
        static member Empty() = Simple()
        val mutable kb: knowledgebase
        val mutable c: context
        
        member this.AddRule(r: rule) = this.kb <- append this.kb r
        member this.Execute(s: Call) = find this.kb this.c s
        member this.PrintAll(s: Call) =
            printfn "Call %s(%A)" s.name s.args
            let contexts = this.Execute s

            if Seq.isEmpty contexts then
                printfn "false"
            elif not (List.exists(fun p -> Argument.isVariable p) s.args) then
                    printfn "true"
            else
                for c in contexts do printfn "%A" c
        interface ISearchMachine with
            member t.AddRule r = t.AddRule r
            member t.Execute s = t.Execute s
            member t.PrintAll s = t.PrintAll s
    
    type Custom(pre: Call -> unit, query: Call -> context seq option, post: Call*context seq -> unit) =
        let mutable simple = Simple.Empty()
        let mutable hits = 0
        member this.AddRule(r: rule) = simple.AddRule r
        member this.PrintAll(s: Call) = simple.PrintAll s
        member this.Execute(s: Call) = 
            pre s |> ignore
            let prequery = query s
            match prequery with
            | Some(contexts) -> 
                hits <- hits + 1
                contexts
            | None -> 
                let res = simple.Execute s
                post (s, res) |> ignore
                res
        member this.CacheHits with get() = hits

        interface ISearchMachine with
            member t.AddRule r = t.AddRule r
            member t.Execute s = t.Execute s
            member t.PrintAll s = t.PrintAll s

        static member CacheFirstMachine cacheParameters =
            let cache = ref Map.empty
            let none = fun s -> ()
            let query = fun s -> (!cache).TryFind(s)
            let post = fun (s, cs) -> if (!cache).Count < cacheParameters.maxPrecedences then cache := (!cache).Add(s, cs)
            new Custom(none, query, post)
    
        static member CacheLastMachine cacheParameters =
            let cache = new System.Collections.Generic.Queue<Call*(context seq)>()
            let rec tryfind (s: Call) =
                match Seq.tryFind(fun t -> Call.Equals(s, (fst t))) cache with
                | Some(_, cs) -> Some(cs)
                | None -> None
            let insertnew(s, cs) =
                if (tryfind s).IsNone then
                    if cache.Count > cacheParameters.maxPrecedences then
                        cache.Dequeue() |> ignore
                    cache.Enqueue(s, cs)

            let none = fun s -> ()
            new Custom(none, tryfind, insertnew)