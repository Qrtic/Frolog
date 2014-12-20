module SearchLib.SearchMachine

open SearchLib.Common
open SearchLib.Rule
open SearchLib.Context

// Temporary removed all immutablity
type CacheParameters = { maxPrecedences : int }

type ISearchMachine = 
    abstract member Execute: Call -> context seq
    abstract member AddRule: rule -> unit
    abstract member PrintAll: Call -> unit
    abstract member Clear: unit -> unit

module SearchMachines =
    type Simple =
        private new() = {kb = Knowledgebase.Empty; finder = new SimpleFinder()}
        private new(finder) = {kb = Knowledgebase.Empty; finder = finder}
        static member Create() = Simple()
        val finder: Finder
        val mutable kb: Rulebase
        member t.Clear() = t.kb <- Knowledgebase.Empty
        member this.AddRule(r: rule) = 
            let newk = this.kb.Append r
            this.kb <- newk
        member this.Execute(s: Call) = this.Execute(s, Context.EmptyContext)
        member this.Execute(s: Call, c: context): context seq =
            let findres = this.finder.Find this.kb c s
            seq {
                for res in findres do
                    match res with
                    | Failure -> ()
                    | Success(resContext) -> yield resContext
                    | Continuation(resContext1, calls) ->
                        let proc(contexts: context seq) (calls: Call seq): context seq =
                            calls |> Seq.fold(fun (s: context seq) call ->
                                s |> Seq.collect(fun c -> this.Execute(call, c))) contexts
                                
                        let proced = proc [resContext1] calls
                        let postsupplied = proced |> Seq.toList
                        let returned = postsupplied |> Seq.map(fun ps -> Context.replace ps c)
                        let reduced = returned |> Seq.map(fun rs -> Context.reduce rs c)

                        yield! reduced
            }
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
            member t.Clear() = t.Clear()
    
    type Custom(pre: Call -> unit, query: Call -> context seq option, post: Call*context seq -> unit) =
        let mutable simple = Simple.Create()
        let mutable hits = 0
        // TODO: custom clear
        member this.Clear() = simple.Clear()
        member this.AddRule(r: rule) = simple.AddRule r
        member this.PrintAll(s: Call) = simple.PrintAll s
        member this.Execute(s: Call) = 
            pre s |> ignore
            let prequery = None // query s
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
            member t.Clear() = t.Clear()

        static member CacheFirstMachine cacheParameters =
            let cache = ref Map.empty<Call, context seq>
            let none s = ()
            let query s: (context seq) option = (!cache).TryFind(s)
            let post(s, cs) = 
                let overmax = (!cache).Count < cacheParameters.maxPrecedences
                if overmax then
                    cache := (!cache).Add(s, cs)
            new Custom(none, query, post)
    
        static member CacheLastMachine cacheParameters =
            let chc = Array.create<(Call*(context seq)) option>(cacheParameters.maxPrecedences) Option.None
            let chcPtr = ref 0
            let lastCacheResult = ref false
            
            let rec tryfind (s: Call) =
                let foundres = chc |> Array.choose(fun x -> x) |> Array.tryFind(fun t -> Call.Equals(s, (fst t)))
                lastCacheResult := foundres.IsSome
                match foundres with
                | Some(_, cs) -> Some(cs)
                | None -> None
            let insertnew(s, cs) =
                if not !lastCacheResult then
                    chc.[!chcPtr] <- Some((s, cs))
                    incr chcPtr
                    if (!chcPtr = cacheParameters.maxPrecedences) then
                        chcPtr := 0
                        
            let none s = ()
            new Custom(none, tryfind, insertnew)