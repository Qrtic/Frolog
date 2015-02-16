namespace Frolog

// Temporary removed all immutablity
type CacheParameters = { maxPrecedences : int }

type ISearchMachine = 
    abstract member Execute: Signature -> SearchResult
    abstract member AddRule: Rule -> unit

module SearchMachines =
    type ExecuteSearcher(machine : ISearchMachine) =
        interface ISearcher with
            member __.Search kb s = machine.Execute s

    type Simple =
        private new() = {kb = Knowledgebase.Default; searcher = new SimpleSearcher()}
        private new(searcher) = {kb = Knowledgebase.Default; searcher = searcher}
        private new(kb) = {kb = kb; searcher = new SimpleSearcher()}
        static member Create() = Simple()
        static member CreateDebug() = Simple(new DebugInfoSearcher())
        static member CreateClear() = Simple(Knowledgebase.Empty)
        val searcher: ISearcher
        val mutable kb: Rulebase
        member this.AddRule(r: Rule) = 
            let newk = this.kb.Append r
            this.kb <- newk
        member this.Execute(s: Signature) = this.searcher.Search this.kb s
        interface ISearchMachine with
            member t.AddRule r = t.AddRule r
            member t.Execute s = t.Execute s
    
    type Custom(pre: Signature -> unit, query: Signature -> SearchResult option, post: Signature*SearchResult -> unit) =
        // TODO: Customize finder
        let mutable kb: Rulebase = Knowledgebase.Default :> Rulebase
        let mutable hits = 0
        member this.searcher = new ExecuteSearcher(this)
        member this.AddRule(r: Rule) = kb <- kb.Append r
        member this.Execute(s: Signature) = 
            pre s |> ignore
            let prequery = query s
            match prequery with
            | Some(contexts) -> 
                hits <- hits + 1
                contexts
            | None -> 
                let res = Search.search (this.searcher :> ISearcher) kb s
                post (s, res) |> ignore
                res
        member this.CacheHits with get() = hits

        interface ISearchMachine with
            member t.AddRule r = t.AddRule r
            member t.Execute s = t.Execute s

        static member CacheFirstMachine cacheParameters =
            let cache = ref Map.empty<Signature, SearchResult>
            let query s =  (!cache).TryFind s
            let post(s, cs) = 
                if (!cache).Count < cacheParameters.maxPrecedences then
                    cache := (!cache).Add(s, cs)
            new Custom(ignore, query, post)

        static member CacheLastMachine cacheParameters =
            let chc = Array.create<(Signature*SearchResult) option>(cacheParameters.maxPrecedences) Option.None
            let chcPtr = ref 0
            let lastCacheResult = ref false

            let rec tryfind (s: Signature) =
                let foundres = chc |> Array.choose(fun x -> x) |> Array.tryFind(fun t -> Signature.Equals(s, (fst t)))
                lastCacheResult := foundres.IsSome
                match foundres with
                | Some(_, cs) -> Some(cs)
                | None -> None
            let insertnew(s, cs) =
                if not !lastCacheResult && cacheParameters.maxPrecedences > 0 then
                    chc.[!chcPtr] <- Some((s, cs))
                    incr chcPtr
                    if (!chcPtr = cacheParameters.maxPrecedences) then
                        chcPtr := 0
                        
            new Custom(ignore, tryfind, insertnew)