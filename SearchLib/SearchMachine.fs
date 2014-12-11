module SearchLib.SearchMachine

open SearchLib.Common
open SearchLib.Signature
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
        private new() = {kb = Knowledgebase.Empty}
        static member Create() = Simple()
        val mutable kb: Knowledgebase
        member t.Clear() = t.kb <- Knowledgebase.Empty
        member this.AddRule(r: rule) = 
            let newk = this.kb.Append r
            this.kb <- newk
        member this.Execute(s: Call) = this.Execute(s, Context.EmptyContext)
        member this.Execute(s: Call, c: context): context seq =
            let findres = SearchLib.Find.find this.kb c s
            seq {
                for res in findres do
                    match res with
                    | Failure -> ()
                    | Success(context) -> yield context
                    | Continuation(context, calls) ->
                        //let proc(contexts: context seq) (calls: Call seq) : context seq =
                        //    calls |> Seq.fold(fun (s: context seq) call -> s |> Seq.collect(fun c -> find d c call)) contexts
                        let first = calls.Head
                        let procFirst = this.Execute(first, context)

                        let proc(contexts: context seq) (calls: Call seq): context seq =
                            calls |> Seq.fold(fun (s: context seq) call ->
                                s |> Seq.collect(fun c -> this.Execute(call, c))) contexts

                        let proced = proc [context] calls
                        let postsupplied = proced
                        let returned = postsupplied |> Seq.map(fun ps -> Context.replace ps c)
                        let reduced = returned |> Seq.map(fun rs -> Context.reduce rs c)

                        yield! reduced
                    (*
                    let psl = conrulesignature.prms
                    let asl = s.args

                    let currentCall = calls.Head
                    let restCalls = calls.Tail
                                        
                    let proc(contexts: context seq) (calls: Call seq) : context seq =
                        calls |> Seq.fold(fun (s: context seq) call -> s |> Seq.collect(fun c -> find d c call)) contexts
                        
                    // substitute parameters
                    let presupplied = supply c psl asl
                    let proced = proc [presupplied] calls
                    // substitute parameters
                    let postsupplied = proced |> Seq.map(fun c -> supply c psl asl)
                    // return previous context parameters

                    let returned = postsupplied |> Seq.map(fun ps -> Context.replace ps c)
                    let reduced = returned |> Seq.map(fun rs -> Context.reduce rs c)

                    yield! reduced*)
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
            member t.Clear() = t.Clear()

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