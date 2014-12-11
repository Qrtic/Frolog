namespace SearchLib

open SearchLib.Common
open SearchLib.Signature
open SearchLib.Rule
open SearchLib.Context

type Knowledgebase =
    val Rules: list<rule>
    private new() = { Rules = list.Empty }
    private new(rules) = { Rules = rules }
    static member Empty = new Knowledgebase()
    static member Default = new Knowledgebase(defaultRules)
    member k.Append r = new Knowledgebase(r::k.Rules)

type FindResult = 
    | Failure
    | Success of context
    | Continuation of context * Call list

module Find =
    let process_predicate(c: context) (s: Call) (comp: predicate): bool * context =
        let check_parameters_count(s: Call) (comp: predicate): bool = 
            let n = s.name
            let args = s.args
            match comp with
                | F0(_) -> args.Length = 0
                | F1(_) -> args.Length = 1
                | F2(_) -> args.Length = 2
                | F3(_) -> args.Length = 3
        let res: result =
            if not (check_parameters_count s comp) then Rejected
            else
                let args = s.args
                match comp with
                    | F0(f0) -> f0
                    | F1(f1) -> f1 args.Head
                    | F2(f2) -> f2 args.Head args.Tail.Head
                    | F3(f3) -> f3 args.Head args.Tail.Head args.Tail.Tail.Head
        let process_result(c: context) (inits: arguments) (ress: arguments):context =
            let proc (acc: context) (p: argument) (a: argument) =
                let av = Argument.asVariable p
                if av.IsNone then
                    acc
                else
                    let v = av.Value
                    acc.Add(v, Argument.asValue a |> Option.get)
            List.fold2 proc c inits ress
        match res with
        | Accepted(arguments) -> true, process_result c s.args arguments
        | Rejected -> false, c

    let process_fact(c: context) (call: Call) (fact: Definition): bool * context =
        let (?=) = Unify.canUnify
        let (?>) p a = Unify.tryUnify p a
        let res = ([], fact.prms, call.args) |||> List.fold2(fun s p a -> (p ?> a) :: s)
        
        let process_result(c: context) (inits: arguments) (ress: arguments):context =
            let proc (acc: context) (p: argument) (a: argument) =
                let av = Argument.asVariable p
                if av.IsNone then
                    acc
                else
                    let v = av.Value
                    acc.Add(v, Argument.asValue a |> Option.get)
            List.fold2 proc c inits ress

        if List.exists Option.isNone res then
            false, c
        else
            let resargs = List.map Option.get res
            let sres = true, (process_result c call.args resargs)
            sres

    /// Returns sequence of answers.
    /// If result is empty,
    /// then predicate equals false.
    let rec find (d: Knowledgebase) (c: context) (s: Call) : FindResult seq = 
        let s = replaceVars c s // replace vars!
        debug (sprintf "Called rule %s with args = %A" s.name s.args)
        let acceptedRules = d.Rules |> List.filter(fun r -> Signature.compatible(r.Signature, s))

        seq {
            for r in acceptedRules do   
                let suppliedContext = supply c r.Parameters s.args
                match r with
                    | Rule(_, p) ->
                        let proc = process_predicate suppliedContext s p
                        let (success, contexts) = proc
                        match success with
                        | false -> yield Failure
                        | true -> yield Success(contexts)
                    | Fact(def) ->
                        let proc = process_fact suppliedContext s def
                        let (success, contexts) = proc
                        match success with
                        | false -> yield Failure
                        | true -> yield Success(contexts)
                    | ConcatenatedRule(conrulesignature, calls) ->
                        if (calls.Length > 0) then
                            yield Continuation(suppliedContext, calls)
                        else
                            yield Success(suppliedContext)
        }

    let exec (d: Knowledgebase) (start: context) (s: Call): unit =
        printfn "Execute: %s. Context = %s" s.AsString (start.ToString())
        let res = find d start s
        if Seq.isEmpty res then
            printfn "Result: %b." false
        else
            for r in res do
                printfn "Result: %b. New context = %s" (true) ((r).ToString())

    let append (d: Knowledgebase) (r: rule): Knowledgebase = d.Append r