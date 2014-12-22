namespace SearchLib

open SearchLib.Common
open SearchLib.Rule
open SearchLib.Context

type Rulebase =
    abstract member Append: rule -> Rulebase
    abstract member AppendRange: rule seq -> Rulebase
    abstract member Rules: rule list with get

type Knowledgebase(rules: rule list) =
    static member Empty = new Knowledgebase([])
    static member Default = new Knowledgebase(defaultRules)

    member k.rules = rules
    interface Rulebase with
        member k.Append r = new Knowledgebase(r::k.rules) :> Rulebase
        member k.AppendRange rs = (k, rs) ||> Seq.fold(fun kb r -> new Knowledgebase(r::kb.rules)) :> Rulebase
        member k.Rules = k.rules
        
type FindResult = 
    | Failure
    | Success of context
    | Continuation of context * Call list

module Find =
    let process_predicate(c: context) (s: Call) (comp: predicate): result =
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
        res

    let process_fact(c: context) (call: Call) (fact: Definition): result =
        let (?>) = Unify.tryUnify
        let res = ([], fact.prms, call.args) |||> List.fold2(fun s p a -> (p ?> a) :: s) |> List.rev
        if List.exists Option.isNone res then
            Rejected
        else
            let resargs = List.map Option.get res
            Accepted(resargs)
            
    let process_result(c: context) (call: Call) (result: arguments):context =
        let proc (acc: context) (p: argument) (a: argument) =
            let av = Argument.asVariable p
            if av.IsNone then
                acc
            else
                let v = av.Value
                let asVal = Argument.asValue a
                match asVal with
                | None -> acc // failwith "Cant determine value with variable parameter and argument"
                | Some(value) -> acc.Add(v, value)
        List.fold2 proc c (call.args) result

    /// Returns sequence of answers.
    /// If result is empty,
    /// then predicate equals false.
    let rec find (d: Rulebase) (c: context) (call: Call) : FindResult seq = 
        let call = replaceVars c call
        let acceptedRules = d.Rules |> List.filter(fun r -> Signature.compatible(r.Signature, call))

        seq {
            for r in acceptedRules do
                let suppliedContext = supply c r.Parameters call.args
                match r with
                    | Rule(_, p) ->
                        let procedres = process_predicate suppliedContext call p
                        match procedres with
                        | Accepted(args) -> yield Success(process_result suppliedContext call args)
                        | Rejected -> yield Failure
                    | Fact(def) ->
                        let procedres = process_fact suppliedContext call def
                        match procedres with
                        | Accepted(args) -> yield Success(process_result suppliedContext call args)
                        | Rejected -> yield Failure
                    | ConcatenatedRule(conrulesignature, calls) ->
                        if (calls.Length > 0) then
                            yield Continuation(suppliedContext, calls)
                        else
                            yield Success(suppliedContext)
        }

type Finder =
    abstract Find: Rulebase -> context -> Call -> FindResult seq

type SimpleFinder() =
    interface Finder with
        member f.Find rb c call = Find.find rb c call

type DebugInfoFinder() =
    interface Finder with
        member f.Find rb c call = 
            debug (sprintf "Called rule %s" call.AsString)
            let found = (new SimpleFinder() :> Finder).Find rb c call
            if (Seq.isEmpty found) then
                debug "No rules match."
            else
                for r in found do
                    debug (sprintf "Found %s" <| r.ToString())
            found

module Execute =
    let exec (f: Finder) (d: Rulebase) (start: context) (s: Call): unit =
        printfn "Execute: %s. Context = %s" s.AsString (start.ToString())
        let res = f.Find d start s
        if Seq.isEmpty res then
            printfn "Result: %b." false
        else
            for r in res do
                printfn "Result: %b. New context = %s" (true) ((r).ToString())
                