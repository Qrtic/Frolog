module SearchLib.Knowledgebase

open SearchLib.Common
open SearchLib.Signature
open SearchLib.Rule
open SearchLib.Context

// type knowledgebase = rulelist
let EmptyKB = Set.empty<rule>
let DefaultKB = defaultRules |> Set.ofList
type knowledgebase = Set<rule>
    
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

/// Returns sequence of answers.
/// If result is empty,
/// then predicate equals false.
let rec find (d: knowledgebase) (c: context) (s: Call) : seq<context> = 
    let s = replaceVars c s // replace vars!
    debug (sprintf "Called rule %s with args = %A" s.name s.args)
    let acceptedRules = d |> Set.filter(fun r -> Signature.compatible(r.Signature, s))
    seq {
        for r in acceptedRules do
            match r with
                | Rule(_, p) ->
                    let proc = process_predicate c s p
                    if fst proc then
                        yield snd proc
                | ConcatenatedRule(conrulesignature, calls) ->
                    let psl = conrulesignature.prms
                    let asl = s.args

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

                    yield! reduced
    }

let exec (d: knowledgebase) (start: context) (s: Call): unit =
    printfn "Execute: %s. Context = %s" s.AsString (start.ToString())
    let res = find d start s
    if Seq.isEmpty res then
        printfn "Result: %b." false
    else
        for r in res do
            printfn "Result: %b. New context = %s" (true) ((r).ToString())

let append (d: knowledgebase) (r: rule): knowledgebase = d.Add r