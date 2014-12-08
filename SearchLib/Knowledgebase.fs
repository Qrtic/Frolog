module SearchLib.Knowledgebase

open SearchLib.Common
open SearchLib.Signature
open SearchLib.Rule
open SearchLib.Context

// type knowledgebase = rulelist
let EmptyKB = Set.empty<rule>
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
                (* TODO:
                | ConcatenatedRule(conrulesignature, calls) ->
                    let addToContext = List.map2(fun k v -> (k, v)) conrulesignature.prms s.args
                    let rec reducecall (contexts: context seq) (calls: Call list): context seq =
                        match calls with
                        | call::restcalls ->
                            seq {
                                for c in contexts do
                                    let newcontexts = find d c call
                                    let restcontexts = reducecall newcontexts restcalls
                                    for c in restcontexts do
                                        yield c}
                        | [] -> contexts
                    // substitute parameters
                    // it looks like there is no situation when values can change
                    // let supplementedContext = List.fold(fun (s: context) (k, v) -> s.Add(k, v)) c addToContext
                    // let newcontexts = find d supplementedContext (replaceVars c call)
                    let supplementedContext = supply c addToContext
                    for co in reducecall [supplementedContext] calls do
                        let replaced = s.parameters |> List.filter(fun p -> isVar p) |> List.map(fun p -> p, Map.find p co)
                        yield supply (reduce c co) replaced*)
                | _ -> ()
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