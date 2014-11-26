module SearchLib.Knowledgebase

open SearchLib.Common
open SearchLib.Argument
open SearchLib.Signature
open SearchLib.Rule
open SearchLib.Context

type knowledgebase = rulelist
    
let process_predicate(c: context) (s: signature) (comp: predicate): bool * context =
    let check_parameters_count(s: signature) (comp: predicate): bool = 
        let n = s.name
        let prms = s.parameters
        match comp with
            | F0(_) -> prms.Length = 0
            | F1(_) -> prms.Length = 1
            | F2(_) -> prms.Length = 2
            | F3(_) -> prms.Length = 3
    let res: result =
        if not (check_parameters_count s comp) then Rejected
        else
            let prms = s.parameters |> List.map (convert_to_arg c) // TODO delete this, converted at find func
            match comp with
                | F0(f0) -> f0
                | F1(f1) -> f1 prms.Head
                | F2(f2) -> f2 prms.Head prms.Tail.Head
                | F3(f3) -> f3 prms.Head prms.Tail.Head prms.Tail.Tail.Head
    let process_result(c: context) (inits: parameters) (ress: parameters):context =
        let proc (acc: context) = fun p1 -> fun p2 -> if isVar p1 then acc.Add(p1, p2) else acc
        List.fold2 proc c inits ress
    match res with
    | Accepted(parameters) -> true, process_result c s.parameters parameters
    | Rejected -> false, c

/// Returns sequence of answers
/// If result is empty
/// Then predicate equals false
let rec find (d: knowledgebase) (c: context) (s: signature) : seq<context> = 
    let s = replaceVars c s // replace vars!
    debug (sprintf "Called rule %s with args = %A" s.name s.parameters)
    let rules = d |> List.toSeq
    let acceptedRules = rules |> Seq.filter(fun r -> r.Signature |> signatureEq s)
    seq {
        for r in acceptedRules do
            match r with
                | Rule(_, p) ->
                    let proc = process_predicate c s p
                    if fst proc then
                        yield snd proc
                | ConcatenatedRule(conrulesignature, calls) ->
                    let addToContext = List.map2(fun k v -> (k, v)) conrulesignature.parameters s.parameters
                    let rec reducecall (contexts: context seq) (calls: signature list): context seq =
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
                        yield supply (reduce c co) replaced
    }

let exec (d: knowledgebase) (start: context) (s: signature): unit =
    printfn "Execute: %s. Context = %s" s.AsString (start.ToString())
    let res = find d start s
    if Seq.isEmpty res then
        printfn "Result: %b." false
    else
        for r in res do
            printfn "Result: %b. New context = %s" (true) ((r).ToString())

let append (d: knowledgebase) (r: rule): knowledgebase = r::d