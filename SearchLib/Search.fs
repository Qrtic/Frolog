namespace SearchLib

open SearchLib.Common
open SearchLib.Argument
open SearchLib.Signature
open SearchLib.Rule
open SearchLib.Context

module Search =
    type knowledgebase = rulelist
    
    let process_predicate(c: context) (s: signature) (comp: predicate): bool * context =
        let check_parameters_count(s: signature) (comp: predicate): bool = 
            let (n, prms) = s
            match comp with
                | F0(_) -> prms.Length = 0
                | F1(_) -> prms.Length = 1
                | F2(_) -> prms.Length = 2
                | F3(_) -> prms.Length = 3
        let res: result =
            if not (check_parameters_count s comp) then False
            else
                let prms = snd s |> List.map (convert_to_arg c)
                match comp with
                    | F0(f0) -> f0
                    | F1(f1) -> f1 prms.Head
                    | F2(f2) -> f2 prms.Head prms.Tail.Head
                    | F3(f3) -> f3 prms.Head prms.Tail.Head prms.Tail.Tail.Head
        let process_result(c: context) (inits: parameters) (ress: parameters):context =
            let proc (acc: context) = fun p1 -> fun p2 -> if isVar p1 then acc.Add(p1, p2) else acc
            List.fold2 proc c inits ress
        match res with
        | True(parameters) -> true, process_result c (snd s) parameters
        | False -> false, c

    /// Returns sequence of answers
    /// If result is empty
    /// Then predicate equals false
    let rec find (d: knowledgebase) (c: context) (s: signature) : seq<context> = 
        let rules = d |> List.toSeq
        let acceptedRules = rules |> Seq.filter(fun r -> r |> get_signature |> signatureEq s)
        seq {
            for r in acceptedRules do
                match r with
                    | Rule(_, p) -> 
                        let proc = process_predicate c s p
                        if fst proc then
                            yield snd proc
                    | ConcatenatedRule(_, p, next_r) -> 
                        let proc = process_predicate c s p
                        if fst proc then
                            let next = find d (snd proc) (get_signature next_r)
                            for proced in next do
                                yield proced
        }

    let exec (d: knowledgebase) (start: context) (s: signature): unit =
        printfn "Execute: %s. Context = %s" (toStr s) (start.ToString())
        let res = find d start s
        if Seq.isEmpty res then
            printfn "Result: %b." false
        else
            for r in res do
                printfn "Result: %b. New context = %s" (true) ((r).ToString())

    let append (d: knowledgebase) (r: rule): knowledgebase = r::d

    let testkb = [Fact("f", ["1"]); Fact("f", ["2"])] @ defaultRules
    let textcontext = Map.empty.Add("A", "1").Add("B", "3").Add("C", "4")
    let test = exec testkb Map.empty ("f", ["D"])