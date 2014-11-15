namespace SearchLib

module Search =
    type name = string
    type value = string
    type parameter = name
    type argument = value
    type parameters = list<parameter>
    type signature = name * parameters
    type context = Map<parameter, value>
    type result = True of parameters | False

    let (?=) (p1: argument) (p2: argument): bool =
        let isUp1 = System.Char.IsUpper (p1.Chars 0)
        let isUp2 = System.Char.IsUpper (p2.Chars 0)
        if isUp1 then
            true
        else
            if isUp2 then
                true
            else
                p1.CompareTo p2 = 0
                
    let isVar (p: argument): bool = System.Char.IsUpper (p.Chars 0)

    let asInt (p: argument): option<int> =
        let ok, value = System.Int32.TryParse p
        if ok then
            Some value
        else
            None
            
    let asIntList (p: argument): option<list<int>> =
        if isVar p then None
        else
            let l = p.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
            if Array.exists(fun x -> not (fst (System.Int32.TryParse x))) l then
               None
            else
                Some(Array.map(fun x -> System.Int32.Parse x) l |> Array.toList)

    let convert_to_arg(c: context) (p: parameter): argument = 
        if isVar p then 
            let res = c.TryFind(p) // Think that we always have this p
            match res with
                | Some(v) -> v
                | None -> p
        else p

    /// Check equals
    let compare (p1: parameter) (p2: parameter): bool =
        let v1 = isVar p1
        let v2 = isVar p2
        if v1 || v2 then true
        else v1 = v2 // All non vars are constants that are defined as name that also can be used as value

    type predicate = F0 of result | F1 of ((argument) -> result) | F2 of (argument -> argument -> result) | F3 of (argument -> argument -> argument -> result)
    type rule = Rule of signature * f: predicate | ConcatenatedRule of signature * predicate * rule
    let Fact(s: signature) = match (snd s) with
                                | [] -> Rule(s, F0(True([])))
                                | h::[] -> Rule(s, F1(fun arg -> if compare arg h then True([h]) else False))
                                | h1::h2::[] -> Rule(s, F2(fun arg1 -> fun arg2 -> if compare arg1 h1 && compare arg2 h2 then True([h1;h2]) else False))
                                | h1::h2::h3::[] -> Rule(s, F3(fun arg1 -> fun arg2 -> fun arg3 -> if compare arg1 h1 && compare arg2 h2 && compare arg3 h3 then True([h1;h2;h3]) else False))

    type rulelist = list<rule>
    type knowledgebase = rulelist

    let toStr (s: signature): string =
        fst s + ((snd s).ToString())
    let signatureEq (s1 : signature) (s2 : signature): bool =
        if fst s1 = fst s2 then
            let p1 = snd s1
            let p2 = snd s2
            List.fold2(fun s t1 t2 -> s && compare t1 t2) true p1 p2
        else
            false

    let get_signature = function
                            | Rule(s, b) -> s
                            | ConcatenatedRule(s, b, nb) -> s
    
    let to_arg (a: 'a): argument = a.ToString()

    let inc(p1: argument) (p2: argument):result =
        let v1 = asInt p1
        let v2 = asInt p2
        match (v1, v2) with
            | (Some(val1), Some(val2)) -> if val1 + 1 = val2 then True([p1;p2]) else False
            | _ -> False
            
    let sum(p1: argument) (p2: argument) (p3: argument):result =
        let v1 = asInt p1
        let v2 = asInt p2
        let v3 = asInt p3
        match (v1, v2, v3) with
            | (Some(val1), Some(val2), Some(val3)) -> if val1 + val2 = val3 then True([p1;p2;p3]) else False
            | (Some(val1), Some(val2), None) -> True([p1;p2;to_arg(val1+val2)])
            | (Some(val1), None, Some(val3)) -> True([p1;to_arg(val3 - val1);p3])
            | (None, Some(val2), Some(val3)) -> True([to_arg(val3-val2);p2;p3])
            | _ -> False

    let divisors(p1: argument) (p2: argument): result =
        let v1 = asInt p1
        let v2 = asIntList p2
        let getdivs x = {1..x} |> Seq.filter(fun i -> x % i = 0) |> Set.ofSeq
        match (v1, v2) with
        | (Some(val1), Some(val2)) -> if Set.isEmpty(Set.difference (getdivs val1) (Set.ofList val2)) then True([p1; p2]) else False
        | (Some(val1), None) -> True([p1; getdivs val1 |> Set.toList |> List.fold(fun acc x -> acc + x.ToString() + " ") ""])
        | (None, Some(val2)) -> False
        | _ -> False

    let IncR: rule = Rule(("inc", ["A";"B"]), F2(inc))
    let SumR: rule = Rule(("sum", ["A";"B";"C"]), F3(sum))
    let DivsR:rule = Rule(("divs", ["A";"B"]), F2(divisors))
    
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

    let testkb = [Fact("f", ["1"]); Fact("f", ["2"]); IncR; SumR; DivsR]
    let textcontext = Map.empty.Add("A", "1").Add("B", "3").Add("C", "4")
    let test = exec testkb Map.empty ("f", ["D"])