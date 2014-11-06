namespace SearchLib

module Search =
    type name = string
    type value = string
    type parameter = name
    type argument = value
    type parameters = list<parameter>
    type signature = name * parameters
    type context = Map<parameter, value>

    type predicate = F0 of bool | F1 of ((argument) -> bool) | F2 of (argument -> argument -> bool) | F3 of (argument -> argument -> argument -> bool)
    type rule = Rule of signature * f: predicate | ConcatenatedRule of signature * predicate * rule
    let Fact(s: signature) = match (snd s) with
                                | [] -> Rule(s, F0(true))
                                | h::[] -> Rule(s, F1(fun arg -> arg = h))
                                | h1::h2::[] -> Rule(s, F2(fun arg1 -> fun arg2 -> arg1 = h1 && arg2 = h2))
                                | h1::h2::h3::[] -> Rule(s, F3(fun arg1 -> fun arg2 -> fun arg3 -> arg1 = h1 && arg2 = h2 && arg3 = h3))

    type rulelist = list<rule>
    type knowledgebase = rulelist

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

    let asString (p: argument): option<string> = Some(p) // mb not always?!

    let convert_to_arg(c: context) (p: parameter): argument = 
        if isVar p then c.TryFind(p).Value // Think that we always have this p
        else p            

    let compare (p1: parameter) (p2: parameter): bool =
        let v1 = isVar p1
        let v2 = isVar p2
        if v1 || v2 then true
        else v1 = v2 // All non vars are constants that are defined as name that also can be used as value

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
    
    let inc(p1: argument) (p2: argument):bool =
        let v1 = asInt p1
        let v2 = asInt p2
        match (v1, v2) with
            | (Some(val1), Some(val2)) -> val1 + 1 = val2
            | _ -> false

    let IncR: rule = Rule(("inc", ["A";"B"]), F2(inc))
    
    let process_predicate(c: context) (s: signature) (comp: predicate) =
        let check_parameters_count(s: signature) (comp: predicate): bool = 
            let (n, prms) = s
            match comp with
                | F0(f0) -> prms.Length = 0
                | F1(f1) -> prms.Length = 1
                | F2(f2) -> prms.Length = 2
        if not (check_parameters_count s comp) then false
        else
            let prms = snd s |> List.map (convert_to_arg c)
            match comp with
                | F0(f0) -> f0
                | F1(f1) -> f1 prms.Head
                | F2(f2) -> f2 prms.Head prms.Tail.Head

    let rec find (d: knowledgebase) (c: context) (s: signature) : bool = 
        let find (r: rule) = find d c (get_signature r)
        d
        |> List.filter(fun r -> r |> get_signature |> signatureEq s)
        |> List.exists(fun r -> match r with
                                    | Rule(_, p) -> process_predicate c s p
                                    | ConcatenatedRule(_, p, next_r) -> process_predicate c s p && find next_r
                                    | _ -> false)

    let testkb = [Fact("f", ["1"]); Fact("f", ["2"]); IncR]
    let textcontext = Map.empty.Add("A", "1").Add("B", "3")
    let test = find testkb textcontext ("inc", ["A"; "B"])
    let dotest = printfn "%b" test