namespace SearchLib

open SearchLib.Signature

module Rule =
    type result = Accepted of arguments | Rejected
    type predicate = 
        | F0 of result
        | F1 of ((argument) -> result) 
        | F2 of (argument -> argument -> result) 
        | F3 of (argument -> argument -> argument -> result)
    
    [<CustomComparison; CustomEquality>]
    type rule = Rule of Definition * f: predicate | ConcatenatedRule of Definition * Call list
        with
        member r.Name = 
            match r with
            | Rule(s, _) -> s.name
            | ConcatenatedRule(s, _) -> s.name
        member r.Parameters = 
            match r with
            | Rule(s, _) -> s.prms
            | ConcatenatedRule(s, _) -> s.prms
        member r.Signature = 
            match r with
            | Rule(s, b) -> s
            | ConcatenatedRule(s, calls) -> s
        static member fact(s: Definition) = 
            let (?=) = Unify.unify
            match s.prms with
            | [] -> Rule(s, F0(Accepted([])))
            | h::[] -> Rule(s, F1(fun arg -> if h ?= arg then Accepted([arg]) else Rejected))
            | h1::h2::[] -> Rule(s, F2(fun arg1 -> fun arg2 -> if h1 ?= arg1 && h2 ?= arg2 then Accepted([arg1;arg2]) else Rejected))
            | h1::h2::h3::[] -> Rule(s, F3(fun arg1 -> fun arg2 -> fun arg3 -> if h1 ?= arg1 && h2 ?= arg2 && h3 ?= arg3 then Accepted([arg1;arg2;arg3]) else Rejected))
            | _ -> failwith("Too many arguments for instantiating a fact.")
        static member create(name: string) (prms: parameters) (p: predicate) = Rule({name = name; prms = prms}, p)
        member r.haveSameSignature (o: rule) = r.Signature.Equals(o.Signature)
        static member CompareSignatures (r1: rule) (r2: rule) = Definition.CompareTo r1.Signature r2.Signature
        interface System.IComparable<rule> with
            member r.CompareTo (o: rule) = rule.CompareSignatures r o
        interface System.IComparable with
            member r.CompareTo (o: System.Object) = 
                match o with
                | :? rule as r1 -> rule.CompareSignatures r r1
                | _ -> failwith "cannot compare rule with any other types"
        interface System.IEquatable<rule> with
            member r.Equals(o) =
                // TODO
                false
                (*match r, o with
                | Rule(s,f), Rule(s1,f1) -> s = s1 // TOOD check predicates && f = f1 -> true
                | ConcatenatedRule(s, ss), ConcatenatedRule(s1, ss1) when s = s1 && ss = ss1 -> true
                | _ -> false*)
    type rulelist = list<rule>
    
    let Fact(name: string) (prms: parameters) = rule.fact {name = name; prms = prms}
    
    let Rule(name: string) (prms: parameters) (p: predicate) = rule.create name prms p
    
    let ConRule(name: string) (prms: parameters) (calls: Call list) = rule.ConcatenatedRule(Signature.define name prms, calls)

    let IncR: rule = 
        let p: parameters =
            [Parameter.create("A", dataType.Integer); Parameter.create("B", dataType.Integer)]
        let inc(p1: argument) (p2: argument):result =
            let v1 = Argument.value p1 |> Option.bind Value.int
            let v2 = Argument.value p2 |> Option.bind Value.int
            match (v1, v2) with
                | (Some(val1), Some(val2)) -> if val1 + 1 = val2 then Accepted([p1;p2]) else Rejected
                | _ -> Rejected
        Rule "inc" p (F2 inc)

    let SumR: rule = 
        let p: parameters =
            [Parameter.create("A", dataType.Integer); Parameter.create("B", dataType.Integer); Parameter.create("C", dataType.Integer)]
        let sum(p1: argument) (p2: argument) (p3: argument):result =
            let v1 = Argument.value p1 |> Option.bind Value.int
            let v2 = Argument.value p2 |> Option.bind Value.int
            let v3 = Argument.value p3 |> Option.bind Value.int
            match (v1, v2, v3) with
                | (Some(val1), Some(val2), Some(val3)) -> if val1 + val2 = val3 then Accepted([p1;p2;p3]) else Rejected
                | (Some(val1), Some(val2), None) -> Accepted([p1;p2;Argument.create(val1 + val2)])
                | (Some(val1), None, Some(val3)) -> Accepted([p1;Argument.create(val3 - val1);p3])
                | (None, Some(val2), Some(val3)) -> Accepted([Argument.create(val3 - val2);p2;p3])
                | _ -> Rejected
        Rule "sum" p (F3 sum)

    let DivsR:rule = 
        let p: parameters =
            [Parameter.create("A", dataType.Integer); Parameter.create("B", dataType.Integer)]
        let divisors(p1: argument) (p2: argument): result =
            let v1 = Argument.value p1 |> Option.bind Value.int
            let v2 = Argument.value p2 |> Option.bind Value.intlist
            let getdivs x = {1..x} |> Seq.filter(fun i -> x % i = 0) |> Set.ofSeq
            let seteq val1 val2 = Set.isEmpty(Set.difference (getdivs val1) (Set.ofList val2))
            match (v1, v2) with
            | (Some(val1), Some(val2)) -> if seteq val1 val2 then Accepted([p1; p2]) else Rejected
            | (Some(val1), None) -> Accepted([p1; Argument.create(getdivs val1 |> Set.toList)])
            | _ -> Rejected
        Rule "divs" p (F2 divisors)

    let defaultRules : rulelist = [IncR; SumR; DivsR]