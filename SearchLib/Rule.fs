namespace SearchLib

module Rule =
    type result = Accepted of arguments | Rejected
    type predicate = 
        | F0 of result
        | F1 of ((argument) -> result) 
        | F2 of (argument -> argument -> result) 
        | F3 of (argument -> argument -> argument -> result)
    
    [<CustomComparison; CustomEquality>]
    [<StructuredFormatDisplay("{Signature}")>]
    type rule = Fact of Definition | Rule of Definition * f: predicate | ConcatenatedRule of Definition * Call list
        with
        member r.Name = 
            match r with
            | Fact(def) -> def.name
            | Rule(def, _) -> def.name
            | ConcatenatedRule(def, _) -> def.name
        member r.Parameters = 
            match r with
            | Fact(def) -> def.prms
            | Rule(def, _) -> def.prms
            | ConcatenatedRule(def, _) -> def.prms
        member r.Signature = 
            match r with
            | Fact(def) -> def
            | Rule(def, b) -> def
            | ConcatenatedRule(def, calls) -> def
        static member fact(def: Definition) = Fact(def)
        static member create(name: string) (prms: parameters) (p: predicate) = Rule({name = name; prms = prms}, p)
        member r.haveSameSignature (o: rule) = r.Signature.Equals(o.Signature)
        static member CompareSignatures (r1: rule) (r2: rule) = Definition.CompareTo r1.Signature r2.Signature
        member r.Equals(r1: rule) =
            let signaturesEq = Definition.StrongEquals(r.Signature, r1.Signature)
            match r, r1 with
            | Fact(def), Fact(def1) -> Definition.StrongEquals(def, def1)
            | Rule(d, p), Rule(d1, p1) -> false // TODO: how to define? p = p1
            | ConcatenatedRule(d, p), ConcatenatedRule(d1, p1) -> signaturesEq && p = p1
            | _ -> false
        override r.Equals(o) = 
            match o with
            | :? rule as r1 -> r.Equals(r1)
            | _ -> false
        interface System.IComparable<rule> with
            member r.CompareTo (o: rule) = rule.CompareSignatures r o
        interface System.IComparable with
            member r.CompareTo (o: System.Object) = 
                match o with
                | :? rule as r1 -> rule.CompareSignatures r r1
                | _ -> failwith "cannot compare rule with any other types"
        override r.GetHashCode() = System.Guid.NewGuid().GetHashCode() // r.Signature.AsString.GetHashCode()
        member r.AsString = r.ToString()
        override r.ToString() = sprintf "Rule = %s" r.Signature.AsString
    type rulelist = list<rule>
    
    let Fact(name: string) (prms: parameters) = rule.fact {name = name; prms = prms}
    
    let Rule(name: string) (prms: parameters) (p: predicate) = rule.create name prms p
    
    let ConRule(name: string) (prms: parameters) (calls: Call list) = rule.ConcatenatedRule(Signature.define(name, prms), calls)
    
    let IncR: rule = 
        let p: parameters =
            [Parameter.create "A"; Parameter.create "B"]
        let inc(p1: argument) (p2: argument):result =
            let v1 = Argument.getValue p1 |> Option.bind Value.int
            let v2 = Argument.getValue p2 |> Option.bind Value.int
            match (v1, v2) with
                | (Some(val1), Some(val2)) -> if val1 + 1 = val2 then Accepted([p1;p2]) else Rejected
                | Some(val1), None -> Accepted([p1; Argument.create(val1 + 1)])
                | None, Some(val2) -> Accepted([Argument.create(val2 - 1); p2])
                | _ -> Rejected
        Rule "inc" p (F2 inc)
        
    let DecR: rule = 
        let p: parameters =
            [Parameter.create "A"; Parameter.create "B"]
        let inc(p1: argument) (p2: argument):result =
            let v1 = Argument.getValue p1 |> Option.bind Value.int
            let v2 = Argument.getValue p2 |> Option.bind Value.int
            match (v1, v2) with
                | (Some(val1), Some(val2)) -> if val1 - 1 = val2 then Accepted([p1;p2]) else Rejected
                | Some(val1), None -> Accepted([p1; Argument.create(val1 - 1)])
                | None, Some(val2) -> Accepted([Argument.create(val2 + 1); p2])
                | _ -> Rejected
        Rule "dec" p (F2 inc)

    let SumR: rule = 
        let p: parameters =
            [Parameter.create "A"; Parameter.create "B"; Parameter.create "C"]
        let sum(p1: argument) (p2: argument) (p3: argument):result =
            let v1 = Argument.getValue p1 |> Option.bind Value.int
            let v2 = Argument.getValue p2 |> Option.bind Value.int
            let v3 = Argument.getValue p3 |> Option.bind Value.int
            match (v1, v2, v3) with
                | (Some(val1), Some(val2), Some(val3)) -> if val1 + val2 = val3 then Accepted([p1;p2;p3]) else Rejected
                | (Some(val1), Some(val2), None) -> Accepted([p1;p2;Argument.create(val1 + val2)])
                | (Some(val1), None, Some(val3)) -> Accepted([p1;Argument.create(val3 - val1);p3])
                | (None, Some(val2), Some(val3)) -> Accepted([Argument.create(val3 - val2);p2;p3])
                | _ -> Rejected
        Rule "sum" p (F3 sum)

    let DivsR:rule = 
        let p: parameters =
            [Parameter.create "A"; Parameter.create "B"]
        let divisors(p1: argument) (p2: argument): result =
            let v1 = Argument.getValue p1 |> Option.bind Value.int
            let v2 = Argument.getValue p2 |> Option.bind Value.intList
            let getdivs x = {1..x} |> Seq.filter(fun i -> x % i = 0) |> Set.ofSeq
            let seteq val1 val2 = Set.isEmpty(Set.difference (getdivs val1) (Set.ofList val2))
            match (v1, v2) with
            | (Some(val1), Some(val2)) -> if seteq val1 val2 then Accepted([p1; p2]) else Rejected
            | (Some(val1), None) -> Accepted([p1; Argument.create(getdivs val1 |> Set.toList)])
            | _ -> Rejected
        Rule "divs" p (F2 divisors)

    let MulR:rule =
        let p = [Parameter.create "A"; Parameter.create "B"; Parameter.create "M"]
        let f a b m =
            let v1 = Argument.getValue a |> Option.bind Value.int
            let v2 = Argument.getValue b |> Option.bind Value.int
            let v3 = Argument.getValue m |> Option.bind Value.int
            match (v1, v2, v3) with
            | Some(val1), Some(val2), Some(val3) -> if val1 * val2 = val3 then Accepted([a;b;m]) else Rejected
            | Some(val1), Some(val2), None -> Accepted([a;b;Argument.create(val1 * val2)])
            | Some(val1), None, Some(val3) -> Accepted([a;Argument.create(val3/val1);m])
            | None, Some(val2), Some(val3) -> Accepted([Argument.create(val3/val2); b; m])
            | _ -> Rejected
        Rule "mul" p (F3 f)

    let GrR: rule =
        let p = [Parameter.create "A"; Parameter.create "B"]
        let f a b =
            let v1 = Argument.getValue a |> Option.bind Value.int
            let v2 = Argument.getValue b |> Option.bind Value.int
            match v1, v2 with
            | Some(v1), Some(v2) -> if v1 > v2 then Accepted([a;b]) else Rejected
            | _ -> Rejected
        Rule "greater" p (F2 f)

    let defaultRules : rulelist = [IncR; DecR; SumR; DivsR; MulR; GrR]