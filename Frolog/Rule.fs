namespace Frolog

// RuleInput is desctructured argument terms
type RuleInput = RuleInput of Term list
type RuleOutput = RuleOutput of Term list

module RuleInputOutputConvert =
    let inputToOutput (RuleInput(input)) = RuleOutput(input)
    let outputToInput (RuleOutput(output)) = RuleInput(output)

type PredicateResult = Failed | Success of RuleOutput

open RuleInputOutputConvert

type RuleBody = True | False | Predicate of (RuleInput -> PredicateResult) | Single of Signature | Continuation of Signature * RuleBody
and Rule = Rule of definition: Signature * body: RuleBody

type Rule with
    member r.Signature = 
        let (Rule(def, _)) = r
        def

module DefineRule =
    // Rule is only a structure with defined constraints
    // It can be fully defined aka f(1).
    // Or partially defined f(1, X).
    // And can be constrained by f(1, X) :- X > 3.
    let defFact sign = Rule(sign, True)
    let tryDefFact t =
        match Option.bind sign (term t) with
        | None -> None
        | Some(sign) -> Some(Rule(sign, True))

    let defConcatRule term body = Rule(term, body)
    
    let rec defBody calllist =
        match calllist with
        | [] -> True
        | [h] -> Single(h)
        | h::t -> Continuation(h, defBody t)

    let defPredicate term inputConverter predicate =
        let inputConvert input convert predicate =
            let (RuleInput(arguments)) = input
            if List.length arguments = List.length convert then
                let convertedArgs = List.map2(fun conv x -> conv(Term.tryGetValue x)) convert arguments
                predicate convertedArgs
            else
                Failed
        Rule(Signature(term), Predicate(fun input -> inputConvert input inputConverter predicate))

    let defUnify term =
        let unif (input: RuleInput): PredicateResult =
            match input with
            | RuleInput([t]) -> 
                match Term.tryUnify term t with
                | Some(unified) -> Success(RuleOutput[unified])
                | None -> Failed
            | _ -> Failed
        Rule(Signature(term), Predicate(unif))
        
    [<AutoOpen>]
    module Converters =
        let convertInt = Option.bind(fun x -> 
                    let ok, v = System.Int32.TryParse x
                    if ok then Some(v) else None)

        // TODO: implement
        let convertIntList x = failwith "Not implemented converter: int list"

    let inline success (list: System.Object list) =
        let inline create t = termf (t.ToString())
        Success(RuleOutput(List.map create list))
        
    module StandartPredicates =
        // What is good:
        // We have nice and compact definition
        // What is bad:
        // We have no unification
        // We have no constraints

        let defEq =
            defPredicate (termf "=(X, Y)") [convertInt; convertInt] (function
                | [Some(a); Some(b)] -> if a = b then success [a; b] else Failed
                | [Some(a); None] -> success [a; a]
                | [None; Some(b)] -> success [b; b]
                | _ -> Failed)
            
        let defGr =
            defPredicate (termf ">(X, Y)") [convertInt; convertInt] (function
                | [Some(a); Some(b)] -> if a > b then success [a; b] else Failed
                | _ -> Failed)
            
        let defInc =
            defPredicate (termf "++(X, Y)") [convertInt; convertInt] (function
                    | [Some(x);Some(y)] when x + 1 = y -> success [x; y]
                    | [Some(x); None] -> success [x; x+1]
                    | [None; Some(y)] -> success [y-1; y]
                    | _ -> Failed
                )
        
        let defDec =
            defPredicate (termf "--(X, Y)") [convertInt; convertInt] (function
                    | [Some(x); Some(y)] when x - 1 = y -> success [x; y]
                    | [Some(x); None] -> success [x; x-1]
                    | [None; Some(y)] -> success [y=1; y]
                    | _ -> Failed
                )
        
        let defSum =
            defPredicate (termf "+(A, B, C)") [convertInt; convertInt; convertInt] (function
                | [Some(a); Some(b); Some(c)] when a + b = c -> success [a; b; c]
                | [Some(a); Some(b); None] -> success[a; b; a + b]
                | [Some(a); None; Some(c)] -> success[a; c - a; c]
                | [None; Some(b); Some(c)] -> success[c - b; b; c]
                | _ -> Failed
            )
        
        let defMul =
            defPredicate (termf "*(A, B, C)") [convertInt; convertInt; convertInt] (function
                | [Some(a); Some(b); Some(c)] when a * b = c -> success [a; b; c]
                | [Some(a); Some(b); None] -> success[a; b; a * b]
                | [Some(a); None; Some(c)] -> success[a; c / a; c]
                | [None; Some(b); Some(c)] -> success[c / b; b; c]
                | _ -> Failed
            )

        let defDiv =
            defPredicate (termf "/(A, B, C)") [convertInt; convertInt; convertInt] (function
                | [Some(a); Some(b); Some(c)] when a / b = c -> success [a; b; c]
                | [Some(a); Some(b); None] -> success[a; b; a / b]
                | [Some(a); None; Some(c)] -> success[a; a / c; c]
                | [None; Some(b); Some(c)] -> success[c * b; b; c]
                | _ -> Failed
            )

        let defDivs =
            defPredicate (termf "divs(A, B)") [convertInt; convertIntList] (function
                | _ -> Failed)

    open StandartPredicates
    let standartRules = [defInc; defDec; defSum; defMul; defDiv; defEq; defGr]