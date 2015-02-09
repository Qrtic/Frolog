namespace Frolog

// RuleInput is desctructured argument terms
type PredicateInput = PredicateInput of Term list
type PredicateOutput = PredicateOutput of Term list

module RuleInputOutputConvert =
    let inputToOutput (PredicateInput(input)) = PredicateOutput(input)
    let outputToInput (PredicateOutput(output)) = PredicateInput(output)

type PredicateResult = Failed | Success of PredicateOutput

open RuleInputOutputConvert

type RuleBodyLexem =
    | True
    | False
    | Call of Signature
    | Predicate of (PredicateInput -> PredicateResult)
type RuleBody = 
    // Simple success verifier
    | Lexem of RuleBodyLexem
    // Execute second if first succeed
    | Conjunction of RuleBody * RuleBody
    // Execute first and then second
    | Or of RuleBody * RuleBody
    // Execute only this
    | Cut of RuleBody
    // If fails then succeed
    | Not of RuleBody
and Rule = Rule of definition: Signature * body: RuleBody * isInternal: bool

type Rule with
    member r.Signature = 
        let (Rule(def, _, _)) = r
        def

// Rule is only a structure with defined constraints
// It can be fully defined aka f(1).
// Or partially defined f(1, X).
// And can be constrained by f(1, X) :- X > 3.
module DefineRule =
    let internal _defFact sign = Rule(sign, Lexem(True), true)
    let defFact sign = Rule(sign, Lexem(True), false)

    let internal _tryDefFact t =
        match Option.bind sign (term t) with
        | None -> None
        | Some(sign) -> Some(Rule(sign, Lexem(True), true))
    let tryDefFact t =
        match Option.bind sign (term t) with
        | None -> None
        | Some(sign) -> Some(Rule(sign, Lexem(True), false))
        
    let internal _defConcatRule term body = Rule(term, body, true)
    let defConcatRule term body = Rule(term, body, false)
    
    let internal _defOrRule term body1 body2 = Rule(term, Or(body1, body2), true)
    let internal defOrRule term body1 body2 = Rule(term, Or(body1, body2), false)

    let rec internal _defBody calllist =
        match calllist with
        | [] -> Lexem(True)
        | [h] -> Lexem(Call(h))
        | h::t -> Conjunction(Lexem(Call h), _defBody t)
    let rec defBody calllist =
        match calllist with
        | [] -> Lexem(True)
        | [h] -> Lexem(Call(h))
        | h::t -> Conjunction(Lexem(Call h), _defBody t)
        
    let internal _defPredicate term inputConverter predicate =
        let inputConvert input convert predicate =
            let (PredicateInput(arguments)) = input
            if List.length arguments = List.length convert then
                let convertedArgs = List.map2(fun conv x -> conv(Term.tryGetValue x)) convert arguments
                predicate convertedArgs
            else
                Failed
        Rule(Signature(term), Lexem(Predicate(fun input -> inputConvert input inputConverter predicate)), true)
    let defPredicate term inputConverter predicate =
        let inputConvert input convert predicate =
            let (PredicateInput(arguments)) = input
            if List.length arguments = List.length convert then
                let convertedArgs = List.map2(fun conv x -> conv(Term.tryGetValue x)) convert arguments
                predicate convertedArgs
            else
                Failed
        Rule(Signature(term), Lexem(Predicate(fun input -> inputConvert input inputConverter predicate)), false)

    let internal _defUnify term =
        let unif input =
            match input with
            | PredicateInput([t]) -> 
                match Term.tryUnify term t with
                | Some(unified) -> Success(PredicateOutput[unified])
                | None -> Failed
            | _ -> Failed
        Rule(Signature(term), Lexem(Predicate(unif)), true)
        
    [<AutoOpen>]
    module Converters =
        let convertInt = Option.bind(fun x -> 
                    let ok, v = System.Int32.TryParse x
                    if ok then Some(v) else None)

        // TODO: implement
        let convertIntList x = failwith "Not implemented converter: int list"

    let inline success (list: System.Object list) =
        let inline create t = termf (t.ToString())
        Success(PredicateOutput(List.map create list))
        
    module internal StandartPredicates =
        // What is good:
        // We have nice and compact definition
        // What is bad:
        // We have no unification
        // We have no constraints

        let defEq =
            _defPredicate (termf "=(X, Y)") [convertInt; convertInt] (function
                | [Some(a); Some(b)] -> if a = b then success [a; b] else Failed
                | [Some(a); None] -> success [a; a]
                | [None; Some(b)] -> success [b; b]
                | _ -> Failed)
            
        let defGr =
            _defPredicate (termf ">(X, Y)") [convertInt; convertInt] (function
                | [Some(a); Some(b)] -> if a > b then success [a; b] else Failed
                | _ -> Failed)
            
        let defInc =
            _defPredicate (termf "++(X, Y)") [convertInt; convertInt] (function
                    | [Some(x);Some(y)] when x + 1 = y -> success [x; y]
                    | [Some(x); None] -> success [x; x+1]
                    | [None; Some(y)] -> success [y-1; y]
                    | _ -> Failed
                )
        
        let defDec =
            _defPredicate (termf "--(X, Y)") [convertInt; convertInt] (function
                    | [Some(x); Some(y)] when x - 1 = y -> success [x; y]
                    | [Some(x); None] -> success [x; x-1]
                    | [None; Some(y)] -> success [y=1; y]
                    | _ -> Failed
                )
        
        let defSum =
            _defPredicate (termf "+(A, B, C)") [convertInt; convertInt; convertInt] (function
                | [Some(a); Some(b); Some(c)] when a + b = c -> success [a; b; c]
                | [Some(a); Some(b); None] -> success[a; b; a + b]
                | [Some(a); None; Some(c)] -> success[a; c - a; c]
                | [None; Some(b); Some(c)] -> success[c - b; b; c]
                | _ -> Failed
            )
        
        let defSub =
            _defPredicate (termf "-(A, B, C)") [convertInt; convertInt; convertInt] (function
                | [Some(a); Some(b); Some(c)] when a - b = c -> success [a; b; c]
                | [Some(a); Some(b); None] -> success[a; b; a - b]
                | [Some(a); None; Some(c)] -> success[a; c + a; c]
                | [None; Some(b); Some(c)] -> success[c + b; b; c]
                | _ -> Failed
            )
        
        let defMul =
            _defPredicate (termf "*(A, B, C)") [convertInt; convertInt; convertInt] (function
                | [Some(a); Some(b); Some(c)] when a * b = c -> success [a; b; c]
                | [Some(a); Some(b); None] -> success[a; b; a * b]
                | [Some(a); None; Some(c)] -> success[a; c / a; c]
                | [None; Some(b); Some(c)] -> success[c / b; b; c]
                | _ -> Failed
            )

        let defDiv =
            _defPredicate (termf "/(A, B, C)") [convertInt; convertInt; convertInt] (function
                | [Some(a); Some(b); Some(c)] when a / b = c -> success [a; b; c]
                | [Some(a); Some(b); None] -> success[a; b; a / b]
                | [Some(a); None; Some(c)] -> success[a; a / c; c]
                | [None; Some(b); Some(c)] -> success[c * b; b; c]
                | _ -> Failed
            )

        let defDivs =
            _defPredicate (termf "divs(A, B)") [convertInt; convertIntList] (function
                | _ -> Failed)

    open StandartPredicates
    let standartRules = [defInc; defDec; defSum; defSub; defMul; defDiv; defEq; defGr]