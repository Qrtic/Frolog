﻿namespace Frolog

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
    | Cut
    | Call of Signature
    | Predicate of (PredicateInput -> PredicateResult)
    with
    override l.ToString() =
        match l with
        | True -> "true"
        | False -> "false"
        | Cut -> "!"
        | Call s -> s.AsString
        | Predicate(_) -> "``P``"
type RuleBody = 
    // Simple success verifier
    | Lexem of RuleBodyLexem
    // Execute second if first succeed
    | Conjunction of RuleBody * RuleBody
    // Execute first and then second
    | Or of RuleBody * RuleBody
    // If fails then succeed
    | Not of RuleBody
    with
    override b.ToString() = 
        match b with
        | Lexem(l) -> l.ToString()
        | Conjunction(b1, b2) -> sprintf "%s, %s" (b1.ToString()) (b2.ToString())
        | Or(b1, b2) -> sprintf "%s, %s" (b1.ToString()) (b2.ToString())
        | Not(b) -> sprintf "not(%s)" (b.ToString())
and
    [<StructuredFormatDisplay("{AsString}")>]
    Rule = Rule of definition: Signature * body: RuleBody * isInternal: bool
    with
    override r.ToString() =
        let (Rule(d, b, isInternal)) = r
        sprintf "%s => %s %s" (d.AsString) (b.ToString()) (if isInternal then "| internal" else "")
    member r.AsString = r.ToString()

type Rule with
    member r.Signature = 
        let (Rule(def, _, _)) = r
        def
    member r.Body =
        let (Rule(_, body, _)) = r
        body

// Rule is only a structure with defined constraints
// It can be fully defined aka f(1).
// Or partially defined f(1, X).
// And can be constrained by f(1, X) :- X > 3.
module DefineRule =
    /// Concat new body to the end
    /// b -> b1, b2 -> b1, b2, b
    /// b -> b1; b2 -> b1; b2, b
    /// b -> ! -> b, !
    /// b -> b1 -> b1, not(b)
    let rec combine body rule =
        let rec combineBody body1 body2 =
            match body1 with
            | Lexem(_) -> Conjunction(body1, body2)
            | Conjunction(b1, b2) -> Conjunction(b1, combineBody b2 body2)
            | Or(b1, b2) -> Or(b1, Conjunction(b2, body))
            | Not(b) -> Conjunction(body1, body2)
        let (Rule(def, b, isInternal)) = rule
        Rule(def, combineBody b body, isInternal)
    let orJoin def bodies =
        Rule(def, List.reduce(fun b1 b2 -> Or(b1, b2)) bodies, false)

    module internal DefInternal =
        let defCall sign call isInternal = Rule(sign, Lexem(Call call), isInternal)
        let defFact sign isInternal = Rule(sign, Lexem(True), isInternal)
        let tryDefFact t isInternal =
            match Option.bind sign (term t) with
            | None -> None
            | Some(sign) -> Some(Rule(sign, Lexem(True), isInternal)) 
        let defConjunction sign body1 body2 isInternal = Rule(sign, body1, isInternal) |> combine body2
        let defOr sign body1 body2 isInternal = Rule(sign, Or(body1, body2), isInternal)
        let defPredicate term inputConverter predicate isInternal =
            let inputConvert input convert predicate =
                let (PredicateInput(arguments)) = input
                if List.length arguments = List.length convert then
                    let convertedArgs = List.map2(fun conv x -> conv(Term.tryGetValue x)) convert arguments
                    predicate convertedArgs
                else
                    Failed
            Rule(Signature(term), Lexem(Predicate(fun input -> inputConvert input inputConverter predicate)), isInternal)
        let defCut sign body isInternal = Rule(sign, Conjunction(body, Lexem(Cut)), isInternal)
        let defNot sign body isInternal = Rule(sign, Not(body), isInternal)

    module internal DefAsInternal =
        open DefInternal
        let defFact sign = defFact sign true
        let tryDefFact t = tryDefFact t true
        let defCall sign call = defCall sign call true
        let defConjunction term body1 body2 = defConjunction term body1 body2 true
        let defOr term body1 body2 = defOr term body1 body2 true
        let defPredicate term inputConverter predicate = defPredicate term inputConverter predicate true
        let defCut sign body isInternal = defCut sign body true
        let defNot sign body isInternal = defNot sign body true

    module public DefPublic =
        open DefInternal
        let callBody call = Lexem(Call(signf call))
        let defFact sign = defFact sign false
        let tryDefFact t = tryDefFact t false
        let defCall sign call = defCall sign call false
        let defConjunction term body1 body2 = defConjunction term body1 body2 false
        let defOr term body1 body2 = defOr term body1 body2 false
        let defPredicate term inputConverter predicate = defPredicate term inputConverter predicate false
        let defCut sign body isInternal = defCut sign body false
        let defNot sign body isInternal = defNot sign body false

    [<AutoOpen>]
    module public DefPublicDirectOperators =
        open DefInternal
        /// Define fact
        let (!) d = defFact (signf d) false
        /// Define def => call
        let (=>) d c = defCall (signf d) (signf c) false
        let (|!) r b = match b with | false -> r |> combine (Conjunction(Lexem(Cut), (Lexem(False)))) | true -> r |> combine (Conjunction(Lexem(Cut), (Lexem(True))))
        /// Combine rule body with new call (conjunction)
        let (|&) r c = combine (Lexem(Call(signf c))) r
        // Combines body or another body
        // let (|*) b1 b2 = orCombine (Lexem(Call(signf c))) r

//    let internal _defConcatRule term body = Rule(term, body, true)
//    let defConcatRule term body = Rule(term, body, false)

//    let rec internal _defBody calllist =
//        match calllist with
//        | [] -> Lexem(True)
//        | [h] -> Lexem(Call(h))
//        | h::t -> Conjunction(Lexem(Call h), _defBody t)
//    let rec defBody calllist =
//        match calllist with
//        | [] -> Lexem(True)
//        | [h] -> Lexem(Call(h))
//        | h::t -> Conjunction(Lexem(Call h), _defBody t)

//    let internal _defUnify term =
//        let unif input =
//            match input with
//            | PredicateInput([t]) -> 
//                match Term.tryUnify term t with
//                | Some(unified) -> Success(PredicateOutput[unified])
//                | None -> Failed
//            | _ -> Failed
//        Rule(Signature(term), Lexem(Predicate(unif)), true)
        
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
        open DefAsInternal
    
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
        
        let defSub =
            defPredicate (termf "-(A, B, C)") [convertInt; convertInt; convertInt] (function
                | [Some(a); Some(b); Some(c)] when a - b = c -> success [a; b; c]
                | [Some(a); Some(b); None] -> success[a; b; a - b]
                | [Some(a); None; Some(c)] -> success[a; c + a; c]
                | [None; Some(b); Some(c)] -> success[c + b; b; c]
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
    let standartRules = [defInc; defDec; defSum; defSub; defMul; defDiv; defEq; defGr]