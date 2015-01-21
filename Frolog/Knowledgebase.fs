namespace Frolog

open Frolog.Common

type Rulebase =
    abstract member Append: Rule -> Rulebase
    abstract member AppendRange: Rule seq -> Rulebase
    abstract member Rules: Rule list with get

type Knowledgebase(rules: Rule list) =
    static member Empty = new Knowledgebase([])
    static member Default = new Knowledgebase(DefineRule.standartRules)

    member k.rules = rules
    interface Rulebase with
        member k.Append r = new Knowledgebase(r::k.rules) :> Rulebase
        member k.AppendRange rs = (k, rs) ||> Seq.fold(fun kb r -> new Knowledgebase(r::kb.rules)) :> Rulebase
        member k.Rules = k.rules
    
type ISearcher =
    abstract Search: Rulebase -> Context -> Signature -> RuleOutput seq

open ContextHelper

module Search =    
    let rec search knowledgebase context call =
        let kb = knowledgebase :> Rulebase
        // Stages

        let sign name args = Signature(Structure(name, args))
        
        let canApply def call =
            let defa = Signature.GetArguments def
            let calla = Signature.GetArguments call
            let anyVar t1 t2 = Term.IsVariableTerm t1 || Term.IsVariableTerm t2
            List.forall2 (fun t1 t2 ->
                match Term.tryUnify t1 t2 with
                | Some(Value(v)) when anyVar t1 t2 -> true
                | Some(Structure(f, args)) as s when anyVar t1 t2 -> true
                | _ -> false) defa calla

        // internal substitution (change variables to arguments)
        let internalSubstitute def call subCall =
            let args = Signature.GetArguments
            let defa = args def
            let calla = args call
            let acalla = args subCall

            let variableValues = 
                let anyVar t1 t2 = Term.IsVariableTerm t1 || Term.IsVariableTerm t2
                let getVarName t1 t2 = 
                    match Term.GetVariableName t1, Term.GetVariableName t2 with
                    | Some(n), _ -> n
                    | _, Some(n) -> n
                    | _ -> failwith "Core error"

                List.map2 (fun t1 t2 ->
                    match Term.tryUnify t1 t2 with
                    | Some(Value(v)) when anyVar t1 t2 -> Some(getVarName t1 t2, Value(v))
                    | Some(Structure(f, args)) as s when anyVar t1 t2 -> Some(getVarName t1 t2, Structure(f, args))
                    | _ -> None) defa calla |> List.choose identity
            let substituted = acalla |> List.map(fun t ->
                match Term.GetVariableName t with
                    | None -> t
                    | Some(name) -> 
                        match List.tryFind(fun (vname, _) -> vname = name) variableValues with
                        | None -> t
                        | Some(vname, vvalue) -> vvalue)
            Signature.Signature(Structure(Signature.GetName subCall, substituted))

        // Changes all bodyies in forward order
        let rec substituteBody def call body =
            match body with
            | True | False | Predicate(_) -> body
            | Single(bodyS) -> Single(internalSubstitute def call bodyS)
            | Continuation(bodyS, cont) ->
                let cur = internalSubstitute def call bodyS
                Continuation(cur, substituteBody def call cont)

        let rec backSubstitute body proced call =
            match body with
            | True | False | Predicate(_) -> proced
            | Single(bodyS) -> internalSubstitute bodyS proced call
            | Continuation(bodyS, cont) ->
                let cur = internalSubstitute bodyS proced call
                backSubstitute cont proced cur

        // call
        seq {
            // search for suitable predicate
            for Rule(def, body) in kb.Rules do
            // match call to predicate (change parameters with arguments)
            let matchedRule = ContextHelper.unifySignatures call def context
            
            match matchedRule with
            | None -> ()
            | Some(signature, _) ->
                let rec procBody body = 
                    match body with
                    | False -> 
                        [PredicateResult.Failed] |> List.toSeq
                    | True -> 
                        let res = [Success(RuleOutput(Signature.GetArguments signature))]
                        res |> List.toSeq
                    | Predicate(p) -> 
                        let res = [p(RuleInput(Signature.GetArguments signature))]
                        res |> List.toSeq
                    // internal substitution (change variables to arguments)
                    | Single(ruleSign) ->                   
                        let substitutedCall = internalSubstitute def call ruleSign
                        let res = search knowledgebase context substitutedCall
                        res |> Seq.map(fun s -> Success(s))
                    | Continuation(ruleSign, cont) -> 
                        if not <| canApply def call then
                            Seq.empty
                        else
                            let sign = sign (Signature.GetName ruleSign)
                            // apply
                            let appCurrent = internalSubstitute def call ruleSign
                            // evaluate
                            let evCurrent = search knowledgebase context appCurrent

                            seq {
                                for RuleOutput(current) in evCurrent do
                                    // apply inner
                                    let appInner = sign current
                                    let appInnerBody = substituteBody ruleSign appInner cont
                                    // evaluate inner
                                    let evInner = procBody appInnerBody
                                    for inner in evInner do
                                        match inner with
                                        | PredicateResult.Failed -> ()
                                        | Success(RuleOutput(predRes)) ->
                                            // backapply
                                            let bappCurrent = backSubstitute cont (sign predRes) appInner
                                            // evaluate second time to check all constaints
                                            let evbapCurrent = search knowledgebase context bappCurrent
                                            // return result
                                            for evbap in evbapCurrent do
                                                yield PredicateResult.Success(evbap)
                            }
                for pb in procBody body do
                    match pb with
                    | PredicateResult.Failed -> ()
                    | Success(RuleOutput(sign)) -> yield RuleOutput(sign)
        }
        // recursively call internal calls with autosubstitution of next calls
        // return result signature

type SimpleSearcher() =
    interface ISearcher with
        member __.Search rb c call = Search.search rb c call

type DebugInfoSearcher() =
    interface ISearcher with
        member s.Search rb c call = 
            debug (sprintf "Called rule %s" call.AsString)
            let found = (new SimpleSearcher() :> ISearcher).Search rb c call
            if (Seq.isEmpty found) then
                debug "No rules match."
            else
                for r in found do
                    debug (sprintf "Found %s" <| r.ToString())
            found

module Execute =
    let exec (f: ISearcher) (d: Rulebase) (start: Context) (s: Signature): unit =
        printfn "Execute: %s. Context = %s" s.AsString (start.ToString())
        let res = f.Search d start s
        if Seq.isEmpty res then
            printfn "Result: %b." false
        else
            for r in res do
                printfn "Result: %b. New context = %s" (true) ((r).ToString())
                