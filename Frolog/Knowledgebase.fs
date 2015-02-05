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
    
type SearchResult = Signature seq

type ISearcher =
    abstract Search: Rulebase -> Signature -> SearchResult
    
module Search =    
    let rec search (searcher: #ISearcher) knowledgebase call: Signature seq =
        let kb = knowledgebase :> Rulebase
        // Stages

        let sign name args = Signature(Structure(name, args))
        
        let canApply def call =
            let defa = Signature.GetArguments def
            let calla = Signature.GetArguments call
            let isvar = Term.IsVariableTerm
            let anyVar t1 t2 = Term.IsVariableTerm t1 || Term.IsVariableTerm t2
            List.forall2 (fun t1 t2 ->
                match Term.tryUnify t1 t2 with
                | Some(Value(v)) when anyVar t1 t2 -> true
                | Some(Structure(f, args)) as s when anyVar t1 t2 -> true
                | _ when isvar t1 && isvar t2 -> true
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
            | Continuation(bodyS, cont) ->
                let cur = internalSubstitute def call bodyS
                Continuation(cur, substituteBody def call cont)

        // Matches True, True
        // or Predicate and Continuation(signature, True)
        // or Continuation, Continuation of same deep level
        let rec backSub def defBody resBody =
            match defBody, resBody with
            | True, True -> Some def
            | Predicate(_), Continuation(s, True) -> Some s
            | Continuation(bs1, c1), Continuation(bs2, c2) ->
                match unifySignatures bs1 bs2 with
                | None -> None
                | Some(s) ->
                    let apDef = internalSubstitute bs1 bs2 def
                    backSub apDef c1 c2
            | _ -> None

        let checkRule(Rule(def, body, isInternal)) =
            let matchedRule = unifySignatures call def
            match matchedRule with
            | None -> Seq.empty
            | Some(signature) ->
                let name = Signature.GetName signature
                let bodyToSignature def defBody resBody =
                    backSub def defBody resBody
                let bodyToSignature def defBody resBody =
                    backSub def defBody resBody

                let rec procBody body = 
                    match body with
                    | False -> Seq.empty
                    | True -> Seq.singleton(True)
                    | Predicate(p) -> 
                        match p (PredicateInput(Signature.GetArguments signature)) with
                        | Failed -> Seq.empty
                        | Success(PredicateOutput(pout)) ->
                            Seq.singleton(Continuation(sign name pout, True))
                    | Continuation(ruleSign, cont) -> 
                        if not <| canApply def call then
                            Seq.empty
                        else
                            // apply
                            let appCurrent = internalSubstitute def call ruleSign
                            // evaluate
                            let evCurrent =
                                if isInternal then
                                    search searcher knowledgebase appCurrent
                                else
                                    searcher.Search knowledgebase appCurrent

                            let applyCur appInner =
                                let appInnerBody = substituteBody ruleSign appInner cont
                                // evaluate inner
                                let evInner = procBody appInnerBody
                                let applyInner inner =  
                                    // apply back
                                    let withInner = backSub def body (Continuation(appInner, inner))
                                    match withInner with
                                    | None -> Seq.empty
                                    | Some withInner -> 
                                        let current = backSub appInner appInnerBody inner
                                        match current with
                                        | None -> Seq.empty
                                        | Some s -> Seq.singleton (Continuation(s, inner))
                                Seq.map applyInner evInner |> Seq.concat
                            Seq.map applyCur evCurrent |> Seq.concat
                // recursively call internal calls with autosubstitution of next calls
                // return result signature
                let proced = procBody body |> Seq.toList
                let res = proced |> List.map(fun proced -> bodyToSignature def body proced) |> List.choose identity
                res |> List.toSeq

        // call
        let res = List.map checkRule kb.Rules |> Seq.concat |> Seq.toList
        res |> List.toSeq

type SimpleSearcher() =
    interface ISearcher with
        member this.Search rb call = Search.search this rb call

type DebugInfoSearcher() =
    interface ISearcher with
        member s.Search rb call = 
            debug (sprintf "Called rule %s" call.AsString)
            let found = (new SimpleSearcher() :> ISearcher).Search rb call
            if (Seq.isEmpty found) then
                debug "No rules match."
            else
                for r in found do
                    debug (sprintf "Found %s" <| r.ToString())
            found

module Execute =
    let exec (f: ISearcher) (d: Rulebase) (s: Signature): unit =
        printfn "Execute: %s." s.AsString
        let res = f.Search d s
        if Seq.isEmpty res then
            printfn "Result: %b." false
        else
            for r in res do
                printfn "Result: %b. New context = %s" (true) ((r).ToString())
                