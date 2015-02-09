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

        // Changes all bodies in forward order
        let rec substituteBody def call body =
            match body with
            | Lexem(Call lexemCall) -> Lexem(Call(internalSubstitute def call lexemCall))
            | Conjunction(body1, body2) -> Conjunction(substituteBody def call body1, substituteBody def call body2)
            | Or(body1, body2) -> Or(substituteBody def call body1, substituteBody def call body2)
            | _ -> body

        let rec substituteBodyByBody defBody callBody subBody =
            match defBody, callBody with
            | Lexem(Call d), Lexem(Call c) -> substituteBody d c subBody
            | Lexem(_), Lexem(_) -> subBody
            | Conjunction(db1, db2), Conjunction(cb1, cb2) ->
                let leftBodySub = substituteBodyByBody db1 cb1 subBody
                let rightBodySub = substituteBodyByBody db2 cb2 leftBodySub
                rightBodySub
            | Or(db1, db2), Or(cb1, cb2) -> 
                let leftBodySub = substituteBodyByBody db1 cb1 subBody
                let rightBodySub = substituteBodyByBody db2 cb2 leftBodySub
                rightBodySub
            | Cut(db), Cut(cb) -> substituteBodyByBody db cb subBody
            | Not(db), Not(cb) -> substituteBodyByBody db cb subBody
            | _ -> failwith "Cant substitute different bodies"  

        let rec backSub def defBody resBody =
            match defBody, resBody with
            | Lexem(Call(s1)), Lexem(Call(s2)) -> internalSubstitute s1 s2 def |> Some
            | Lexem(Predicate(_)), Lexem(Call s) -> Some s
            | Lexem(_), Lexem(_) -> Some def
            | Conjunction(b1, b2), Conjunction(b3, b4) -> 
                match backSub def b1 b3 with
                | Some s -> 
                    match backSub s b2 b4 with
                    | Some s -> Some s
                    | _ -> None
                | None -> None
            | Or(b1, b2), Or(b3, b4) ->
                match backSub def b1 b3, backSub def b2 b4 with
                | Some s1, Some s2 -> internalSubstitute s1 s2 def |> Some
                | _ -> None
            | Cut(b1), Cut(b2) -> backSub def b1 b2
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

                let rec procBody = 
                    function
                    | Lexem(False) -> Seq.empty
                    | Lexem(True) -> Seq.singleton(Lexem(True))
                    | Lexem(Predicate(p)) -> 
                        let cur = internalSubstitute def call signature
                        match p (PredicateInput(Signature.GetArguments cur)) with
                        | Failed -> Seq.empty
                        | Success(PredicateOutput(pout)) ->
                            Seq.singleton(Lexem(Call(sign name pout)))
                    | Lexem(Call(c)) -> 
                        let cur = internalSubstitute def call c
                        // evaluate
                        let evCurrent =
                            if isInternal then
                                search searcher knowledgebase cur
                            else
                                searcher.Search knowledgebase cur
                        let binder c = Some(Lexem(Call c))
                        Seq.map (unifySignatures cur) >> Seq.choose (Option.bind binder) <| evCurrent
                    | Or(b1, b2) ->
                        let p1 = procBody b1
                        let p2 = procBody b2
                        Seq.concat [Seq.map(fun p -> Or(p, b2)) p1; Seq.map(fun p -> Or(b1, p)) p2]
                    | Conjunction(b1, b2) ->
                        // TODO research in what case
                        if not <| canApply def call then
                            Seq.empty
                        else
                            let subB1 = substituteBody def call b1
                            let procedB1 = procBody subB1
                            seq {
                                for pb1 in procedB1 do
                                    let preSubB2 = substituteBody def call b2
                                    let subB2 = substituteBodyByBody b1 pb1 preSubB2
                                    let procedB2 = procBody subB2
                                    for pb2 in procedB2 do
                                        let postSub1 = substituteBodyByBody subB2 pb2 pb1
                                        yield Conjunction(postSub1, pb2)
                            }
                    | Not(_) -> failwith "Not implemented function <NOT>"
                    | Cut(_) -> failwith "Not implemented function <CUT>"
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
                