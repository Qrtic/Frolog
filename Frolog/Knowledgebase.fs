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
        member k.Append r = new Knowledgebase(k.rules@[r]) :> Rulebase
        member k.AppendRange rs = new Knowledgebase(k.rules@(Seq.toList rs)) :> Rulebase
        member k.Rules = k.rules
    
type SearchResult = Signature seq

type ISearcher =
    abstract Search: Rulebase -> Signature -> SearchResult

module Search =    
    type ProcBodyResult = FalseResult | SingleResult of RuleBody | ManyResults of RuleBody seq | CutResults of RuleBody seq
    with
        member r.AsSeq =
            match r with
            | FalseResult -> Seq.empty
            | SingleResult s -> Seq.singleton s
            | ManyResults ss -> ss
            | CutResults ss -> ss
        static member toSeq (r: ProcBodyResult) = r.AsSeq

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
            | Cut(db), cb -> substituteBodyByBody db cb subBody // cb // TODO: check this case
            | Not(ndb), Or(Conjunction(Cut(cdb), Lexem(False)), Lexem(True)) -> 
                failwith "Dont know what to do in this case"
            | _ -> failwith "Cant substitute different bodies"

        let rec backSub def defBody resBody =
            match defBody, resBody with
            | Lexem(True), Lexem(True) -> Some def
            | Lexem(False), Lexem(False) -> Some def
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
            | Cut(b1), b2 -> backSub def b1 b2
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
                    | Lexem(False) -> FalseResult
                    | Lexem(True) -> SingleResult (Lexem True)
                    | Lexem(Predicate(p)) -> 
                        let cur = internalSubstitute def call signature
                        match p (PredicateInput(Signature.GetArguments cur)) with
                        | Failed -> FalseResult
                        | Success(PredicateOutput(pout)) ->
                            SingleResult(Lexem(Call(sign name pout)))
                    | Lexem(Call(c)) -> 
                        if Signature.GetName c = "not" then
                            // That is the not option
                            match Signature.GetArguments c with
                            | [Structure(cname, args)] -> procBody(Not(Lexem(Call(sign cname args))))
                            | _ -> failwith "Cant parse not expression."
                        else
                            let cur = internalSubstitute def call c
                            // evaluate
                            let evCurrent =
                                if isInternal then
                                    search searcher knowledgebase cur
                                else
                                    searcher.Search knowledgebase cur
                            let binder c = Some(Lexem(Call c))
                            ManyResults((Seq.map (unifySignatures cur) >> Seq.choose (Option.bind binder)) evCurrent)
                    | Or(b1, b2) ->
                        let p1 = procBody b1
                        let p2 = procBody b2

                        let left = Seq.map(fun p -> Or(p, b2)) p1.AsSeq
                        let right = Seq.map(fun p -> Or(b1, p)) p2.AsSeq

                        ManyResults(Seq.concat [left; right])
                    | Conjunction(b1, b2) ->
                        // TODO research in what case
                        if not <| canApply def call then
                            FalseResult
                        else
                            let subB1 = substituteBody def call b1
                            let procedB1 = procBody subB1
                            let s = seq {
                                let e = procedB1.AsSeq.GetEnumerator()
                                let notCutten = ref true

                                while e.MoveNext() && !notCutten do
                                    let pb1 = e.Current
                                    let preSubB2 = substituteBody def call b2
                                    let subB2 = substituteBodyByBody b1 pb1 preSubB2
                                    let procedB2 = procBody subB2
                                    match procedB2 with
                                    | CutResults(pbs2) ->
                                        for pb2 in pbs2 do
                                            let postSub1 = substituteBodyByBody subB2 pb2 pb1
                                            yield Conjunction(postSub1, pb2)
                                        notCutten := false
                                    | _ ->
                                        for pb2 in procedB2.AsSeq do
                                            let postSub1 = substituteBodyByBody subB2 pb2 pb1
                                            yield Conjunction(postSub1, pb2)
                            }
                            ManyResults(s)
                    | Not(body) -> 
                        // Not is only an (Cut, False); True
                        procBody(Or(Conjunction(Cut(body), Lexem(False)), Lexem(True)))
                        // failwith "Not implemented function <NOT>"
                    | Cut(body) -> 
                        // Cut is a message to stop searching for any another facts
                        // ONLY WITHIN ONE RULE
                        // TODO: TEST THIS BEHAVIOUR
                        let procThis = procBody body
                        CutResults(procThis.AsSeq)
                        // failwith "Not implemented function <CUT>"
                // recursively call internal calls with autosubstitution of next calls
                // return result signature
                let proced = procBody body |> ProcBodyResult.toSeq |> Seq.toList
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
                