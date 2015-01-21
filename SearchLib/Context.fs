namespace Frolog

open Frolog.Common

type Context = Context of terms: Term list * boundedVariables: Term list list
      with
      static member Empty = Context([], [])
      static member Terms c =
        let (Context(terms, _)) = c
        terms
      static member Bounded c =
        let (Context(_, bounded)) = c
        bounded

module ContextHelper =
    let signelton x = Context([x], [])

    let rec add t c =
        match c with
        | Context([], []) -> Context([t], [])
        | Context(terms, bounded) ->
            if List.exists(fun x -> x = t) terms then
                failwith "Failed to add dublicates"
            else
                Context(t::terms, bounded)

    let rec remove t c =
        let newTerms = List.except(same t) (Context.Terms c)
        let newBounded = List.map(fun bs -> List.except(same t) bs) (Context.Bounded c)
        Context(newTerms, newBounded)

    let rec tryFind t c =
        let terms = Context.Terms c
        match t with
        | Variable(name) -> 
            List.tryFind (function
            | BoundedVariable(name2, v) -> name = name2
            | _ -> false) terms
        | _ -> List.tryFind (same t) terms

    type TermUnifyResult = TermUnifyResult of t1: Term * t2: Term * unifyied: Term
        with
        member this.T1 = 
            let (TermUnifyResult(t1, _, _)) = this
            t1
        member this.T2 = 
            let (TermUnifyResult(_, t2, _)) = this
            t2
        member this.Unified = 
            let (TermUnifyResult(_, _, unifyied)) = this
            unifyied

    type UnifyResult = 
        | Unified of TermUnifyResult * Context
        | Failed
        with
        static member isSuccess = function
            | Unified(_, _) -> true
            | Failed -> false
        static member isFailed = function
            | Unified(_, _) -> false
            | Failed -> true
        static member tryGetTerm result =
            match result with
            | Unified(termurl, _) -> 
                let (TermUnifyResult(_, _, unifyied)) = termurl
                Some(unifyied)
            | Failed -> None

    let rec applyContext t c =
        match t with
        | Variable(name) -> 
            match tryFind t c with
            | Some v -> v
            | _ -> t
        | _ -> t

    /// Guaranties bounding all variables
    let rec unify t1 t2 c: UnifyResult =
        let t1, t2 = applyContext t1 c, applyContext t2 c
        let _unify c t1 t2 =
            let t1Tot2UnifyResult = TermUnifyResult(t1, t2, t2)
            if not <| Term.Compatible t1 t2 then
                Failed
            else
                let terms = Context.Terms c
                let bounded = Context.Bounded c
                match t1, t2 with
                | Variable(name), _ -> 
                    if Term.IsChangeable t2 then
                        // Bound them
                        let withT2 = List.insert(List.exists (same t1)) (fun rest -> 
                            match rest with
                            | None -> [t1;t2]
                            | Some(ts) -> t2::ts) bounded
                        Unified(t1Tot2UnifyResult, Context(t2::terms, withT2))
                    else
                        // We can bind our value now
                        // Bind ourself
                        // get all bounded
                        let sameBounded = List.tryFind(List.exists (same t1)) (Context.Bounded c)
                        match sameBounded with
                        | None -> Unified(t1Tot2UnifyResult, Context(List.insert(same t1) (constant (BoundedVariable(name, t2))) terms, bounded))
                        | Some(sameBounded) ->
                            // Delete all bounded
                            let newBounded = List.except(same sameBounded) bounded
                            // Change all terms
                            let newterms = 
                                List.map(fun t -> 
                                if List.exists(same t) sameBounded then
                                    match t with
                                    | Variable(name) -> BoundedVariable(name, t2)
                                    | _ -> failwith "Cant unify unchangeables"
                                else
                                    t) terms
                            Unified(t1Tot2UnifyResult, Context(newterms, newBounded))
                | Structure(f1, args1), Structure(f2, args2) when f1 = f2 && List.length args1 = List.length args2 ->
                    let unifiedArgs = List.map2(fun a1 a2 -> unify a1 a2 c) args1 args2
                    let unifyStep a1 a2 st =
                        match st with
                        | Some(context, args) ->
                            match unify a1 a2 context with
                            | Failed -> None
                            | Unified(tur, context) -> Some(context, tur.Unified::args)
                        | None -> None
                    match List.foldBack2 unifyStep args1 args2 (Some(c, [])) with
                    | None -> Failed
                    | Some(c, argsUnifyResult) -> Unified(TermUnifyResult(t1, t2, Structure(f1, argsUnifyResult)), c)
                | _ when t1 = t2 -> Unified(t1Tot2UnifyResult, c)
                | _ -> Failed
        if Term.StrongEquals t1 t2 then
            Unified(TermUnifyResult(t1, t2, t2), c)
        else
            let u1 = _unify c t1 t2
            let u2 = _unify c t2 t1
            match u1, u2 with
            | Unified(_, _), _ -> u1
            | _, Unified(_, _) -> u2
            | _ -> Failed
            
    let unifySignatures s1 s2 context =
        match unify (Signature.AsTerm s1) (Signature.AsTerm s2) context with
        | Failed -> None
        | Unified(t, c) ->
            match sign (t.Unified) with
            | None -> None
            | Some(sign) -> Some(sign, c)

    /// <summary>
    /// Reduces current context to previus.
    /// </summary>
    /// <param name="cur">New context</param>
    /// <param name="reduceTo">Init context</param>
    let rec reduce (cur: Context) (reduceTo: Context) =
        let toRemove = List.filter(fun x -> List.exists(same x) (Context.Terms reduceTo)) (Context.Terms cur)
        List.fold(fun st t -> remove t st) cur toRemove
            
    let reduceWithVars (cur: Context) (reduceTo: Context) vars = 
        let toRemove = List.filter(fun x -> List.exists (same x) (Context.Terms reduceTo) || List.exists (same x) vars) (Context.Terms cur)
        List.fold(fun st t -> remove t st) cur toRemove
        
    let supply(context: Context) (supp: Term list) (supa: Term list) = 
        let f(p: Term) (a: Term) (c: Context * Term list) =
            let context, terms = c
            let unified = unify p a context
            match unified with
            | Unified(unterms, c) -> c, unterms.Unified::terms
            | _ -> failwith "Supplying failed due to unknown reason on stage of unifying"
        List.foldBack2 f supp supa (context, [])