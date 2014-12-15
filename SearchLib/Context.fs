namespace SearchLib

open SearchLib.Common

module Context =
    type context = Map<variable, value>
    let EmptyContext = Map.empty<variable, value>

    let create (m: Map<variable, value>): context = m
    let singleton (_var: variable) (_val: value) = EmptyContext.Add(_var, _val)

    let uniform(c: context) (Argument p): argument =
        match p with
        | Val(v) -> argument.Argument(Val(v))
        | Var(v) ->
            let res = c.TryFind v
            match res with
                | Some(r) -> Argument.create r
                | None -> Argument.create v

    let replaceVars(c: context) (s: Call) =
        let replaced = List.map (fun p -> uniform c p) s.args
        {name = s.name; args = replaced}

    /// <summary>
    /// Reduces current context to previus.
    /// </summary>
    /// <param name="init">Init context</param>
    /// <param name="cur">New context</param>
    let reduce (cur: context) (reduceTo: context) = Map.map (fun k v -> Map.find k cur) reduceTo
    
    // let supply(init: context) (supp: (Parameter*value) list) = List.fold(fun (s: context) (k, v) -> s.Add(k, v)) init supp

    let supply(context: context) (supp: parameter list) (supa: argument list) = 
        let f(c: context) (p: parameter) (a: argument): context =
            let pv = Parameter.asVariable p
            let av = Argument.asValue a
            match pv, av with
            | Some(pv), Some(av) -> c.Add(pv, av)
            | _ ->
                let pv = Parameter.asValue p
                let av = Argument.asVariable a
                match pv, av with
                | Some(pv), Some(av) -> c.Add(av, pv)
                | _ -> c
        List.fold2 f context supp supa

    let replace(init: context) (newc: context): context =
        newc |> Map.fold(fun (s: context) k t -> s.Add(k, t)) init