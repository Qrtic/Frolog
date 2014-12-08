namespace SearchLib

open SearchLib.Common
open SearchLib.Signature

module Context =
    type context = Map<variable, value>
    let EmptyContext = Map.empty<variable, value>

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
    // let reduce(init: context) (cur: context) = Map.map (fun k v -> Map.find k cur) init

    // let supply(init: context) (supp: (Parameter*value) list) = List.fold(fun (s: context) (k, v) -> s.Add(k, v)) init supp