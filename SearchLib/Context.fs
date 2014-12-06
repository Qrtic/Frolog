namespace SearchLib

open SearchLib.Common
open SearchLib.Signature

module Context =
    type context = Map<Parameter, value>
    
    let convert_to_arg(c: context) (p: Parameter): argument = 
        if isVar p then 
            let res = c.TryFind(p) // Think that we always have this p
            match res with
                | Some(v) -> v
                | None -> p
        else p

    let replaceVars(c: context) (s: signature) =
        let replaced = List.map (fun p -> convert_to_arg c p) s.parameters
        {name = s.name; parameters = replaced}

    /// <summary>
    /// Reduces current context to previus.
    /// </summary>
    /// <param name="init">Init context</param>
    /// <param name="cur">New context</param>
    let reduce(init: context) (cur: context) = Map.map (fun k v -> Map.find k cur) init

    let supply(init: context) (supp: (Parameter*value) list) = List.fold(fun (s: context) (k, v) -> s.Add(k, v)) init supp