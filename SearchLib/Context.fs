namespace SearchLib

open SearchLib.Common
open SearchLib.Argument

module Context =
    type context = Map<parameter, value>
    let convert_to_arg(c: context) (p: parameter): argument = 
        if isVar p then 
            let res = c.TryFind(p) // Think that we always have this p
            match res with
                | Some(v) -> v
                | None -> p
        else p