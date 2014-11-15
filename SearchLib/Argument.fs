namespace SearchLib

open SearchLib.Common

module Argument =
    type parameter = name
    type argument = value
    type parameters = list<parameter>
    
    let to_arg (a: 'a): argument = a.ToString()

    let isVar (p: argument): bool = System.Char.IsUpper (p.Chars 0)

    // All non vars are constants that are defined as name that also can be used as value
    let (?=) (p1: argument) (p2: argument): bool = isVar p1 || isVar p2 || p1 = p2
    // let (?=) (p1: parameter) (p2: parameter): bool = isVar p1 || isVar p2 || p1 = p2

    let asInt (p: argument): option<int> =
        let ok, value = System.Int32.TryParse p
        if ok then
            Some value
        else
            None
            
    let asIntList (p: argument): option<list<int>> =
        if isVar p then None
        else
            let l = p.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
            if Array.exists(fun x -> not (fst (System.Int32.TryParse x))) l then
               None
            else
                Some(Array.map(fun x -> System.Int32.Parse x) l |> Array.toList)