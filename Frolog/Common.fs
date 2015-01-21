namespace Frolog

module Common =
    let debug s = System.Diagnostics.Debug.WriteLine s
    let constant x = fun y -> x
    let identity x = x
    let allways(a: 'a) = true
    let equalsOpt<'T when 'T :> System.IEquatable<'T>> (x : 'T) (y : 'T) =
        x.Equals(y)

    let compareOpt (x : #System.IComparable<'T>) (y : #System.IComparable<'T>) = x.CompareTo(y)
    
    let isVariableName (s : string) = System.Char.IsUpper (s.Chars 0)
    let isStructureName (s: string) = s.Chars 0 = '(' && s.Chars(s.Length - 1) = ')'

    let same x y = x = y
    let contains (container) (what) =
        Set.ofSeq(container) |> Set.intersect(Set.ofSeq(what)) |> Set.isEmpty |> not
    
    open System.Text.RegularExpressions
 
    let (|Match|_|) pattern input =
        let m = Regex.Match(input, pattern) in
        if m.Success then Some (List.tail [ for g in m.Groups -> g.Value ]) else None

module List =
    let rec change f n = function
        | [] -> []
        | h::t when f(h) -> n(h)::t
        | h::t -> h::change f n t
    let rec insert f n = function
        | [] -> [n None]
        | h::t when f(h) -> n(Some(h))::t
        | h::t -> h::insert f n t
    let rec except f = function
        | [] -> []
        | h::t when f(h) -> t
        | h::t -> h::except f t