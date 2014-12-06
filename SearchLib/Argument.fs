namespace SearchLib

open System
open SearchLib.Common

(*
signature("Abs", ["A","B"])
A and B are parameters

call("Abs", ["a", "B"])
a and B are arguments
*)

/// Parameter is a name of argument in predicate
exception ArgumentCastException of string

type parameter = {name : string}

type argument = 
    internal Value of value: string | Variable of name: string
    with
    member a.IsVar = match a with
                        | Value(_)    -> false
                        | Variable(_) -> true
    member a.AsInt = match a with
                        | Value(i)    -> 
                            let ok, value = Int32.TryParse i
                            if ok then Some(value) else None
                        | Variable(_) -> None
    member a.AsIntList = match a with
                            | Value(p) -> 
                                let l = p.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
                                let ia = Array.map(fun x -> System.Int32.TryParse x) l
                                if (Array.exists(fun (ok, _) -> not ok)) ia then None
                                else Array.map(fun (_, v) -> v) ia |> Array.toList |> Option.Some
                            | Variable(_) -> None
    static member (?=) (a1: argument, a2: argument) = a1.IsVar && a2.IsVar || a1.Equals(a2)

// type parameter = name
type parameters = list<parameter>
type arguments = list<argument>

[<RequireQualifiedAccessAttribute>]
module Argument =
    let toArgument (a: 'a): argument =
        let c = a.ToString()
        if System.Char.IsUpper (c.Chars 0) then Variable(c) else Value(c)