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

type dataType = Integer | String | List of dataType
type variable = {Name: string; Type: dataType}
type value = Integer of int | String of string | IntList of int list | StrList of string list

type variable
    with
    member v.Compatible = function
        | Integer(i) -> v.Type = dataType.Integer
        | String(s) -> v.Type = dataType.String
        | IntList(l) -> v.Type = dataType.List(dataType.Integer)
        | StrList(l) -> v.Type = dataType.List(dataType.String)
    
type value
    with
    member v.AsString = 
        match v with
        | Integer(i) -> i.ToString()
        | String(s) -> s
        | IntList(l) -> l.ToString()
        | StrList(l) -> l.ToString()

type Variable() =
    static member intVar name = {Name = name; Type = dataType.Integer}
    static member strVar name = {Name = name; Type = dataType.String}
    static member intlistVar name = {Name = name; Type = dataType.List(dataType.Integer)}
    static member strlistVar name = {Name = name; Type = dataType.List(dataType.String)}
    static member accept(v, i: int) = v.Type = dataType.Integer
    static member accept(v, s: string) = v.Type = dataType.String
    static member accept(v, s: int list) = v.Type = dataType.List(dataType.Integer)
    static member accept(v, s: string list) = v.Type = dataType.List(dataType.String)

type Value() =
    static member create i = Integer(i)
    static member create s = String(s)
    static member create l = IntList(l)
    static member create l = StrList(l)
    static member int v = 
        match v with
        | Integer(v) -> Some(v)
        | _ -> None
    static member string v = 
        match v with
        | String(s) -> Some(s)
        | _ -> None
    static member intlist v = 
        match v with
        | IntList(l) -> Some(l)
        | _ -> None
    static member strlist v = 
        match v with
        | StrList(l) -> Some(l)
        | _ -> None

type VarOrValue = Var of variable | Val of value
    with
    member v.AsString = 
        match v with
        | Val(v) -> v.AsString
        | Var(v) -> v.Name
    member v.IsVariable = 
        match v with
        | Val(_) -> false
        | Var(_) -> true
    member v.asVariable = 
        match v with
        | Var(v) -> Some(v)
        | _ -> None
    member v.asValue =
        match v with
        | Val(v) -> Some(v)
        | _ -> None

type parameter = Parameter of VarOrValue
    with
    static member (?=) (p1: parameter, p2: parameter) =
        let (parameter.Parameter p1, parameter.Parameter p2) = (p1, p2)
        match p1, p2 with
        | Val(v1), Val(v2) -> v1 = v2
        | Var(n1), Var(n2) -> n1 = n2
        | _ -> false

type argument = Argument of VarOrValue
    with
    static member (?=) (a1: argument, a2: argument) =
        let (argument.Argument a1, argument.Argument a2) = (a1, a2)
        match a1, a2 with
        | Val(v1), Val(v2) -> v1 = v2
        | Var(n1), Var(n2) -> n1 = n2
        | _ -> false
        
type parameters = parameter list
type arguments = argument list

module Unify =
    let unify (Parameter p) (Argument a) =
        p.IsVariable || a.IsVariable || p.AsString = a.AsString
    let convert (Parameter p): argument =
        Argument(p)

type Parameter() =
    static member asString(Parameter p) = p.AsString
    static member isVariable(Parameter p) = p.IsVariable
    static member asVariable(Parameter p) = p.asVariable
    static member create i = parameter.Parameter(Val(Integer(i)))
    static member create s = if Common.isVariableName s then failwith "" else parameter.Parameter(Val(String(s)))
    static member create(name, dtype) = parameter.Parameter(Var({Name = name; Type = dtype}))

type Argument() =
    static member asString(Argument p) = p.AsString
    static member isVariable(Argument p) = p.IsVariable
    static member asVariable(Argument p) = p.asVariable
    static member asValue(Argument p) = p.asValue
    static member create i = argument.Argument(Val(Integer(i)))
    static member create s = if Common.isVariableName s then failwith "" else argument.Argument(Val(String(s)))
    static member create il = argument.Argument(Val(IntList(il)))
    static member create sl = argument.Argument(Val(StrList(sl)))
    static member create(name, dtype) = argument.Argument(Var({Name = name; Type = dtype}))
    static member create v = argument.Argument(Val(v))
    static member create v = argument.Argument(Var(v))
    static member value(Argument a) = 
        match a with
        | Var(_) -> None
        | Val(v) -> Some(v)