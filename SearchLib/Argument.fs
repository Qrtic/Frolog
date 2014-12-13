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

type dataType = Undefined | Integer | String | List of dataType
    with
    override d.ToString() = 
        match d with
        | Undefined -> "undefined"
        | Integer -> "int"
        | String -> "string"
        | List(dt) -> "list of " + dt.ToString()

type variable = string
type value = string

type Variable() =
    static member tryCreate name =
        if isVariableName name then
            Some(name)
        else
            None
    static member create name =
        if isVariableName name then
            name
        else
            failwith "Cant create variable with value name"
    static member accept name = isVariableName name
    
type Value() =
    static member tryCreate name =
        if isVariableName name then
            None
        else
            Some(name)
    static member canCreate name =
        if isVariableName name then
            false
        elif Value.getType(name) = dataType.Undefined then
            false
        else
            true
    static member create name =
        if Value.canCreate name then
            name
        else
            failwith "Cant create value with variable name"
    static member create (i: int) = i.ToString()
    static member create (il: int list) = 
        let start = List.fold(fun s i -> s + i.ToString() + ";") "[" il
        start.Trim(';') + "]"
    static member getType (value: value) =
        match value with
        | i when fst(Int32.TryParse(i)) -> dataType.Integer
        | list when contains list "[]" ->
            let anyentry = list.Trim([|'['; ']'|]).Split(';')
            let innertype = if anyentry.Length = 0 then dataType.Undefined else Value.getType(anyentry.[0])
            dataType.List(innertype)
        | str when not <| contains str "[]\\' ,." -> dataType.String
        | _ -> failwith "Cant define type"
    static member int = function
        | v when Value.getType(v) = dataType.Integer -> Some(Int32.Parse v)
        | _ -> None
    static member string = function
        | v when Value.getType(v) = dataType.String -> Some(v)
        | _ -> None
    static member intList = function
        | v when Value.getType(v) = dataType.List(dataType.Integer) -> 
            let entries = v.Trim([|'['; ']'|]).Split(';')
            Some(Array.map Int32.Parse entries |> Array.toList)
        | _ -> None

// Var is uppercase
// Val is integer or downcase
// TODO Var with types and anytypes
type VarOrValue = Var of string | Val of string
    with
    member v.Type =
        match v with
        | Var(name) -> dataType.Undefined
        | Val(v) -> dataType.String // TODO
    member v.AsString = 
        match v with
        | Val(v) -> v
        | Var(v) -> v
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
    override v.ToString() = v.AsString
        
[<StructuredFormatDisplayAttribute("{AsString")>]
type parameter = Parameter of VarOrValue
    with
    member p.AsString = p.ToString()
    override p.ToString() = 
        let (Parameter prm) = p
        prm.AsString
    static member (?=) (p1: parameter, p2: parameter) =
        let (parameter.Parameter p1, parameter.Parameter p2) = (p1, p2)
        match p1, p2 with
        | Val(v1), Val(v2) -> v1 = v2
        | Var(n1), Var(n2) -> n1 = n2
        | _ -> false

type argument = Argument of VarOrValue
    with
    member a.AsString = a.ToString()
    override a.ToString() = 
        let (Argument arg) = a
        arg.AsString
    static member (?=) (a1: argument, a2: argument) =
        let (argument.Argument a1, argument.Argument a2) = (a1, a2)
        match a1, a2 with
        | Val(v1), Val(v2) -> v1 = v2
        | Var(n1), Var(n2) -> n1 = n2
        | _ -> false
        
type parameters = parameter list
type arguments = argument list

module Unify =
    let canUnify (Parameter p) (Argument a) =
        let isanyvar = p.IsVariable || a.IsVariable
        if isanyvar then
            true
        else
            p.AsString = a.AsString
    let tryUnify (Parameter p) (Argument a): argument option =
        match p, a with
        | Var(vp), Var(va) -> Argument(a) |> Some
        | Var(vp), Val(va) -> Argument(a) |> Some
        | Val(vp), Var(va) -> Argument(p) |> Some
        | Val(vp), Val(va) when vp = va -> Argument(p) |> Some
        | _ -> None
    let convert (Parameter p): argument =
        Argument(p)

type Parameter() =
    static member asString(Parameter p) = p.AsString
    static member isVariable(Parameter p) = p.IsVariable
    static member asVariable(Parameter p) = p.asVariable
    static member asValue(Parameter p) = p.asValue
    static member create (i: int) = parameter.Parameter(Val(Value.create(i)))
    static member create (il: int list) = parameter.Parameter(Val(Value.create il))
    static member create (s: string) = 
        if isVariableName s then
            parameter.Parameter(Var(s))
        else
            parameter.Parameter(Val(s))

type Argument() =
    static member asString(Argument p) = p.AsString
    static member isVariable(Argument p) = p.IsVariable
    static member asVariable(Argument p) = p.asVariable
    static member asValue(Argument p) = p.asValue
    static member create (i: int) = argument.Argument(Val(Value.create(i)))
    static member create (il: int list) = argument.Argument(Val(Value.create il))
    static member create (s: string) = 
        if isVariableName s then
            argument.Argument(Var(s))
        else
            argument.Argument(Val(s))
    static member getValue(Argument a) = 
        match a with
        | Var(_) -> None
        | Val(v) -> Some(v)