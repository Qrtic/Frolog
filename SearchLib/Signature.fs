namespace SearchLib

open SearchLib.Common

[<CustomEquality>][<CustomComparison>]
[<StructuredFormatDisplayAttribute("{AsString}")>]
type Definition = {name: string; prms: parameters}
    with
    member d.AsString = 
        let parameterString = ("", d.prms) ||> List.fold(fun acc p -> acc + p.AsString + ", ")
        sprintf "%s(%s)" d.name parameterString
    override d.Equals(d1) = 
        match d1 with
        | :? Definition as d1 -> Definition.StrongEquals(d, d1)
        | _ -> false
    override d.GetHashCode() =
        d.name.GetHashCode() + d.prms.GetHashCode()
    static member Compatible(d1: Definition, d2: Definition) =
        let cnames = d1.name = d2.name
        let cprms = lazy List.fold2(fun s t1 t2 -> s && t1 ?= t2) true d1.prms d2.prms
        cnames && cprms.Force()
    static member StrongEquals(d1: Definition, d2: Definition) =
        let cnames = d1.name = d2.name
        let cprms = lazy ((d1.prms.Length = d2.prms.Length) && List.fold2(fun s t1 t2 -> s && t1 = t2) true d1.prms d2.prms)
        cnames && cprms.Force()
    static member CompareTo(d1: Definition) (d2: Definition) =
        let namec = d1.name.CompareTo(d2.name)
        let argsc = d1.prms.Length.CompareTo(d2.prms.Length)
        if namec = 0 then argsc else namec
    interface System.IComparable<Definition> with
        member d.CompareTo(d1) = Definition.CompareTo d d1
    interface System.IComparable with
        member d.CompareTo(o) = 
            match o with
            | :? Definition as d1 -> Definition.CompareTo d d1
            | _ -> failwith "Cant compare definition with any other type"
        
[<CustomEquality>][<CustomComparison>]
type Call = {name: string; args: arguments}
    with
    member c.AsString = 
        let parameterString = ("", c.args) ||> List.fold(fun acc arg -> acc + arg.AsString + ", ")
        sprintf "%s(%s)" c.name parameterString
    override c.Equals(d1) = 
        match d1 with
        | :? Call as c1 -> Call.Equals(c, c1)
        | _ -> false
    override c.GetHashCode() =
        c.name.GetHashCode() + c.args.GetHashCode()
    static member Equals(c1: Call, c2: Call) =
        let cnames = c1.name = c2.name
        let cprms = lazy List.fold2(fun s t1 t2 -> s && t1 ?= t2) true c1.args c2.args
        cnames && cprms.Force()
    static member CompareTo(c1: Call) (c2: Call) =
            let namec = c1.name.CompareTo(c2.name)
            let argsc = c1.args.Length.CompareTo(c2.args.Length)
            if namec = 0 then argsc else namec
    interface System.IComparable<Call> with
        member c.CompareTo(c1) = Call.CompareTo c c1
    interface System.IComparable with
        member c.CompareTo(o) = 
            match o with
            | :? Call as c1 -> Call.CompareTo c c1
            | _ -> failwith "Cant compare definition with any other type"

type Signature() =
    static member compatible(d: Definition, c: Call) =
        let cnames = d.name = c.name
        let cprmsLen = d.prms.Length = c.args.Length
        let cprms = lazy List.fold2(fun s t1 t2 -> s && Unify.canUnify t1 t2) true d.prms c.args
        cnames && cprmsLen && cprms.Force()
        
    static member define(name, parameters) = {name = name; prms = parameters}
    static member define(name, prms : string list) =
        let parameters = prms |> List.map(fun p -> Parameter.create p)
        {name = name; prms = parameters}
    static member define(name, prms : int list) =
        let parameters = prms |> List.map(fun p -> Parameter.create p)
        {name = name; prms = parameters}

    static member call(name, arguments) = {name = name; args = arguments}
    static member call(name, args: string list) =
        let arguments = args |> List.map(fun p -> Argument.create p)
        {name = name; args = arguments}
    static member call(name, args: int list) =
        let arguments = args |> List.map(fun p -> Argument.create p)
        {name = name; args = arguments}