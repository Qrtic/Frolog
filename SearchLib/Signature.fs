﻿namespace SearchLib

open SearchLib.Common

[<CustomEquality>][<CustomComparison>]
type Definition = {name: string; prms: parameters}
    with
    member d.AsString = sprintf "%s(%A)" d.name d.prms
    override d.Equals(d1) = 
        match d1 with
        | :? Definition as d1 -> Definition.Equals(d, d1)
        | _ -> false
    override d.GetHashCode() =
        d.name.GetHashCode() + d.prms.GetHashCode()
    static member Equals(d1: Definition, d2: Definition) =
        let cnames = d1.name = d2.name
        let cprms = lazy List.fold2(fun s t1 t2 -> s && t1 ?= t2) true d1.prms d2.prms
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
    member c.AsString = sprintf "%s(%A)" c.name c.args
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

module Signature =
    let compatible(d: Definition, c: Call) =
        let cnames = d.name = c.name
        let cprms = lazy List.fold2(fun s t1 t2 -> s && Unify.canUnify t1 t2) true d.prms c.args
        cnames && cprms.Force()

    let define name parameters = {name = name; prms = parameters}
    let call name arguments = {name = name; args = arguments}