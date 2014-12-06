namespace SearchLib

open SearchLib.Common

module Signature =
    [<StructuralComparison>][<StructuralEqualityAttribute>]
    type signature = {name: string; parameters: parameters}
        with
        override s.ToString() = s.AsString
        member s.AsString = s.name + s.parameters.ToString()
        member s1.signatureEq s2 = 
            if s1.name = s2.name then
                let p1 = s1.parameters
                let p2 = s2.parameters
                List.fold2(fun s t1 t2 -> s && t1 ?= t2) true p1 p2
            else
                false

    let signatureEq (s1 : signature) (s2 : signature): bool =
        s1.signatureEq s2
    let call name parameters = {name = name; parameters = parameters}