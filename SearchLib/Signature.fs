namespace SearchLib

open SearchLib.Argument
open SearchLib.Common

module Signature =
    type signature = name * parameters
    type result = True of parameters | False

    let toStr (s: signature): string =
        fst s + ((snd s).ToString())
    let signatureEq (s1 : signature) (s2 : signature): bool =
        if fst s1 = fst s2 then
            let p1 = snd s1
            let p2 = snd s2
            List.fold2(fun s t1 t2 -> s && t1 ?= t2) true p1 p2
        else
            false