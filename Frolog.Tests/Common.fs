namespace Frolog.Tests

open Frolog

open NUnit.Framework
open FsUnit

[<AutoOpen>]
module Common =
    let test f = f()
    let testeq r res = r |> should equal res
    let testneq r res = r |> should not' (equal res)
    let testfail f = should throw typeof<System.Exception> (f >> ignore)