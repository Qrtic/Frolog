namespace Frolog.Tests

open NUnit.Framework

open Frolog
open Frolog.DefineRule
open Frolog.DefineRule.DefPublic
open Frolog.SearchMachines
open Frolog.CustomRules

[<TestFixture>]
module ControlFlow =
    let def = signf << termf
    let execute facts call =
        let m = SearchMachines.Simple.Create()
        for fact in facts do
            m.AddRule(fact) 
        m.Execute(call) |> Seq.map(fun s -> s.AsString) |> Seq.toList

    [<Test>]
    let ``Test of fact``() = 
        let s = def "f(1)"
        let f = defFact s
        Assert.AreEqual(execute [f] s, ["f(1)"])
    [<Test>]
    let ``Test of call``() = 
        let f = defFact(def "f(1)")
        let c = defCall (def "call(X)") (def "f(X)")
        Assert.AreEqual(execute [f; c] (def "call(1)"), ["call(1)"])
        Assert.AreEqual(execute [f; c] (def "call(X)"), ["call(1)"])
    [<Test>]
    let ``Test of conjunction``() = 
        let f1 = defFact(def "f(1)")
        let f2 = defFact(def "f(2)")
        let c = defConjunction (def "call(X, Y)") (defCallBodyf "f(X)") (defCallBodyf "f(Y)")
        Assert.AreEqual(execute [f1; f2; c] (def "call(1, 0)"), [])
        Assert.AreEqual(execute [f1; f2; c] (def "call(1, 2)"), ["call(1, 2)"])
        Assert.AreEqual(execute [f1; f2; c] (def "call(X, Y)"), ["call(1, 1)"; "call(1, 2)"; "call(2, 1)"; "call(2, 2)"])
    [<Test>]
    let ``Test of or``() = 
        let f1 = defFact(def "f1(1)")
        let f2 = defFact(def "f2(2)")
        let c = defOr (def "call(X)") (defCallBodyf "f1(X)") (defCallBodyf "f2(X)")
        Assert.AreEqual(execute [f1; f2; c] (def "call(0)"), [])
        Assert.AreEqual(execute [f1; f2; c] (def "call(1)"), ["call(1)"])
        Assert.AreEqual(execute [f1; f2; c] (def "call(X)"), ["call(1)"; "call(2)"])
    [<Test>]
    let ``Test of cut``() = 
        let f1 = defFact(def "f(1)")
        let f2 = defFact(def "f(2)")
        let cut = defCall (def "call(X)") (def "f(X)") |> combine (Cut(Lexem(True)))
        let r1 = execute [f1; f2; cut] (def "call(1)")
        Assert.AreEqual(execute [f1; f2; cut] (def "call(1)"), ["call(1)"])
        let r2 = execute [f1; f2; cut] (def "call(X)")
        Assert.AreEqual(execute [f1; f2; cut] (def "call(X)"), ["call(1)"])
    [<Test>]
    let ``Test of not``() = 
        let f1 = defFact(def "a(1)")
        let f2 = defFact(def "b(1)")
        let f3 = defFact(def "b(2)")
        let not = defCall(def "call(X)") (def "b(X)") |> combine (Not(defCallBodyf "a(X)"))
        Assert.AreEqual(execute [f1; f2; f3] (def "call(1)"), [])
        Assert.AreEqual(execute [f1; f2; f3] (def "call(2)"), ["call(2)"])
        Assert.AreEqual(execute [f1; f2; f3] (def "call(X)"), ["call(2)"])