open SearchLib
open SearchLib.Context
open SearchLib.SearchMachine
open SearchLib.Signature
open SearchLib.Knowledgebase
open SearchLib.Rule

open NUnit.Framework

// is parent to children
// let isparent = Rule "isparent" ["P"; "C"] (F2(fun p c -> Signature.Rejected))
(*
let parent p c = Fact "parent" [p; c]
let isgrandparent: rule =
    let calls = [call "parent" ["G"; "P"]; call "parent" ["P"; "C"]]
    ConRule "grandparent" ["G"; "C"] calls

let parents = [parent "andrew" "pasha"; parent "alesha" "misha"; parent "misha" "sasha"; parent "misha" "yura"]
let testkb = [Fact "f" ["1"]; Fact "f" ["2"]] @ defaultRules @ parents @ [isgrandparent]
let textcontext = Map.empty.Add("Alesha", "alesha").Add("Sasha", "sasha")
let testmachine = SearchMachine(testkb, textcontext)
let test = testmachine.PrintAll({name = "grandparent"; parameters = ["Alesha"; "Sasha"]})

*)

[<Test>]
let ``Add fact, check it``() =
    let def = Signature.define("b") [Parameter.create 1]
    let fact = rule.fact(def)
    let kb = EmptyKB.Add(fact)
    let c = Context.EmptyContext
    let call = Signature.call "b" [Argument.create 1]
    let res = find kb c call
    Assert.AreEqual(Seq.length res, 1)

[<Test>]
let ``Add fact, check another``() =
    let def = Signature.define("b") [Parameter.create 1]
    let fact = rule.fact(def)
    let kb = EmptyKB.Add(fact)
    let c = Context.EmptyContext
    let call = Signature.call "b" [Argument.create 2]
    let res = find kb c call
    Assert.AreNotEqual(Seq.length res, 1)

``Add fact, check it``() |> ignore
TestSearchMachine.starttest 1000 3 5
TestSearchMachine.starttest 1000 3 3
System.Console.ReadKey() |> ignore