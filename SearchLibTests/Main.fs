open SearchLib
open SearchLib.Context
open SearchLib.SearchMachine
open SearchLib.Signature
open SearchLib.Knowledgebase
open SearchLib.Rule

open NUnit.Framework
open FsUnit

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

[<TestFixture>]
module SimpleTest =
    let dtype = dataType.Integer
    let name = "b"
    let kb (i: int) = EmptyKB.Add(rule.fact(Signature.define name [Parameter.create i]))
    let varkb (n) = EmptyKB.Add(rule.fact(Signature.define name [Parameter.create(n, dtype)]))
    let c = Context.EmptyContext
    let vc k (v: int) = Map.empty.Add(Variable.intVar k, Value.create v)
    let call (i: int) = Signature.call name [Argument.create i]
    let varcall n = Signature.call name [Argument.create(n, dtype)]
    
    [<Test>]
    let ``Add fact, check it``() =
        let kb = kb 1
        let call = call 1
        let res = find kb c call |> Seq.length
        res |> should equal 1
    
    [<Test>]
    let ``Add fact, check another``() =
        let kb = kb 1
        let call = call 2
        let res = find kb c call |> Seq.length
        res |> should not' (equal 1)
        
    [<Test>]
    let ``Call fact with variable argument``() =
        let kb = kb 1
        let c = vc "C" 1
        let call = varcall "C"
        let res = find kb c call |> Seq.length
        res |> should equal 1

    [<Test>]
    let ``Call var fact with argument``() =
        let kb = varkb "A"
        let call = call 1
        let res = find kb c call |> Seq.length
        res |> should equal 1

TestSearchMachine.starttest 1000 3 5
TestSearchMachine.starttest 1000 3 3
System.Console.ReadKey() |> ignore