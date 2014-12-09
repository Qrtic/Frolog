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

    [<Test>]
    let ``Call standart predicates``() =
        let kb = DefaultKB
        let inc = Signature.call "inc" [Argument.create 1; Argument.create 2]
        let sum = Signature.call "sum" [Argument.create 1; Argument.create 2; Argument.create 3]
        let divs = Signature.call "divs" [Argument.create 10; Argument.create [1;2;5;10]]
        let res call = find kb c call |> Seq.length
        res inc |> should equal 1
        res sum |> should equal 1
        res divs |> should equal 1

    [<Test>]
    let ``Call complicated(d = 1) rule``() =
        let prm = [Parameter.create("X", dataType.Integer); Parameter.create("Y", dataType.Integer)]
        let cls = [Signature.call "sum" [Argument.create("X", dataType.Integer); Argument.create("X", dataType.Integer); Argument.create("Y", dataType.Integer)]]
        let r = ConRule "2x" prm cls
        let kb = DefaultKB.Add(r)
        let tcall = Signature.call "2x" [Argument.create 3; Argument.create 6]
        let fcall = Signature.call "2x" [Argument.create 3; Argument.create 7]
        let res call = find kb Context.EmptyContext call |> Seq.length
        let tcall' = res tcall
        let fcall' = res fcall
        tcall' |> should equal 1
        fcall' |> should equal 0
    
    [<Test>]
    let ``Call complicated(d = 2) rule``() =
        let prm = [Parameter.create("X", dataType.Integer); Parameter.create("Y", dataType.Integer)]
        let cls = [
            Signature.call "sum" [Argument.create("X", dataType.Integer); Argument.create("X", dataType.Integer); Argument.create("Y1", dataType.Integer)];
            Signature.call "sum" [Argument.create("X", dataType.Integer); Argument.create("Y1", dataType.Integer); Argument.create("Y", dataType.Integer);]
            ]
        let r = ConRule "3x" prm cls
        let kb = DefaultKB.Add(r)
        let tcall = Signature.call "3x" [Argument.create 3; Argument.create 9]
        let fcall = Signature.call "3x" [Argument.create 3; Argument.create 10]
        let res call = find kb Context.EmptyContext call |> Seq.length
        let tcall' = res tcall
        let fcall' = res fcall
        tcall' |> should equal 1
        fcall' |> should equal 0

    [<Test>]
    let ``Call complicated(d = 2) with context rule``() =
        let prm = [Parameter.create("X", dataType.Integer); Parameter.create("Y", dataType.Integer)]
        let cls = [
            Signature.call "sum" [Argument.create("X", dataType.Integer); Argument.create("X", dataType.Integer); Argument.create("Y1", dataType.Integer)];
            Signature.call "sum" [Argument.create("X", dataType.Integer); Argument.create("Y1", dataType.Integer); Argument.create("Y", dataType.Integer);]
            ]
        let r = ConRule "3x" prm cls
        let kb = DefaultKB.Add(r)
        let tcall = Signature.call "3x" [Argument.create 3; Argument.create 9]
        let fcall = Signature.call "3x" [Argument.create 3; Argument.create 10]
        let context = Context.singleton (Variable.intVar "X") (Value.create(1))

        let res call = find kb context call
        let tcall' = res tcall
        let fcall' = res fcall
        tcall' |> Seq.length |> should equal 1
        fcall' |> Seq.length |> should equal 0

        let h = Seq.head tcall'
        h |> should equal context

TestSearchMachine.starttest 1000 3 5
TestSearchMachine.starttest 1000 3 3
System.Console.ReadKey() |> ignore