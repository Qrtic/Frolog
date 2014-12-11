open SearchLib
open SearchLib.Context
open SearchLib.SearchMachine
open SearchLib.Signature
open SearchLib.Rule

open NUnit.Framework
open FsUnit

[<TestFixture>]
module SimpleTest =
    let dtype = dataType.Integer
    let name = "b"
    let kb (i: int) = Knowledgebase.Empty.Append <| rule.fact(Signature.define name [Parameter.create i])
    let varkb (n) = Knowledgebase.Empty.Append <| rule.fact(Signature.define name [Parameter.create(n, dtype)])
    let vc k (v: int) = Map.empty.Add(Variable.intVar k, Value.create v)
    let call (i: int) = Signature.call name [Argument.create i]
    let varcall n = Signature.call name [Argument.create(n, dtype)]
    
    let find kb c call =
        let sm = SearchMachine.SearchMachines.Simple.Create()
        sm.kb <- kb
        sm.Execute(call, c)

    [<Test>]
    let ``Add fact, check it``() =
        let kb = kb 1
        let call = call 1
        let res = find kb Context.EmptyContext call |> Seq.length
        res |> should equal 1
    
    [<Test>]
    let ``Add fact, check it with context``() =
        let call = Signature.call name [Argument.create("C", dataType.Integer)]
        let context = Context.EmptyContext.Add(Variable.intVar "C", Value.create 1)
        let rest = find (kb 1) context call |> Seq.length
        let resf = find (kb 2) context call |> Seq.length
        rest |> should equal 1
        resf |> should equal 0

    [<Test>]
    let ``Add fact, check another``() =
        let kb = kb 1
        let call = call 2
        let res = find kb Context.EmptyContext call |> Seq.length
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
        let res = find kb Context.EmptyContext call |> Seq.length
        res |> should equal 1

    [<Test>]
    let ``Call standart predicates``() =
        let kb = Knowledgebase.Default
        let inc = Signature.call "inc" [Argument.create 1; Argument.create 2]
        let sum = Signature.call "sum" [Argument.create 1; Argument.create 2; Argument.create 3]
        let divs = Signature.call "divs" [Argument.create 10; Argument.create [1;2;5;10]]
        let res call = find kb Context.EmptyContext call |> Seq.length
        res inc |> should equal 1
        res sum |> should equal 1
        res divs |> should equal 1

    [<Test>]
    let ``Call complicated(d = 1) rule``() =
        let prm = [Parameter.create("X", dataType.Integer); Parameter.create("Y", dataType.Integer)]
        let cls = [Signature.call "sum" [Argument.create("X", dataType.Integer); Argument.create("X", dataType.Integer); Argument.create("Y", dataType.Integer)]]
        let r = ConRule "2x" prm cls
        let kb = Knowledgebase.Default.Append r
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
        let kb = Knowledgebase.Default.Append r
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
        let kb = Knowledgebase.Default.Append r
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

[<TestFixture>]
module CustomRulesTest =
    [<Test>]
    let ``Call custom parents and grandparents rule``() =
        let machine = SearchMachines.Simple.Create()
        let parent (p: string) (c: string) =
            let def = Signature.define "parent" [Parameter.create(p); Parameter.create(c)]
            let fact = rule.fact(def)
            machine.AddRule fact

        let isgrandparent: rule =
            let def = Signature.define "grandparent" [Parameter.create("G", dataType.String); Parameter.create("C", dataType.String)]
            
            let calls = [Signature.call "parent" 
                            [Argument.create("G", dataType.String); Argument.create("P", dataType.String)];
                        Signature.call "parent" 
                            [Argument.create("P", dataType.String); Argument.create("C", dataType.String)]]
            rule.ConcatenatedRule(def, calls)

        parent "andrew" "pasha"
        parent "alesha" "misha"
        parent "misha" "sasha"
        parent "misha" "yura"

        machine.AddRule(isgrandparent)
        let context = Context.EmptyContext.Add(Variable.strVar "Alesha", Value.create "alesha").Add(Variable.strVar "Sasha", Value.create "sasha")

        let call = Signature.call "grandparent" [Argument.create("Alesha", dataType.String); Argument.create("Sasha", dataType.String)]
        let res = machine.Execute(call, context)
        printfn "%d" (Seq.length res)
        for r in res do
            printfn "%A" r
        Seq.length res |> should equal 1

open CustomRulesTest

``Call custom parents and grandparents rule``() |> ignore
TestSearchMachine.starttest 1000 3 5
TestSearchMachine.starttest 1000 3 3
System.Console.ReadKey() |> ignore