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
    let varkb (n: string) = Knowledgebase.Empty.Append <| rule.fact(Signature.define name [Parameter.create n])
    let vc k (v: int) = Map.empty.Add(Variable.create k, Value.create v)
    let call (i: int) = Signature.call name [Argument.create i]
    let varcall (n: string) = Signature.call name [Argument.create n]
    
    let find kb c call =
        let sm = SearchMachine.SearchMachines.Simple.Create()
        sm.kb <- kb
        sm.Execute(call, c)

    [<Test>]
    let ``Add fact, check it``() =
        let res = find (kb 1) Context.EmptyContext (call 1) |> Seq.length
        res |> should equal 1
    
    [<Test>]
    let ``Add fact, check it with context``() =
        let call = Signature.call name [Argument.create "C"]
        let context = Context.EmptyContext.Add(Variable.create "C", Value.create 1)
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
        let prm = [Parameter.create("X"); Parameter.create("Y")]
        let cls = [Signature.call "sum" [Argument.create("X"); Argument.create("X"); Argument.create("Y")]]
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
        let prm = [Parameter.create("X"); Parameter.create("Y")]
        let cls = [
            Signature.call "sum" [Argument.create("X"); Argument.create("X"); Argument.create("Y1")];
            Signature.call "sum" [Argument.create("X"); Argument.create("Y1"); Argument.create("Y");]
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
        let prm = [Parameter.create("X"); Parameter.create("Y")]
        let cls = [
            Signature.call "sum" [Argument.create("X"); Argument.create("X"); Argument.create("Y1")];
            Signature.call "sum" [Argument.create("X"); Argument.create("Y1"); Argument.create("Y");]
            ]
        let r = ConRule "3x" prm cls
        let kb = Knowledgebase.Default.Append r
        let tcall = Signature.call "3x" [Argument.create 3; Argument.create 9]
        let fcall = Signature.call "3x" [Argument.create 3; Argument.create 10]
        let context = Context.singleton (Variable.create "X") (Value.create(1))

        let res call = find kb context call
        let tcall' = res tcall
        let fcall' = res fcall
        tcall' |> Seq.length |> should equal 1
        fcall' |> Seq.length |> should equal 0

        let h = Seq.head tcall'
        h |> should equal context

[<TestFixture>]
module CustomRulesTest =
    let parent (machine: ISearchMachine) (p: string) (c: string) =
        let def = Signature.define "parent" [Parameter.create(p); Parameter.create(c)]
        let fact = rule.fact(def)
        machine.AddRule fact
        
    let isgrandparent(): rule =
        let def = Signature.define "grandparent" [Parameter.create("G"); Parameter.create("C")]
            
        let calls = [Signature.call "parent" 
                        [Argument.create("G"); Argument.create("P")];
                    Signature.call "parent" 
                        [Argument.create("P"); Argument.create("C")]]
        rule.ConcatenatedRule(def, calls)

    let parentcall (parent: string) (child: string) =
        Signature.call "parent" [Argument.create(parent); Argument.create(child)]

    [<Test>]
    let ``Simulate grandparent by parents``() =
        let machine = SearchMachines.Simple.Create()
        parent machine "grandparent" "parent"
        parent machine "parent" "child"
        let context = Context.EmptyContext // .Add(Variable.strVar "G1", Value.create "grandparent")
        let contexts = machine.Execute(parentcall "Parent" "Child", context)
        Seq.length contexts |> should equal 2

    [<Test>]
    let ``Call custom parents and grandparents rule``() =
        let machine = SearchMachines.Simple.Create()
        parent machine "andrew" "pasha"
        parent machine "alesha" "misha"
        parent machine "misha" "sasha"
        parent machine "misha" "yura"

        machine.AddRule(isgrandparent())
        let context = Context.EmptyContext.Add(Variable.create "Alesha", Value.create "alesha").Add(Variable.create "Sasha", Value.create "sasha")

        let call = Signature.call "grandparent" [Argument.create "Alesha"; Argument.create "Sasha"]
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