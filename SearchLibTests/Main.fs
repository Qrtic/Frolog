open SearchLib
open SearchLib.Context
open SearchLib.SearchMachine
open SearchLib.Rule

open NUnit.Framework
open FsUnit

type FactOptions = None | Variable | Defined | Custom of rule | Multiple of rule seq
type ContextOptions = WithFactValue | WithAnotherValue | Customized of context | Empty
type CallOptions = FactValueCall | AnotherValueCall | VarCall | CustomCall of Call
    
[<TestFixture>]
module SimpleTest =
    let check(factop) (contextop) (callop) =
        printfn "Call(%A) with context=(%A) with rule=(%A)" callop contextop factop
        let sm = SearchMachine.SearchMachines.Simple.Create()
        sm.kb <- Knowledgebase.Default
        match factop with
            | None -> ()
            | Custom(r) -> sm.AddRule r
            | Multiple(rs) ->
                for r in rs do
                    sm.AddRule r
            | Variable -> sm.AddRule(rule.fact(Signature.define("b", ["C"])))
            | Defined -> sm.AddRule(rule.fact(Signature.define("b", ["1"])))

        let call = match callop with
                       | VarCall -> Signature.call("b", ["D"])
                       | FactValueCall -> Signature.call("b", ["1"])
                       | AnotherValueCall -> Signature.call("b", ["2"])
                       | CustomCall(c) -> c
        let cont = match contextop with
                   | WithFactValue -> Context.EmptyContext.Add(Variable.create("D"), Value.create(1))
                   | WithAnotherValue -> Context.EmptyContext.Add(Variable.create("D"), Value.create(2))
                   | Customized(con) -> con
                   | Empty -> Context.EmptyContext
        let result = sm.Execute(call, cont) |> Seq.toList
        printfn "Call executed and results in = %A" result
        result

    let checklen factop contextop callop =
        check factop contextop callop |> List.length
        
    let check_1 factop contextop callop =
        checklen factop contextop callop |> should equal 1

    let check_0 factop contextop callop =
        checklen factop contextop callop |> should equal 0
        
    let check_eq factop contextop callop res =
        check factop contextop callop |> Set.ofList |> Set.difference(Set.ofSeq(res)) |> Set.isEmpty |> should equal true

    // Check time for all iterations in ms
    let check_time rules calls maxTime =
        let sm = SearchMachine.SearchMachines.Simple.Create()
        sm.kb <- Knowledgebase.Default
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
        for c in calls do
            sm.Execute(c) |> ignore
        sw.Stop()
        sw.ElapsedMilliseconds |> should lessThan maxTime

    [<Test>]
    let ``Simple. Call fact with it value``() =
        check_1 FactOptions.Variable ContextOptions.Empty CallOptions.FactValueCall
    [<Test>]
    let ``Simple. Call fact with another value``() =
        check_0 FactOptions.Defined ContextOptions.Empty CallOptions.AnotherValueCall
    [<Test>]
    let ``Simple. Call fact with variable``() =
        check_1 FactOptions.Variable ContextOptions.Empty CallOptions.VarCall
        
    [<Test>]
    let ``Simple. Call var fact with another value``() =
        check_1 FactOptions.Variable ContextOptions.Empty CallOptions.FactValueCall
    [<Test>]
    let ``Simple. Call var fact with it value``() =
        check_1 FactOptions.Variable ContextOptions.Empty CallOptions.AnotherValueCall
    [<Test>]
    let ``Simple. Call var fact with variable``() =
        check_1 FactOptions.Variable ContextOptions.Empty CallOptions.VarCall
    
    [<Test>]
    let ``Simple. Call fact with same context``() =
        check_1 FactOptions.Defined ContextOptions.WithFactValue CallOptions.VarCall
    [<Test>]
    let ``Simple. Call fact with another context``() =
        check_0 FactOptions.Defined ContextOptions.WithAnotherValue CallOptions.VarCall
    [<Test>]
    let ``Simple. Call fact with empty context``() =
        check_1 FactOptions.Defined ContextOptions.Empty CallOptions.VarCall
        
    [<Test>]
    let ``Simple. Check inc predicate``() =
        check_1 FactOptions.None ContextOptions.Empty (CallOptions.CustomCall(Signature.call("inc", [1; 2])))
        check_0 FactOptions.None ContextOptions.Empty (CallOptions.CustomCall(Signature.call("inc", [1; 3])))
    [<Test>]
    let ``Simple. Check sum predicate``() =
        check_1 FactOptions.None ContextOptions.Empty (CallOptions.CustomCall(Signature.call("sum", [1; 2; 3])))
        check_0 FactOptions.None ContextOptions.Empty (CallOptions.CustomCall(Signature.call("sum", [1; 2; 4])))
    [<Test>]
    let ``Simple. Check divs predicate``() =
        check_1 FactOptions.None ContextOptions.Empty (CallOptions.CustomCall(Signature.call("divs", [Argument.create 10; Argument.create [1;2;5;10]])))
        check_0 FactOptions.None ContextOptions.Empty (CallOptions.CustomCall(Signature.call("divs", [Argument.create 10; Argument.create [1;2;5]])))

    [<Test>]
    let ``Simple. Call custom rule with d=1``() =
        let cls = [Signature.call("sum", ["X"; "X"; "Y"])]
        let r = ConRule "2x" [Parameter.create "X"; Parameter.create "Y"] cls
        check_1 (FactOptions.Custom(r)) ContextOptions.Empty (CallOptions.CustomCall(Signature.call("2x", [3; 6])))
        check_0 (FactOptions.Custom(r)) ContextOptions.Empty (CallOptions.CustomCall(Signature.call("2x", [3; 7])))
    
    [<Test>]
    let ``Simple. Call custom rule with d=2``() =
        let prm = [Parameter.create("X"); Parameter.create("Y")]
        let cls = [
            Signature.call("sum", ["X"; "X"; "Y1"]);
            Signature.call("sum", ["X"; "Y1"; "Y";])
            ]
        let r = ConRule "3x" prm cls
        check_1 (FactOptions.Custom(r)) ContextOptions.Empty (CallOptions.CustomCall(Signature.call("3x", [3; 9])))
        check_0 (FactOptions.Custom(r)) ContextOptions.Empty (CallOptions.CustomCall(Signature.call("3x", [3; 10])))

    [<Test>]
    let ``Simple. Check call custom rule(d=2) with context doesnt changes the context``() =
        let prm = [Parameter.create("X"); Parameter.create("Y")]
        let cls = [
            Signature.call("sum", ["X"; "X"; "Y1"]);
            Signature.call("sum", ["X"; "Y1"; "Y";])
            ]
        let r = ConRule "3x" prm cls
        let context = Context.singleton (Variable.create "X") (Value.create(1))
        check_eq (FactOptions.Custom(r)) (ContextOptions.Customized(context)) (CallOptions.CustomCall(Signature.call("3x", [3; 9]))) [context]
        check_eq (FactOptions.Custom(r)) (ContextOptions.Customized(context)) (CallOptions.CustomCall(Signature.call("3x", [3; 10]))) []

    [<TestFixture>]
    module CustomRulesTest =
        let parent (p: string) (c: string) =
            rule.fact(Signature.define("parent", [p; c]))
        
        let isgrandparent(): rule =
            let def = Signature.define("grandparent", ["G"; "C"])
            
            let calls = [Signature.call("parent", ["G"; "P"]);
                        Signature.call("parent", ["P"; "C"])]
            rule.ConcatenatedRule(def, calls)

        let parentcall (parent: string) (child: string) =
            Signature.call("parent", [parent; child])
            
        [<Test>]
        let ``Grandparent. Call grandparents rule``() =
            let factopt = FactOptions.Multiple([parent "andrew" "pasha"; parent "alesha" "misha"; parent "misha" "sasha"; parent "misha" "yura"; isgrandparent()])
            check_1 factopt ContextOptions.Empty (CallOptions.CustomCall(Signature.call("grandparent", ["alesha"; "sasha"])))
            check_1 factopt ContextOptions.Empty (CallOptions.CustomCall(Signature.call("grandparent", ["alesha"; "yura"])))
            check_0 factopt ContextOptions.Empty (CallOptions.CustomCall(Signature.call("grandparent", ["andrew"; "pasha"])))
            check_0 factopt ContextOptions.Empty (CallOptions.CustomCall(Signature.call("grandparent", ["alesha"; "misha"])))

        [<Test>]
        let ``Grandparent. Call grandparents rule with context``() =
            let factopt = FactOptions.Multiple([parent "andrew" "pasha"; parent "alesha" "misha"; parent "misha" "sasha"; parent "misha" "yura"; isgrandparent()])
            let context1 = Context.EmptyContext.Add(Variable.create "G", Value.create "alesha").Add(Variable.create "C", Value.create "sasha")
            let context2 = Context.EmptyContext.Add(Variable.create "G", Value.create "alesha").Add(Variable.create "C", Value.create "yura")
            let context3 = Context.EmptyContext.Add(Variable.create "G", Value.create "andrew").Add(Variable.create "C", Value.create "pasha")
            let context4 = Context.EmptyContext.Add(Variable.create "G", Value.create "alesha").Add(Variable.create "C", Value.create "misha")
            check_1 factopt (ContextOptions.Customized(context1)) (CallOptions.CustomCall(Signature.call("grandparent", ["G"; "C"])))
            check_1 factopt (ContextOptions.Customized(context2)) (CallOptions.CustomCall(Signature.call("grandparent", ["alesha"; "yura"])))
            check_0 factopt (ContextOptions.Customized(context3)) (CallOptions.CustomCall(Signature.call("grandparent", ["andrew"; "pasha"])))
            check_0 factopt (ContextOptions.Customized(context4)) (CallOptions.CustomCall(Signature.call("grandparent", ["alesha"; "misha"])))

        [<TestFixture>]
        module TimeTest =
            let r = new System.Random()
            let person(max) = r.Next(max).ToString()
            let nFacts = 1000 * 1000
            let nCalls = 1000 * 1000
            let maxTime = 10 * 1000

            [<Test>]
            let ``Performance. Call simple facts``() =   
                let facts = [1..nFacts] |> List.map(fun x -> rule.fact(Signature.define("b", [person(nFacts)])))
                let calls = [1..nCalls] |> List.map(fun x -> Signature.call("b", [person(nFacts)]))
                check_time facts calls maxTime
            [<Test>]
            let ``Performance. Call 2d rules``() =
                let parents = [1..nFacts] |> List.map(fun x -> parent (person(nFacts)) (person(nFacts)))
                let calls = [1..nCalls] |> List.map(fun x -> Signature.call("grandparent", [person(nFacts); person(nFacts)]))
                check_time (isgrandparent()::parents) calls maxTime
     
[<EntryPoint>]
let main(args) =
    TestSearchMachine.starttest 1000 3 5
    TestSearchMachine.starttest 1000 3 3
    System.Console.ReadKey() |> ignore
    0