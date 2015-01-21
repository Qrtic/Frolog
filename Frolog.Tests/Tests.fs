namespace Frolog.Tests

open NUnit.Framework
open FsUnit

open Frolog
open Frolog.DefineRule
open Frolog.SearchMachines

type FactOptions = None | Variable | Defined | Custom of Rule | Multiple of Rule seq
type ContextOptions = WithFactValue | WithAnotherValue | Customized of Context | Empty
type SignatureOptions = FactValueSignature | AnotherValueSignature | VarSignature | CustomSignature of Signature
    
[<TestFixture>]
module SimpleTest =
    let forceFact x = tryDefFact x |> Option.get
    let forceSign x = Option.bind sign (term x) |> Option.get
    let signOpt (x: string) = SignatureOptions.CustomSignature(forceSign x)

    let check(factop) (contextOp) (signatureOp) =
        printfn "Signature(%A) with Context=(%A) with Rule=(%A)" signatureOp contextOp factop
        let sm = SearchMachines.Simple.Create()
        sm.kb <- Knowledgebase.Default
        match factop with
            | None -> ()
            | Custom(r) -> sm.AddRule r
            | Multiple(rs) ->
                for r in rs do
                    sm.AddRule r
            | Variable -> sm.AddRule(forceFact "b(C)")
            | Defined -> sm.AddRule(forceFact "b(1)")

        let Signature = match signatureOp with
                           | FactValueSignature -> forceSign "b(1)"
                           | AnotherValueSignature -> forceSign "b(2)"
                           | VarSignature -> forceSign "b(D)"
                           | CustomSignature(c) -> c
        let add = ContextHelper.add
        let cont = match contextOp with
                   | WithFactValue -> add (BoundedVariable("D", Value("1"))) Context.Empty
                   | WithAnotherValue -> add (BoundedVariable("D", Value("2"))) Context.Empty
                   | Customized(con) -> con
                   | Empty -> Context.Empty
        let result = sm.Execute(Signature, cont) |> Seq.toList
        printfn "Signature executed and results in = %A" result
        result

    let checklen factop Contextop Signatureop =
        check factop Contextop Signatureop |> List.length
        
    let check_1 factop Contextop Signatureop =
        checklen factop Contextop Signatureop |> should equal 1

    let check_0 factop Contextop Signatureop =
        checklen factop Contextop Signatureop |> should equal 0
        
    let check_eq factop Contextop Signatureop res =
        check factop Contextop Signatureop |> Set.ofList |> Set.difference(Set.ofSeq(res)) |> Set.isEmpty |> should equal true

    // Check time for all iterations in ms
    let check_time Rules Signatures maxTime =
        let sm = SearchMachines.Simple.Create()
        sm.kb <- Knowledgebase.Default
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
        for c in Signatures do
            sm.Execute(c) |> ignore
        sw.Stop()
        sw.ElapsedMilliseconds |> should lessThan maxTime

    [<Test>]
    let ``Simple. Signature fact with it value``() =
        check_1 FactOptions.Variable ContextOptions.Empty SignatureOptions.FactValueSignature
    [<Test>]
    let ``Simple. Signature fact with another value``() =
        check_0 FactOptions.Defined ContextOptions.Empty SignatureOptions.AnotherValueSignature
    [<Test>]
    let ``Simple. Signature fact with variable``() =
        check_1 FactOptions.Variable ContextOptions.Empty SignatureOptions.VarSignature
        
    [<Test>]
    let ``Simple. Signature var fact with another value``() =
        check_1 FactOptions.Variable ContextOptions.Empty SignatureOptions.FactValueSignature
    [<Test>]
    let ``Simple. Signature var fact with it value``() =
        check_1 FactOptions.Variable ContextOptions.Empty SignatureOptions.AnotherValueSignature
    [<Test>]
    let ``Simple. Signature var fact with variable``() =
        check_1 FactOptions.Variable ContextOptions.Empty SignatureOptions.VarSignature
    
    [<Test>]
    let ``Simple. Signature fact with same Context``() =
        check_1 FactOptions.Defined ContextOptions.WithFactValue SignatureOptions.VarSignature
    [<Test>]
    let ``Simple. Signature fact with another Context``() =
        check_0 FactOptions.Defined ContextOptions.WithAnotherValue SignatureOptions.VarSignature
    [<Test>]
    let ``Simple. Signature fact with empty Context``() =
        check_1 FactOptions.Defined ContextOptions.Empty SignatureOptions.VarSignature
        
    [<Test>]
    let ``Simple. Check inc predicate``() =
        check_1 FactOptions.None ContextOptions.Empty (signOpt "++(1, 2)")
        check_0 FactOptions.None ContextOptions.Empty (signOpt "++(1, 3)")
    [<Test>]
    let ``Simple. Check sum predicate``() =
        check_1 FactOptions.None ContextOptions.Empty (signOpt "+(1, 2, 3)")
        check_0 FactOptions.None ContextOptions.Empty (signOpt "+(1, 2, 4)")
    [<Test>]
    let ``Simple. Check div predicate``() =
        check_1 FactOptions.None ContextOptions.Empty (signOpt "/(10, 2, 5)")
        check_0 FactOptions.None ContextOptions.Empty (signOpt "/(10, 2, 4)")
    // NO LISTS YET
    // [<Test>]
    // let ``Simple. Check divs predicate``() =
    //    check_1 FactOptions.None ContextOptions.Empty (signOpt "divs(10, .(1,.(2,.(5,10))))")
    //    check_0 FactOptions.None ContextOptions.Empty (signOpt "divs(10, .(1,.(2,.(5))))")
    [<Test>]
    let ``Simple. Check mul predicate``() =
        check_1 FactOptions.None ContextOptions.Empty (signOpt "*(2, 3, 6)")
        check_0 FactOptions.None ContextOptions.Empty  (signOpt "*(2, 3, 10)")
    [<Test>]
    let ``Simple. Check greater predicate``() =
        check_1 FactOptions.None ContextOptions.Empty (signOpt ">(5, 4)")
        check_0 FactOptions.None ContextOptions.Empty (signOpt ">(5, 5)")
        check_0 FactOptions.None ContextOptions.Empty (signOpt ">(5, 6)")

    [<Test>]
    let ``Simple. Signature custom Rule with d=1``() =
        let cls = forceSign "x2(X, Y)"
        let r = defConcatRule cls (defBody [forceSign "+(X, X, Y)"])
        check_1 (FactOptions.Custom(r)) ContextOptions.Empty (signOpt "x2(3, 6)")
        check_0 (FactOptions.Custom(r)) ContextOptions.Empty (signOpt "x2(3, 7)")
    
    [<Test>]
    let ``Simple. Signature custom Rule with d=2``() =
        let cls = forceSign "x3(X, Y)"
        let r = defConcatRule cls (defBody [forceSign "+(X, X, Y1)"; forceSign "+(Y1, X, Y)"])
        check_1 (FactOptions.Custom(r)) ContextOptions.Empty (signOpt "x3(3, 9)")
        check_0 (FactOptions.Custom(r)) ContextOptions.Empty (signOpt "x3(3, 10)")

    [<Test>]
    let ``Simple. Check Signature custom Rule(d=2) with Context doesnt changes the Context``() =
        let cls = forceSign "x3(X, Y)"
        let context = ContextHelper.add(Term.BoundedVariable("X", Value("1"))) Context.Empty
        let r = defConcatRule cls (defBody [forceSign "+(X, X, Y1)"; forceSign "+(Y1, X, Y)"])
        check_eq (FactOptions.Custom(r)) (ContextOptions.Customized(context)) (signOpt "x3(3, 9)") [RuleOutput([Value("1")])]
        check_eq (FactOptions.Custom(r)) (ContextOptions.Customized(context)) (signOpt "x3(3, 10)") []

    [<TestFixture>]
    module CustomRulesTest =
        let parent (p: string) (c: string) =
            forceFact (sprintf "parent(%s, %s)" p c)
        
        let isgrandparent(): Rule =
            let def = forceSign "grandparent(G, C)"
            defConcatRule def (defBody [forceSign "parent(G, P)"; forceSign "parent(P, C)"])

        let parentSignature (parent: string) (child: string) =
            forceSign (sprintf "parent(%s, %s)" parent child)
            
        [<Test>]
        let ``Grandparent. Signature grandparents Rule``() =
            let factopt = FactOptions.Multiple([parent "andrew" "pasha"; parent "alesha" "misha"; parent "misha" "sasha"; parent "misha" "yura"; isgrandparent()])
            check_1 factopt ContextOptions.Empty (signOpt "grandparent(alesha, sasha)")
            check_1 factopt ContextOptions.Empty (signOpt "grandparent(alesha, yura)")
            check_0 factopt ContextOptions.Empty (signOpt "grandparent(andrew, pasha)")
            check_0 factopt ContextOptions.Empty (signOpt "grandparent(alesha, misha)")

        [<Test>]
        let ``Grandparent. Signature grandparents Rule with Context``() =
            let factopt = FactOptions.Multiple([parent "andrew" "pasha"; parent "alesha" "misha"; parent "misha" "sasha"; parent "misha" "yura"; isgrandparent()])
            let context1 = ContextHelper.add (BoundedVariable("C", Value("sasha"))) (ContextHelper.signelton(BoundedVariable("G", Value("alesha"))))
            let context2 = ContextHelper.add (BoundedVariable("C", Value("yura"))) (ContextHelper.signelton(BoundedVariable("G", Value("alesha"))))
            let context3 = ContextHelper.add (BoundedVariable("C", Value("pasha"))) (ContextHelper.signelton(BoundedVariable("G", Value("andrew"))))
            let context4 = ContextHelper.add (BoundedVariable("C", Value("misha"))) (ContextHelper.signelton(BoundedVariable("G", Value("alesha"))))
            check_1 factopt (ContextOptions.Customized(context1)) (signOpt "grandparent(G, C")
            check_1 factopt (ContextOptions.Customized(context2)) (signOpt "grandparent(alesha, yura")
            check_0 factopt (ContextOptions.Customized(context3)) (signOpt "grandparent(andrew, pasha")
            check_0 factopt (ContextOptions.Customized(context4)) (signOpt "grandparent(alesha, misha")
            
            (*
        [<Test>]
        let ``Factorial. Signature factorial``() =
            let factorial = (FactOptions.Multiple([Frolog.Tests.Factorial.fromZero; Factorial.fromN]))
            check_1 factorial ContextOptions.Empty (signOpt "factorial(0, 1)")
            check_1 factorial ContextOptions.Empty (signOpt "factorial(1, 1)")
            check_1 factorial ContextOptions.Empty (signOpt "factorial(2, 2)")
            check_1 factorial ContextOptions.Empty (signOpt "factorial(3, 6)")
            *)

        [<TestFixture>]
        module TimeTest =
            let r = new System.Random()
            let person(max) = r.Next(max).ToString()
            let nFacts = 1000 * 1000
            let nSignatures = 1000 * 1000
            let maxTime = 10 * 1000

            [<Test>]
            let ``Performance. Signature simple facts``() =   
                let facts = [1..nFacts] |> List.map(fun x -> forceFact (sprintf "b(%s)" (person(nFacts))))
                let signatures = [1..nSignatures] |> List.map(fun x -> forceSign (sprintf "b(%s)" (person(nFacts))))
                check_time facts signatures maxTime
            [<Test>]
            let ``Performance. Signature 2d Rules``() =
                let parents = [1..nFacts] |> List.map(fun x -> parent (person(nFacts)) (person(nFacts)))
                let signatures = [1..nSignatures] |> List.map(fun x -> forceSign (sprintf "grandparent(%s, %s)" (person(nFacts)) (person(nFacts))))
                check_time (isgrandparent()::parents) signatures maxTime