namespace Frolog.Tests

open NUnit.Framework

open Frolog
open Frolog.DefineRule
open Frolog.SearchMachines
open Frolog.CustomRules

type FactOptions = None | Variable | Defined | Custom of Rule | Multiple of Rule seq
type SignatureOptions = FactValueSignature | AnotherValueSignature | VarSignature | CustomSignature of Signature
    
[<TestFixture>]
module SimpleTest =
    let forceFact x = tryDefFact x |> Option.get
    let forceSign x = Option.bind sign (term x) |> Option.get
    let signOpt (x: string) = SignatureOptions.CustomSignature(forceSign x)

    let check(factop) (signatureOp) =
        printfn "Signature(%A) with Rule=(%A)" signatureOp factop
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
        let result = sm.Execute(Signature) |> Seq.toList
        printfn "Signature executed and results in = %A" result
        result

    let checklen factop signatureop =
        check factop signatureop |> List.length
        
    let check_2 factop signatureop =
        let len = checklen factop signatureop 
        Assert.AreEqual(len, 2)

    let check_1 factop signatureop =
        let len = checklen factop signatureop 
        Assert.AreEqual(len, 1)

    let check_0 factop signatureop =
        let len = checklen factop signatureop 
        Assert.AreEqual(len, 0)
        
    let check_eq factop signatureop res =
        let callres = check factop signatureop |> Set.ofSeq
        let expectedres = Set.ofList res
        let intersectLen = Set.intersect callres expectedres |> Set.count
        Assert.AreEqual(intersectLen, Set.count expectedres)

    // Check time for all iterations in ms
    let check_time rules signatures maxTime =
        let sm = SearchMachines.Simple.Create()
        sm.kb <- Knowledgebase.Default
        for r in rules do sm.AddRule r

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
        for c in signatures do
            sm.Execute(c) |> ignore
        sw.Stop()
        Assert.LessOrEqual(sw.ElapsedMilliseconds, maxTime)

    [<Test>]
    let ``Simple. Signature fact with it value``() =
        check_1 FactOptions.Variable SignatureOptions.FactValueSignature
    [<Test>]
    let ``Simple. Signature fact with another value``() =
        check_0 FactOptions.Defined SignatureOptions.AnotherValueSignature
    [<Test>]
    let ``Simple. Signature fact with variable``() =
        check_1 FactOptions.Variable SignatureOptions.VarSignature
        
    [<Test>]
    let ``Simple. Signature var fact with another value``() =
        check_1 FactOptions.Variable SignatureOptions.FactValueSignature
    [<Test>]
    let ``Simple. Signature var fact with it value``() =
        check_1 FactOptions.Variable SignatureOptions.AnotherValueSignature
    [<Test>]
    let ``Simple. Signature var fact with variable``() =
        check_1 FactOptions.Variable SignatureOptions.VarSignature
            
    [<Test>]
    let ``Simple. Check inc predicate``() =
        check_1 FactOptions.None (signOpt "++(1, 2)")
        check_0 FactOptions.None (signOpt "++(1, 3)")
    [<Test>]
    let ``Simple. Check sum predicate``() =
        check_1 FactOptions.None (signOpt "+(1, 2, 3)")
        check_0 FactOptions.None (signOpt "+(1, 2, 4)")
    [<Test>]
    let ``Simple. Check div predicate``() =
        check_1 FactOptions.None (signOpt "/(10, 2, 5)")
        check_0 FactOptions.None (signOpt "/(10, 2, 4)")
    // NO LISTS YET
    // [<Test>]
    // let ``Simple. Check divs predicate``() =
    //    check_1 FactOptions.None (signOpt "divs(10, .(1,.(2,.(5,10))))")
    //    check_0 FactOptions.None (signOpt "divs(10, .(1,.(2,.(5))))")
    [<Test>]
    let ``Simple. Check mul predicate``() =
        check_1 FactOptions.None (signOpt "*(2, 3, 6)")
        check_0 FactOptions.None (signOpt "*(2, 3, 10)")
    [<Test>]
    let ``Simple. Check greater predicate``() =
        check_1 FactOptions.None (signOpt ">(5, 4)")
        check_0 FactOptions.None (signOpt ">(5, 5)")
        check_0 FactOptions.None (signOpt ">(5, 6)")

    [<Test>]
    let ``Simple. Signature custom Rule with d=1``() =
        let cls = forceSign "x2(X, Y)"
        let r = defConcatRule cls (defBody [forceSign "+(X, X, Y)"])
        check_1 (FactOptions.Custom(r)) (signOpt "x2(3, 6)")
        check_0 (FactOptions.Custom(r)) (signOpt "x2(3, 7)")
    
    [<Test>]
    let ``Simple. Signature custom Rule with d=2``() =
        let cls = forceSign "x3(X, Y)"
        let r = defConcatRule cls (defBody [forceSign "+(X, X, Y1)"; forceSign "+(Y1, X, Y)"])
        check_1 (FactOptions.Custom(r)) (signOpt "x3(3, 9)")
        check_0 (FactOptions.Custom(r)) (signOpt "x3(3, 10)")
        
    [<Test>]
    let ``Simple. Check Signature custom Rule(d=2) with Context doesnt changes the Context``() =
        let cls = forceSign "x3(X, Y)"
        let r = defConcatRule cls (defBody [forceSign "+(X, X, Y1)"; forceSign "+(Y1, X, Y)"])
        check_eq (FactOptions.Custom(r)) (signOpt "x3(3, 9)") [forceSign "x3(3, 9)"]
        check_eq (FactOptions.Custom(r)) (signOpt "x3(3, A)") [forceSign "x3(3, 9)"]
        check_eq (FactOptions.Custom(r)) (signOpt "x3(3, 10)") []

    [<Test>]
    let ``Simple. Check Signature custom Rule(d=3) with Context doesnt changes the Context``() =
        let cls = forceSign "x4(X, Y)"
        let r = defConcatRule cls (defBody [forceSign "+(X, X, Y1)"; forceSign "+(Y1, X, Y2)"; forceSign "+(Y2, X, Y)"])
        check_eq (FactOptions.Custom(r)) (signOpt "x4(3, 12)") [forceSign "x4(3, 12)"]
        check_eq (FactOptions.Custom(r)) (signOpt "x4(3, A)") [forceSign "x4(3, 12)"]
        check_eq (FactOptions.Custom(r)) (signOpt "x4(3, 13)") []

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
        let ``Grandparent. Check fully initialized rules.``() =
            let factopt = FactOptions.Multiple([parent "andrew" "pasha"; parent "alesha" "misha"; parent "misha" "sasha"; parent "misha" "yura"; isgrandparent()])
            check_1 factopt (signOpt "grandparent(alesha, sasha)")
            check_1 factopt (signOpt "grandparent(alesha, yura)")
            check_0 factopt (signOpt "grandparent(andrew, pasha)")
            check_0 factopt (signOpt "grandparent(alesha, misha)")

        [<Test>]
        let ``Grandparent. Check len of variable rules.``() =
            let factopt = FactOptions.Multiple([parent "andrew" "pasha"; parent "alesha" "misha"; parent "misha" "sasha"; parent "misha" "yura"; isgrandparent()])
            check_2 factopt (signOpt "grandparent(alesha, X)")
            check_0 factopt (signOpt "grandparent(andrew, X)")
            check_0 factopt (signOpt "grandparent(misha, X)")

        [<Test>]
        let ``Grandparent. Check with variables.``() =
            let factopt = FactOptions.Multiple([parent "andrew" "pasha"; parent "alesha" "misha"; parent "misha" "sasha"; parent "misha" "yura"; isgrandparent()])
            check_eq factopt (signOpt "grandparent(alesha, X)") [forceSign "grandparent(alesha, sasha)"; forceSign "grandparent(alesha, yura)"]
            
        [<Test>]
        let ``Factorial. Signature factorial``() =
            let factorial = (FactOptions.Multiple([Factorial.fromZero; Factorial.fromN]))
            
            check_eq factorial (signOpt "factorial(1, 1)") [forceSign "factorial(1, 1)"]
            check_eq factorial (signOpt "factorial(2, 2)") [forceSign "factorial(2, 2)"]
            check_eq factorial (signOpt "factorial(2, A)") [forceSign "factorial(2, 2)"]

            check_1 factorial (signOpt "factorial(0, 1)")
            check_1 factorial (signOpt "factorial(1, 1)")
            check_1 factorial (signOpt "factorial(2, 2)")
            check_1 factorial (signOpt "factorial(3, 6)")
            check_1 factorial (signOpt "factorial(4, 24)")

        [<TestFixture>]
        module TimeTest =
            let r = new System.Random()
            let person(max) = r.Next(max).ToString()
            let nFacts = 1000
            let nCalls = 10000
            let maxTime = 30L * 1000L

            [<Test>]
            let ``Performance. Signature simple facts``() =   
                let facts = [1..nFacts] |> List.map(fun x -> forceFact (sprintf "b(%s)" (person(nFacts))))
                let calls = [1..nCalls] |> List.map(fun x -> forceSign (sprintf "b(%s)" (person(nFacts))))
                check_time facts calls maxTime
            [<Test>]
            let ``Performance. Signature 2d Rules``() =
                let parents = [1..nFacts] |> List.map(fun x -> parent (person(nFacts)) (person(nFacts)))
                let calls = [1..nCalls] |> List.map(fun x -> forceSign (sprintf "grandparent(%s, %s)" (person(nFacts)) (person(nFacts))))
                check_time (isgrandparent()::parents) calls maxTime