namespace Frolog.Tests

open Frolog
open Frolog.DefineRule
open Frolog.Tests.TestSearchMachine

module Main =
    let defFact name x =
        tryDefFact (sprintf "%s(%i)" name x) |> Option.get
        
    let defCall name x =
        sprintf "%s(%i)" name x |> termf |> sign |> Option.get

    let defCall2 name x y =
        sprintf "%s(%s, %s)" name x y |> termf |> sign |> Option.get

    let createFacts predName (distrib: int -> int) size: Rule list =
        [1..size] |> List.map(fun x -> defFact predName (distrib x))
        
    let createCustomFacts (distrib: int -> 'a) factG size: Rule list =
        [1..size] |> List.map(fun x -> factG (distrib x))

    let createSimpleQueries predName (distrib: int -> int) size: Signature list =
        [1..size] |> List.map(fun x -> defCall predName (distrib x))
    
    let createCustomQueries (distrib: int -> 'a) callDef size: Signature list =
        [1..size] |> List.map(fun x -> callDef (distrib x))
        
    let createFibsQueries (distrib: int -> int) size =
        createCustomQueries distrib (fun x -> defCall2 "factorial" (string x) "F") size

    let testFacts() =
        starttest 
            (createFacts "b" (Distributions.random 1 1000))
            (createSimpleQueries "b" (Distributions.random 1 1000))
            1500
            3000
            5
            3000

    let testGGp distrib facts queries precedences =
        let r = Distributions.random 1 distrib
        let next() = r().ToString()
        let factsGet = createCustomFacts (fun _ -> next(), next()) (fun (g, c) -> GGrandparent.parent g c)
        let queryGet = createCustomQueries (fun _ -> next(), next()) (fun (g, c) -> defCall2 "ggrandparent" g c)
        starttest
            factsGet
            queryGet
            facts
            queries
            1
            precedences

    [<EntryPoint>]
    let main args =
        // starttest (fun x -> []) (createFibsQueries (Distributions.random 1 10)) 0 200 2 100
        use stream = System.IO.File.Open("testresults" + System.DateTime.Now.ToString() + ".txt", System.IO.FileMode.CreateNew)
        use wr = new System.IO.StreamWriter(stream)
        System.Console.SetOut(wr)
        for d in [10; 25; 50; 100; 500; 1000] do
            for f in [25; 50; 100; 500; 1000; 2500] do
                for q in [100; 1000; 3000; 5000] do
                    for p in [q / 100; q / 10; q; q * 10; q * 100] do
                        printfn "Use %i distribution coefficient" d
                        testGGp d f q p |> ignore
            wr.Flush()
        System.Console.ReadKey() |> ignore
        wr.Flush()
        0