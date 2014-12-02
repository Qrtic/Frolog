module TestSearchMachine

open SearchLib.Rule
open SearchLib.Signature
open SearchLib.SearchMachine

let createRandomFacts size: rule list =
    let r = System.Random()
    let rec create i l =
        if i >= size then l
        else
            let prms = [r.Next(size).ToString()]
            create (i+1) ((Fact "b" prms)::l)
    create 1 []
    
/// creates with "b"(int, int) facts
let createSimpleTest size =
    let machine = SearchMachines.Simple.Empty()
    let addrule (s: ISearchMachine) r = s.AddRule r
    createRandomFacts size |> List.iter(fun r -> machine.AddRule r)
    machine

/// creates with "b"(int, int) facts
let createFIFOCacheTest size prms =
    let machine = SearchMachines.Custom.CacheFirstMachine prms
    let addrule (s: ISearchMachine) r = s.AddRule r
    createRandomFacts size |> List.iter(fun r -> machine.AddRule r)
    machine

let createLIFOCacheTest size prms =
    let machine = SearchMachines.Custom.CacheLastMachine prms
    let addrule (s: ISearchMachine) r = s.AddRule r
    createRandomFacts size |> List.iter(fun r -> machine.AddRule r)
    machine

let createSigTest size = 
    createRandomFacts size |> List.map(fun f -> f.Signature)

let test (m: ISearchMachine) (s: signature list) =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    for sign in s do
        m.Execute sign |> ignore
    sw.ElapsedMilliseconds

let starttest size times sizek =
    let queries = [1..times] |> List.map(fun i -> createSigTest size)

    let simpleIter (f: unit -> SearchMachines.Simple) queries =
        let simple = f()
        let testsimple = test simple queries
        testsimple
    let customIter (f: unit -> SearchMachines.Custom) queries =
        let custom = f()
        let testsimple = test custom queries
        let hits = custom.CacheHits
        testsimple, hits
       
    let simpleg = fun () -> createSimpleTest size
    let testsimple() =
        let mutable time = 0L
        for i in 1..times do
            let q = List.nth queries (i - 1)
            time <- time + (simpleIter simpleg q)
        printfn "Simple db performance executed in %d ms." time
        
    let prms = {maxPrecedences = size / sizek }
    let fifocacheg = fun () -> createFIFOCacheTest size prms
    let lifocacheg = fun () -> createLIFOCacheTest size prms
    let testcustom name getter =
        let mutable time = 0L
        let mutable hits = 0
        for i in 1..times do
            let q = List.nth queries (i - 1)
            let t, h = customIter getter q
            time <- time + t
            hits <- hits + h
        
        printfn "%s performance executed in %d ms. Hits = %d." name time hits

    printfn "Test started. Iterations = %d. Size = %d. Size k = %d." times size sizek
    testsimple()
    testcustom "FIFO cache" fifocacheg
    testcustom "LIFO cache" lifocacheg
    printfn "Test finished."