module TestSearchMachine

open SearchLib.Rule
open SearchLib.Signature
open SearchLib.SearchMachine

let createRandomFacts size: rule list =
    let r = System.Random()
    let rec create i l =
        if i >= size then l
        else
            let prms = [r.Next(size/2).ToString(); r.Next(size/2).ToString()]
            create (i+1) ((Fact "b" prms)::l)
    create 1 []
    
/// creates with "b"(int, int) facts
let createSimpleTest size =
    let addrule (s: ISearchMachine) r = s.AddRule r
    createRandomFacts size |> List.fold addrule (SearchLib.SearchMachine.SearchMachine.Empty :> ISearchMachine)

/// creates with "b"(int, int) facts
let createCacheTest size =
    let addrule (s: ISearchMachine) r = s.AddRule r
    createRandomFacts size |> List.fold addrule (SearchLib.SearchMachine.CacheSearchMachine.Empty :> ISearchMachine)

let createSigTest size = 
    createRandomFacts size |> List.map(fun f -> f.Signature)

let test (m: ISearchMachine) (s: signature list) times =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    for i in 1..times do
        for sign in s do
            m.Execute sign |> ignore
    sw.ElapsedMilliseconds

let starttest size times =
    let simple = createSimpleTest size
    let cache = createCacheTest size
    let queries = createSigTest size
    let testsimple = test simple queries times
    let testcache = test cache queries times
    printfn "Simple db performance executed in %d ms" (testsimple)
    printfn "Cache db performance executed in %d ms" (testcache)