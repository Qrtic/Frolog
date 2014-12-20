namespace SearchLibTests

open SearchLib
open SearchLib.Context
open SearchLib.Rule
open SearchLib.SearchMachine

module TestSearchMachine =
    let predicateName = "b"
    let createRandomFacts size: rule list =
        let r = System.Random()
        let rec create i l =
            if i >= size then l
            else
                let prms = [Parameter.create(r.Next(size))]
                create (i+1) ((Fact predicateName prms)::l)
        create 1 []
        
    let createRandomQueries size = 
        let createRandomQueries size: Call list =
            let r = System.Random()
            let rec create i l =
                if i >= size then l
                else
                    let args = [Argument.create(r.Next(size))]
                    create (i+1) ((Signature.call(predicateName, args))::l)
            create 1 []
        createRandomQueries size

    let starttest (factsGet) (queriesGet) factsN queriesN times precedences =
        let test (m: ISearchMachine) (s: Call list): (int64 * context array array) =
            let res = Array.create s.Length [||]
            let sw = System.Diagnostics.Stopwatch.StartNew()
            for i in 0..s.Length-1 do
                let sign = s.[i]
                let contexts = m.Execute sign |> Seq.toArray
                res.[i] <- contexts
            sw.ElapsedMilliseconds, res
        let _iter (mGet: unit -> #ISearchMachine) queries ret =
            let m = mGet()
            ret(m, test m queries)
            
        let prms = {maxPrecedences = precedences }
        let simpleGet(facts)() = 
            /// creates with "b"(int, int) facts
            let createSimpleTest =
                let machine = SearchMachines.Simple.Create()
                let addrule (s: ISearchMachine) r = s.AddRule r
                facts |> List.iter(fun r -> machine.AddRule r)
                machine
            createSimpleTest :> SearchMachine.ISearchMachine
        let fifocacheGet(facts)() = 
            /// creates with "b"(int, int) facts
            let createFIFOCacheTest prms =
                let machine = SearchMachines.Custom.CacheFirstMachine prms
                let addrule (s: ISearchMachine) r = s.AddRule r
                facts |> List.iter(fun r -> machine.AddRule r)
                machine
            createFIFOCacheTest prms :> SearchMachine.ISearchMachine
        let lifocacheGet(facts)() = 
            let createLIFOCacheTest prms =
                let machine = SearchMachines.Custom.CacheLastMachine prms
                let addrule (s: ISearchMachine) r = s.AddRule r
                facts |> List.iter(fun r -> machine.AddRule r)
                machine
            createLIFOCacheTest prms :> SearchMachine.ISearchMachine

        /// Returns hits, ms, res
        let getIter getter qs =
            _iter getter qs (fun (m: SearchMachine.ISearchMachine, r) -> 
                                            match m with
                                            | :? SearchMachines.Simple as simpl -> 0, fst r, snd r
                                            | :? SearchMachines.Custom as custm -> custm.CacheHits, fst r, snd r)

        let testSmth get' print' qs =
            let mutable time = 0L
            let mutable hits = 0
            let hits, time, res = getIter get' qs
            print' time hits
            res
            
        let testsimple(fs, qs) =
            testSmth (simpleGet fs) (fun time hits -> printfn "Simple db performance executed in %d ms." time) qs
        let testfifo(fs, qs) =
            testSmth (fifocacheGet fs) (printfn "Fifo db performance executed in %d ms. Hits = %d") qs
        let testlifo(fs, qs) =
            testSmth (lifocacheGet fs) (printfn "Lifo db performance executed in %d ms. Hits = %d") qs
            
        printfn "Test started. Iterations = %d. Facts = %d. Queries = %d. Max precedences = %d." times factsN queriesN precedences
        for i in 1..times do
            let facts = factsGet factsN
            let queries = queriesGet queriesN

            let ressimple = testsimple(facts, queries)
            let resfifo = testfifo(facts, queries)
            let reslifo = testlifo(facts, queries)

            // Check results are same
            let aresameres t1 t2 =
                Array.forall2(fun a b -> Array.forall2(fun c d -> c = d) a b) t1 t2
                
            if (not (aresameres ressimple resfifo)) then
                printfn "Fifo results are wrong."
            if (not (aresameres ressimple reslifo)) then
                printfn "Lifo results are wrong."

        printfn "Test finished."