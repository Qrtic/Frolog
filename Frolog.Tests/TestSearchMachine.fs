namespace Frolog.Tests

open Frolog
open Frolog.SearchMachines

[<RequireQualifiedAccess>]
module Distributions =
    let random min max = 
        let r = new System.Random()
        fun (x) -> r.Next(min, max)

module TestSearchMachine =
    let starttest (factsGet) (queriesGet) factsN queriesN times precedences =
        let test (m: ISearchMachine) (s: Signature list): (int64 * Signature array array) =
            let res = Array.create s.Length [||]
            let sw = System.Diagnostics.Stopwatch.StartNew()
            for i in 0..s.Length-1 do
                let sign = s.[i]
                let contexts = m.Execute sign |> Seq.toArray
                res.[i] <- contexts
            sw.ElapsedMilliseconds, res
            
        let prms = {maxPrecedences = precedences }
        let prepare m facts =
            let addrule (s: #ISearchMachine) r = s.AddRule r
            facts |> List.iter(fun r -> addrule m r)
            // Factorial.appendFactorial m
        let simpleGet(facts)() = 
            let machine = SearchMachines.Simple.Create()
            prepare machine facts
            machine :> ISearchMachine
        let fifocacheGet(facts)() = 
            let machine = SearchMachines.Custom.CacheFirstMachine prms
            prepare machine facts
            machine :> ISearchMachine
        let lifocacheGet(facts)() = 
            let machine = SearchMachines.Custom.CacheLastMachine prms
            prepare machine facts
            machine :> ISearchMachine

        /// Returns hits, ms, res
        let getIter getter qs =
            let _iter (mGet: unit -> #ISearchMachine) queries ret =
                let m = mGet()
                ret(m, test m queries)
            _iter getter qs (fun (m: ISearchMachine, r) -> 
                                    match m with
                                    | :? SearchMachines.Simple as simpl -> 0, fst r, snd r
                                    | :? SearchMachines.Custom as custm -> custm.CacheHits, fst r, snd r
                                    | _ -> failwith "Unknown type of search machine.")

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
            printfn "_______________________________"

        printfn "Test finished."