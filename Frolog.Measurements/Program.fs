namespace Frolog.Tests

open Frolog
open Frolog.DefineRule
open Frolog.CustomRules

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

    // TODO: generalize to depth of the rule
    type measureParameters = { factsN: int; factsD: int; queriesN: int; queriesD: int; precedencesN: int }
    let measureGGp (mp: measureParameters) resCallback =
        let prms = { maxPrecedences = mp.precedencesN }
        let rf = Distributions.random 1 mp.factsD >> (fun o -> o.ToString())
        let rq = Distributions.random 1 mp.queriesD >> (fun o -> o.ToString())
        
        let facts = [1..mp.factsN] |> List.map(fun _ -> GGrandparent.parent (rf()) (rf()))
        let queries = [1..mp.queriesN] |> List.map(fun _ -> defCall2 "ggrandparent" (rq()) (rq()))

        let simple = MachinePrepare.fifocacheGet facts prms
        let fifo = MachinePrepare.fifocacheGet facts prms
        let lifo = MachinePrepare.lifocacheGet facts prms

        let sr = MachineMeasure.measureMachine simple queries
        resCallback "Simple" sr
        let fr = MachineMeasure.measureMachine fifo queries
        resCallback "Fifo" fr
        let lr = MachineMeasure.measureMachine lifo queries
        resCallback "Lifo" lr

    [<EntryPoint>]
    let main args =
        let sb = new System.Text.StringBuilder()
        let wrf (s: string) = sb.AppendLine(s) |> ignore
        let watch = System.Diagnostics.Stopwatch.StartNew()

        for fd in [10; 25; 100; 500] do
            printfn "Check fd = %i" fd
            for f in [25; 100; 500; 1000] do
                for qd in [10; 25; 100; 500] do
                    for q in [100; 500; 1000; 2500] do
                        for p in [q / 100; q / 10; q] do
                            let prms = { factsN = f; factsD = fd; queriesN = q; queriesD = qd; precedencesN = p }
                            wrf <| sprintf "Parameters: %A" prms
                            measureGGp prms (fun name res -> wrf <| sprintf "%s executes in %i with %i hits." name res.time res.hits)
        printfn "Wait for result. Executed in %i ms." watch.ElapsedMilliseconds
        System.IO.File.WriteAllText(System.DateTime.Now.ToShortDateString() + ".txt", sb.ToString())
        System.Console.ReadKey() |> ignore
        0