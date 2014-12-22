namespace SearchLibTests

open SearchLib
open SearchLib.Context
open SearchLib.SearchMachine
open SearchLib.Rule

open SearchLibTests.TestSearchMachine

module Main =
    let createFacts predName (distrib: int -> int) size: rule list =
        [1..size] |> List.map(fun x -> Fact predName [Parameter.create (distrib x)])
        
    let createQueries predName (distrib: int -> int) size: Call list =
        [1..size] |> List.map(fun x -> Signature.call(predName, [Argument.create (distrib x)]))

    let createFibsQueries (distrib: int -> int) size =
        [1..size] |> List.map(fun x -> Signature.call("factorial", [Argument.create(distrib x); Argument.create "F"]))

    [<EntryPoint>]
    let main args =
        starttest (fun x -> []) (createFibsQueries (Distributions.random 1 2)) 200 1000 1 100
        System.Console.ReadKey() |> ignore
        0