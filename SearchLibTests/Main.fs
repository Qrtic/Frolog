namespace SearchLibTests

open SearchLib
open SearchLib.Context
open SearchLib.SearchMachine
open SearchLib.Rule

open SearchLibTests.TestSearchMachine

module Main =
    [<EntryPoint>]
    let main args =
        starttest createRandomFacts createRandomQueries 1000 1000 1 1
        starttest createRandomFacts createRandomQueries 1000 1000 1 1
        System.Console.ReadKey() |> ignore
        0