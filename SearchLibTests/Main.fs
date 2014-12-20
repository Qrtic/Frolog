namespace SearchLibTests

open SearchLib
open SearchLib.Context
open SearchLib.SearchMachine
open SearchLib.Rule

open SearchLibTests.TestSearchMachine

module Main =
    [<EntryPoint>]
    let main args =        
        SearchLibTests.TestSearchMachine.starttest 1000 3 5
        SearchLibTests.TestSearchMachine.starttest 1000 3 3
        System.Console.ReadKey() |> ignore
        0