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
        sprintf "%s(%i, %s)" name x y |> termf |> sign |> Option.get

    let createFacts predName (distrib: int -> int) size: Rule list =
        [1..size] |> List.map(fun x -> defFact predName (distrib x))
        
    let createSimpleQueries predName (distrib: int -> int) size: Signature list =
        [1..size] |> List.map(fun x -> defCall predName (distrib x))
    
    let createCustomQueries (distrib: int -> int) callDef size: Signature list =
        [1..size] |> List.map(fun x -> callDef (distrib x))
        
    let createFibsQueries (distrib: int -> int) size =
        createCustomQueries distrib (fun x -> defCall2 "factorial" x "F") size

    [<EntryPoint>]
    let main args =
        starttest (fun x -> []) (createFibsQueries (Distributions.random 1 2)) 200 1000 1 100
        System.Console.ReadKey() |> ignore
        0