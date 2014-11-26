open SearchLib.SearchMachine
open SearchLib.Signature
open SearchLib.Knowledgebase
open SearchLib.Rule

// is parent to children
// let isparent = Rule "isparent" ["P"; "C"] (F2(fun p c -> Signature.Rejected))

let parent p c = Fact "parent" [p; c]
let isgrandparent: rule =
    let calls = [call "parent" ["G"; "P"]; call "parent" ["P"; "C"]]
    ConRule "grandparent" ["G"; "C"] calls

let parents = [parent "andrew" "pasha"; parent "alesha" "misha"; parent "misha" "sasha"; parent "misha" "yura"]
let testkb = [Fact "f" ["1"]; Fact "f" ["2"]] @ defaultRules @ parents @ [isgrandparent]
let textcontext = Map.empty.Add("Alesha", "alesha").Add("Sasha", "sasha")
let testmachine = SearchMachine(testkb, textcontext)
let test = testmachine.PrintAll({name = "grandparent"; parameters = ["Alesha"; "Sasha"]})

System.Console.ReadKey() |> ignore