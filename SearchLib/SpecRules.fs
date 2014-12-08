module SearchLib.SpecRules

open Rule
open Signature

let specRules = new System.Collections.Generic.Dictionary<string, bool>()

let code = """
-: f(1)
-: f(2)
-? f(1)
"""

let (-?) t = specRules.Add(t.ToString(), true)

(-?) "f(1)"

type Lexem = Int of int | Functor of char
type State = WhiteSpace | IntSpace of int

let rec lex (s: string) state = 
    if s = "" then
        match state with
        WhiteSpace -> []
        | IntSpace(n) -> [Int(n)]
    else
        let this = s.[0]
        let rest = s.Substring 1
        let toInt c = System.Int32.Parse (c.ToString())
        match this with
        | ' ' -> match state with
                   | WhiteSpace -> lex rest WhiteSpace
                   | IntSpace(n) -> Int(n) :: (lex rest WhiteSpace)
        | x when x >= '0' || x <= '9' -> 
            match state with
                | WhiteSpace -> lex rest (IntSpace(toInt x))
                | IntSpace(n) -> lex rest (IntSpace(n * 10 + toInt x))
        | 'f' ->
            match state with
                | WhiteSpace -> Functor(this)::(lex rest WhiteSpace)
                |IntSpace(n) -> Functor(this)::Int(n)::(lex rest WhiteSpace)
        | _ -> failwith "incorrect token"