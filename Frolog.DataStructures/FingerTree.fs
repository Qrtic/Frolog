namespace Frolog.DataStructures

[<RequireQualifiedAccess>]
module Common =
    let inline (!>) (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)
    
type Node<'a> = Branch2 of 'a * 'a | Branch3 of 'a * 'a * 'a
    with
    member t.ToList = function
        | Branch2(a, b)    -> [a;b]
        | Branch3(a, b, c) -> [a;b;c]  
    static member From(a, b) = Branch2(a, b)
    static member From(a, b, c) = Branch3(a, b, c)
    static member FromList = function
        | [a;b] -> Branch2(a, b)
        | [a;b;c] -> Branch3(a, b, c)
        | _ -> failwith "Finger tree node can be instantiated only with 2 or 3 argument list."
    static member op_Implicit(a:'a list) = Node<'a>.FromList(a)

type Affix<'a> = One of 'a | Two of 'a*'a | Three of 'a*'a*'a | Four of 'a*'a*'a*'a
    with
    member a.ToList = 
        match a with
        | One(a) -> [a]
        | Two(a,b) -> [a;b]
        | Three(a,b,c) -> [a;b;c]
        | Four(a,b,c,d) -> [a;b;c;d]
    static member FromList = function
        | [a] -> One(a)
        | [a;b] -> Two(a,b)
        | [a;b;c] -> Three(a,b,c)
        | [a;b;c;d] -> Four(a,b,c,d)
        | _ -> failwith "Finger tree affix must have one to four elements"
    member a.Append x = x::a.ToList |> Affix<'a>.FromList
    member a.Prepend x = a.ToList @ [x] |> Affix<'a>.FromList
    static member op_Implicit(a:'a list) = Affix<'a>.FromList(a)

type FingerTree<'a> = Empty | Single of 'a | Deep of prefix: Affix<'a> * deeper: FingerTree<Node<'a>> * suffix: Affix<'a>

type CommonFingerOperations() =
    static member Append<'T> (this : FingerTree<'T> ) (x : 'T) : FingerTree<'T> = 
                            match this with
                            | Empty -> Single(x)
                            | Single(a) -> Deep(One(x), Empty, One(a))
                            | Deep(Four(a,b,c,d), deeper, s) -> Deep(Two(x, a), CommonFingerOperations.Append deeper (Branch3(b,c,d)), s)
                            | Deep(p, d, s) -> Deep(p.Append(x), d, s)
    static member Prepend<'T> (this : FingerTree<'T> ) (x : 'T) : FingerTree<'T> = 
                            match this with
                            | Empty -> Single(x)
                            | Single(a) -> Deep(One(a), Empty, One(x))
                            | Deep(p, deeper, Four(a,b,c,d)) -> Deep(p, CommonFingerOperations.Append deeper (Branch3(a,b,c)), Two(d, x))
                            | Deep(p, d, s) -> Deep(p, d, s.Prepend(x))
