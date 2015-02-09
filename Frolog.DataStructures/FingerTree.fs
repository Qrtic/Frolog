namespace Frolog.DataStructures

(*
[<RequireQualifiedAccess>]
module Common =
    let inline (!>) (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)
    
type node<'a> = Branch2 of 'a * 'a | Branch3 of 'a * 'a * 'a
    with
    member __.ToList = function
        | Branch2(a, b)    -> [a;b]
        | Branch3(a, b, c) -> [a;b;c]  
    static member From(a, b) = Branch2(a, b)
    static member From(a, b, c) = Branch3(a, b, c)
    static member FromList = function
        | [a;b] -> Branch2(a, b)
        | [a;b;c] -> Branch3(a, b, c)
        | _ -> failwith "Finger tree node can be instantiated only with 2 or 3 argument list."
    static member op_Implicit(a:'a list) = node<'a>.FromList(a)

type affix<'a> = One of 'a | Two of 'a*'a | Three of 'a*'a*'a | Four of 'a*'a*'a*'a

module Node =
    let toFinger = function
                   | Branch2(a, b) -> Two(a, b)
                   | Branch3(a, b, c) -> Three(a, b, c)

module Affix =
    let append a x = 
        match a with
        | One(a) -> Two(a, x)
        | Two(a,b) -> Three(a, b, x)
        | Three(a,b,c) -> Four(a, b, c, x)
        | Four(_,_,_,_) -> failwith "Cant append an element to four elements affix"
    let prepend a x = 
        match a with
        | One(a) -> Two(x, a)
        | Two(a,b) -> Three(x, a, b)
        | Three(a,b,c) -> Four(x, a, b, c)
        | Four(_,_,_,_) -> failwith "Cant append an element to four elements affix"
    let peekLeft = function
        | One(a) -> a
        | Two(a,_) -> a
        | Three(a,_,_) -> a
        | Four(a,_,_,_) -> a
    let peekRight = function
            | One(a) -> a
            | Two(_,b) -> b
            | Three(_,_,c) -> c
            | Four(_,_,_,d) -> d
    let popLeft = function
        | One(a) -> failwith "Cant pop from singleton affix"
        | Two(_,b) -> One(b)
        | Three(_,b,c) -> Two(b,c)
        | Four(_,b,c,d) -> Three(b,c,d)
    let popRight = function
        | One(a) -> failwith "Cant pop from singleton affix"
        | Two(_,b) -> One(b)
        | Three(_,b,c) -> Two(b, c)
        | Four(_,b,c,d) -> Three(b, c, d)
    let tryFind a f =
        match a with
        | One(a) -> if f a then Some a else None
        | Two(a,b) -> 
            if f a then Some a
            elif f b then Some b
            else None
        | Three(a,b,c) -> 
            if f a then Some a
            elif f b then Some b
            elif f c then Some c
            else None
        | Four(a,b,c,d) -> 
            if f a then Some a
            elif f b then Some b
            elif f c then Some c
            elif f d then Some d
            else None

type CommonAffixOperations() =
    static member tryFind<'n> a f =
        match a with
        | One(a) -> if f a then Some a else None
        | Two(a,b) -> 
            if f a then Some a
            elif f b then Some b
            else None
        | Three(a,b,c) -> 
            if f a then Some a
            elif f b then Some b
            elif f c then Some c
            else None
        | Four(a,b,c,d) -> 
            if f a then Some a
            elif f b then Some b
            elif f c then Some c
            elif f d then Some d
            else None

type fingerTree<'a> = Empty | Single of 'a | Deep of prefix: affix<'a> * deeper: fingerTree<node<'a>> * suffix: affix<'a>

module FingerTree =
    (*let peekLeft t = 
        match t with
        | Empty -> failwith "Cant peek from empty tree"
        | Single(v) -> v
        | Deep(l, _, _) -> Affix.peekLeft l
    let peekRight t =
        match t with
        | Empty -> failwith "Cant peek from empty tree"
        | Single(v) -> v
        | Deep(_, _, r) -> Affix.peekRight r*)
    let empty<'a> = fingerTree<'a>.Empty

type CommonFingerOperations() =
    static member peekLeft (this : fingerTree<'T>) = 
        match this with
        | Empty -> failwith "Cant peek from empty tree"
        | Single(v) -> v
        | Deep(l, _, _) -> Affix.peekLeft l
    static member peekRight (this : fingerTree<'T> ) =
        match this with
        | Empty -> failwith "Cant peek from empty tree"
        | Single(v) -> v
        | Deep(_, _, r) -> Affix.peekRight r
    static member append<'T> (this : fingerTree<'T> ) (x : 'T) : fingerTree<'T> = 
                            match this with
                            | Empty -> Single(x)
                            | Single(a) -> Deep(One(x), Empty, One(a))
                            | Deep(Four(a,b,c,d), deeper, s) -> Deep(Two(x, a), CommonFingerOperations.append deeper (Branch3(b,c,d)), s)
                            | Deep(p, d, s) -> Deep(Affix.append p x, d, s)
    static member prepend<'T> (this : fingerTree<'T> ) (x : 'T) : fingerTree<'T> = 
                            match this with
                            | Empty -> Single(x)
                            | Single(a) -> Deep(One(a), Empty, One(x))
                            | Deep(p, deeper, Four(a,b,c,d)) -> Deep(p, CommonFingerOperations.prepend deeper (Branch3(a,b,c)), Two(d, x))
                            | Deep(p, d, s) -> Deep(p, d, Affix.prepend s x)
    static member popLeft (this : fingerTree<'t>) =
        let peekLeft t = 
            match t with
            | Empty -> failwith ""
            | Single(v) -> v
            | Deep(l, _, _) -> Affix.peekLeft l
        match this with
        | Empty -> failwith "Cant pop from empty tree"
        | Single(_) -> Empty
        | Deep(One(_), Empty, One(v)) -> Single(v)
        | Deep(One(_), Empty, r) -> Deep(One(Affix.peekLeft r), Empty, Affix.popLeft r)
        | Deep(One(_), m, r) -> Deep (Node.toFinger (peekLeft m), (CommonFingerOperations.popLeft m), r)
        | Deep(l, m, r) ->  Deep (Affix.popLeft l, m, r)

module FingerTree =
    let append = CommonFingerOperations.Append
    let prepend = CommonFingerOperations.Prepend*)

module SimpleFingerTree =
    let flip f x y = f y x
    let raiseImpossible() = failwith "this should never happen"
    let raiseEmpty() = failwith "tree is empty"

    type Finger<'T> = 
        | One of 'T
        | Two of 'T * 'T
        | Three of 'T * 'T * 'T
        | Four of 'T * 'T * 'T * 'T

        member x.SeqLeft = 
            seq { match x with
                    | One(a) -> yield a
                    | Two(a, b) -> yield a; yield b
                    | Three(a, b, c) -> yield a; yield b; yield c
                    | Four(a, b, c, d) -> yield a; yield b; yield c; yield d  }
        member x.SeqRight = 
            seq { match x with
                    | One(a) -> yield a
                    | Two(b, a) -> yield a; yield b
                    | Three(c, b, a) -> yield a; yield b; yield c
                    | Four(d, c, b, a) -> yield a; yield b; yield c; yield d  }

    type Node<'T> = 
        | Node2 of 'T * 'T
        | Node3 of 'T * 'T * 'T

    type FingerTree<'T> = 
        | Empty
        | Single of 'T
        | Multi of Finger<'T> * FingerTree<Node<'T>> * Finger<'T>

    module Nodes = 
        let seqLeft t = 
            seq { match t with
                    | Node2(a, b) -> yield a; yield b
                    | Node3(a, b, c) -> yield a; yield b; yield c }

        let seqRight t = 
            seq { match t with
                    | Node2(b, a) -> yield a; yield b
                    | Node3(c, b, a) -> yield a; yield b; yield c }

        let nodeToFinger n = 
            match n with
            | Node2(a, b) -> Two(a, b)
            | Node3(a, b, c) -> Three(a, b, c)


    module Fingers =
        let peekLeft (t : Finger<_>) = t.SeqLeft |> Seq.head 
        let peekRight (t : Finger<_>) = t.SeqRight |> Seq.head 
        let pushLeft a = function
            | One(b) -> Two(a, b)
            | Two(b, c) -> Three(a, b, c)
            | Three(b, c, d) -> Four(a, b, c, d)
            | _ -> raiseImpossible()
        let popLeft = function
            | Two(_, a) -> One(a)
            | Three(_, a, b) -> Two(a, b)
            | Four(_, a, b, c) -> Three(a, b, c)
            | _ -> raiseImpossible()
        let pushRight a = function
            | One(b) -> Two(b, a)
            | Two(c, b) -> Three(c, b, a)
            | Three(d, c, b) -> Four(d, c, b, a)
            | _ -> raiseImpossible()
        let popRight = function
            | Two(a, _) -> One(a)
            | Three(b, a, _) -> Two(b, a)
            | Four(c, b, a, _) -> Three(c, b, a)
            | _ -> raiseImpossible()
        let seqLeft (t : Finger<_>) =  t.SeqLeft
        let seqRight (t : Finger<_>) = t.SeqRight

    (* common functions *)   
    let peekLeft t = 
        match t with
        | Empty -> raiseEmpty()
        | Single(v) -> v
        | Multi(l, _, _) -> Fingers.peekLeft l

    let peekRight t =
        match t with
        | Empty -> raiseEmpty()
        | Single(v) -> v
        | Multi(_, _, r) -> Fingers.peekRight r

    type private CommonOperations() =

        static member pushLeft<'T> (this : FingerTree<'T> ) (a : 'T) : FingerTree<'T> = 
            match this with
            | Empty -> Single(a)
            | Single(b) -> Multi(One a, Empty, One b)
            | Multi(Four(b, c, d, e), m, r) -> Multi(Two(a, b), CommonOperations.pushLeft m (Node3(c,d,e)), r)
            | Multi(l, m, r) -> Multi(Fingers.pushLeft a l, m, r)

        static member pushRight<'T > (this : FingerTree<'T> ) (a : 'T) : FingerTree<'T> = 
            match this with
            | Empty -> Single(a)
            | Single(b) -> Multi(One b, Empty, One a)
            | Multi(l, m, Four(e, d, c, b)) -> Multi(l, CommonOperations.pushRight m (Node3(e, d, c)), Two(b, a))
            | Multi(l, m, r) -> Multi(l, m, Fingers.pushRight a r)
    
        static member popLeft<'T> (this : FingerTree<'T> )  : FingerTree<'T> =
            match this with
            | Empty -> raiseEmpty()
            | Single(_) -> Empty
            | Multi(One(_), Empty, One(v)) -> Single(v)
            | Multi(One(_), Empty, r) -> Multi(One(Fingers.peekLeft r), Empty, Fingers.popLeft r)
            | Multi(One(_), m, r) -> Multi (Nodes.nodeToFinger (peekLeft m), (CommonOperations.popLeft m), r)
            | Multi(l, m, r) ->  Multi (Fingers.popLeft l, m, r)

        static member popRight<'T> (this : FingerTree<'T> ) : FingerTree<'T> =
            match this with
            | Empty -> raiseEmpty()
            | Single(_) -> Empty
            | Multi(One(v), Empty, One(_)) -> Single(v)
            | Multi(l, Empty, One(_)) -> Multi(Fingers.popRight l, Empty, One (Fingers.peekRight l))
            | Multi(l, m, One(_)) -> Multi(l, (CommonOperations.popRight m), Nodes.nodeToFinger (peekRight m))   
            | Multi(l, m, r) -> Multi(l, m, Fingers.popRight r)

        static member seqLeft<'T> (t : FingerTree<'T>) : seq<'T> = 
            seq { match t with
                    | Empty -> ()
                    | Single v -> yield v
                    | Multi(l, m, r) ->
                        yield! Fingers.seqLeft l
                        yield! CommonOperations.seqLeft m |> Seq.collect Nodes.seqLeft
                        yield! Fingers.seqLeft r  }

        static member seqRight<'T> (t : FingerTree<'T>) : seq<'T> = 
            seq { match t with
                    | Empty -> ()
                    | Single v -> yield v
                    | Multi(l, m, r) ->
                        yield! Fingers.seqRight r
                        yield! CommonOperations.seqRight m |> Seq.collect Nodes.seqRight
                        yield! Fingers.seqRight l  }

        static member flatten a x b = [ yield! Fingers.seqLeft a; yield! x; yield! Fingers.seqLeft b]
    
        static member nodes l = 
            match l with
            | [a; b] -> [Node2(a, b)]
            | [a; b; c] -> [Node3(a, b, c)]
            | [a; b; c; d] -> [Node2(a, b); Node2(c, d)]
            | a::b::c::xs -> Node3(a, b, c)::CommonOperations.nodes(xs)
            | _ -> raiseImpossible()

        static member concat<'T> (t1 : FingerTree<'T>) (ts : 'T list ) (t2 : FingerTree<'T>) : FingerTree<'T> = 
            match t1, t2 with
            | (Empty, t) -> (ts, t) ||> List.foldBack (flip CommonOperations.pushLeft)
            | (t, Empty) -> (t, ts) ||> List.fold CommonOperations.pushRight
            | (Single v, t) -> (v::ts, t) ||> List.foldBack (flip CommonOperations.pushLeft)
            | (t, Single v) -> (t, v::ts) ||> List.fold (CommonOperations.pushRight)
            | Multi(l1, m1, r1), Multi(l2, m2, r2) -> Multi(l1,  CommonOperations.concat m1 (CommonOperations.nodes (CommonOperations.flatten r1 ts l2))  m2, r2)

    let create = Empty
    let pushLeft = CommonOperations.pushLeft
    let pushRight = CommonOperations.pushRight
    let popLeft = CommonOperations.popLeft
    let popRight = CommonOperations.popRight
    let seqLeft = CommonOperations.seqLeft
    let seqRight = CommonOperations.seqRight
    let concat t1 t2 = CommonOperations.concat t1 [] t2