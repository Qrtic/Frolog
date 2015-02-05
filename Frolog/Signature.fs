namespace Frolog

open Frolog.Common

[<StructuredFormatDisplay("{AsString}")>]
type Signature = Signature of Term
    with
    member s.AsString =
        let (Signature(t)) = s
        t.AsString
    static member AsTerm signature =
        let (Signature(term)) = signature
        term
    static member GetName (Signature(Structure(name, _))) = name
    static member GetArguments (Signature(Structure(_, args))) = args

[<AutoOpen>]
module SignatureHelper =
    let sign term =
        match term with
        | Structure(_, _) -> Some(Signature(term))
        | _ -> None
    let signf term =
        match term with
        | Structure(_, _) -> Signature(term)
        | _ ->  failwith "Cant instantiate signature"
        