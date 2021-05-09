module Monads

type Option<'T> =
    | Some of 'T
    | None
with
    static member map f =
        function
        | Some x -> Some (f x)
        | None -> None

    static member apply f x =
        match f with
        | Some f' -> Option<_>.map f' x
        | None -> None

    static member return' = Some

    static member bind (f: 'a -> Option<'b>) (x: Option<'a>) =
        let x' = Option<_>.map f x
        match x' with
        | Some result -> result
        | None -> None

type OptionBuilder() =
    let (<*>) = Option<_>.apply
    member __.Bind(x, f) = Option<_>.bind f x
    member __.Return x = Option<_>.return' x
    member __.RetrunFrom (x: Option<'a>) = x
    member __.Bind2(x, f) = Option<_>.apply f x
    member __.Zero () = None

let option = OptionBuilder()

let f a b c = a + b + c

let testM () =
    option {
        let! a = Some 5
        let! b = Some 4
        let! c = Some 1
        return f a b c
    }

let testA () =
    let (<*>) = Option<_>.apply
    Some f <*> Some 5 <*> Some 10 <*> Some 1

