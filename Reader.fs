module Reader

type Reader<'e, 'a> = Reader of ('e -> 'a)

let retn x = Reader (fun _ -> x)

let run (Reader f) e = f e

let bind (f : 'a -> Reader<'e, 'b>) (Reader g) =
    Reader (fun e -> run (f (g e)) e)

let ask = Reader (fun e -> e)

type ReaderBuilder() =
    member __.Bind(v, f) = bind f v

    member __.Return v = retn v

    member __.ReturnFrom v = Reader (run v)

let reader = ReaderBuilder()

let x : Reader<string, int> =
    reader {
        let! e = ask
        printfn "1: %s" e
        let! q = Reader (fun e -> printfn "2: %s" e; -5)
        return 5 + q
    }

printfn "%A" (run x "hello")
