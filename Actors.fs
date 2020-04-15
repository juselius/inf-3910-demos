module Actors

open System.Threading

// Example 1: mutable state
let actor1 = MailboxProcessor.Start (fun inbox ->
    let mutable state = 0
    let rec loop () =
        async {
            let! n = inbox.Receive ()
            printfn "received %d, state %d" n state
            state <- n
            return! loop ()
        }
    loop ()
    )
actor1.Post 1
actor1.Post 2
// Example 2: ephemeral state
let actor2 = MailboxProcessor.Start (fun inbox ->
    let rec loop state =
        async {
            let! n = inbox.Receive ()
            printfn "received %d, state %d" n state
            return! loop (state + n)
        }
    loop 0
    )
actor2.Post 3
actor2.Post 4
// Example 3: reply
let actor3 = MailboxProcessor.Start (fun (inbox : MailboxProcessor<AsyncReplyChannel<int> * int>) ->
    let rec loop state =
        async {
            let! (reply, n) = inbox.Receive ()
            reply.Reply state
            return! loop (state + n)
        }
    loop 0
    )
actor3.PostAndReply (fun reply -> (reply, 21)) |> printfn "reply: %d"
actor3.PostAndReply (fun reply -> (reply, 42)) |> printfn "reply: %d"
// Example 4: serial, ordered queue
let actor4 = MailboxProcessor.Start (fun inbox ->
    let mutable state = 0
    let rnd = System.Random(1000)
    let rec loop () =
        async {
            let! n = inbox.Receive ()
            printf "in %d " state
            state <- n + state
            Thread.Sleep (rnd.Next ())
            printfn "out %d " state
            return! loop ()
        }
    loop ()
)

let test4 () =
    printfn "Start"
    actor4.Post 1
    actor4.Post 2
    actor4.Post 3
    printfn "Sent"
    Thread.Sleep 5000
    printfn "Done"
// Example 5: Msg type
type Msg =
    | Set of int
    | Get of AsyncReplyChannel<int>

let actor5 = MailboxProcessor.Start (fun inbox ->
    let mutable state = 0
    let rec loopy () =
        async {
            match! inbox.Receive () with
            | Set n -> state <- n + state
            | Get reply -> reply.Reply state
            return! loopy ()
        }
    loopy ()
)

let test5 () =
    printfn "Start"
    [1..3]
    |> List.iter (Set >> actor5.Post)
    printfn "Sent"
    let x = actor5.PostAndReply Get
    printfn "Done %d" x
// Example 6: 5 in parallel
let postAsync n = async { Set n |> actor5.Post }

let test6 () =
    printfn "Start"
    [1..7]
    |> List.map postAsync
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
    printfn "Sent"
    let x = actor5.PostAndReply Get
    printfn "Done %d" x
// let runTests () =
//     test4 ()
//     test5 ()
//     test6 ()
//     Thread.Sleep 5000