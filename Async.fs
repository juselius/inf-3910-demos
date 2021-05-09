module AsyncDemo

open System
open System.Threading

type Cont<'r, 'a> = Cont of (('a -> 'r) -> 'r)

type Task<'a> = Task of (('a -> unit) -> unit)

let inline p x = printfn "%A" x

// simple async constant synchronously
let a () =
  async {
    return "hello world."
  }
a () |> Async.RunSynchronously |> p

// async computation with mutable
let mutable _x = 0
let b x =
  async {
    _x <- x
    return ()
  }
b 42 |> Async.Start
p _x
p _x
p _x
p _x

// async computation with I/O
let minutes ms =
    let m = (float ms) / 1000.0 / 60.0
    async {
        return m
    }

let minutes' ms = (float ms) / 1000.0 / 60.0 |> async.Return

let sleepy n =
  let rnd = Random ()
  let s = rnd.NextDouble () * 10000.0 |> int
  Thread.Sleep s
  async {
    let! m = minutes s
    return p (n, m)
  }
sleepy 4 |> Async.Start

// parallel computation
let l =
  5
  |> List.unfold (fun n -> if n > 0 then Some (sleepy n, n - 1) else None)

l |> Async.Parallel
  |> Async.Ignore
  |> Async.Start