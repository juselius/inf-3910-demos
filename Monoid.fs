module Monoid

#if INTERACTIVE
#r "nuget: FSharpPlus"
#load "monoids.txt.fs"
#endif

open FSharpPlus
open MonoidsTxt

let s1 = "hello"
let s2 = " world! "

// closure
let ssum = s1 ++ s2  // sum is a string

// associativity
let s3 = "x"
let s4a = (s1 ++ s2) ++ s3
let s4b = s1 ++ (s2 ++ s3)
printfn "%A" (s4a = s4b)

// identity
printfn "%A" (s1 ++ "" = s1)
printfn "%A" ("" ++ s1 = s1)


let l1 = [ "hello" ]
let l2 = [ " world! " ]

// closure
let lsum = l1 ++ l2  // sum is a string

// associativity
let l3 = [ "x" ]
let l4a = (l1 ++ l2) ++ l3
let l4b = l1 ++ (l2 ++ l3)

// identity
printfn "%A" (l1 ++ [] = l1)
printfn "%A" ([] ++ l1 = l1)

// let (++) (o1: ^t option when ^t : (static member (+) : ^t * ^t -> ^t)) (o2: ^t option when ^t : (static member (+) : ^t * ^t -> ^t)) =
//     match o1, o2 with
//     | Some o1, Some o2 -> Some (o1 ++ o2)
//     | Some o1, None -> Some o1
//     | None, Some o2 -> Some o2
//     | None, None -> None

let o1 = Some "hello"
let o2 = Some " world! "

// closure
let osum = o1 ++ o2  // sum is a string

// associativity
let o3 = Some "x"
let o4a = (o1 ++ o2) ++ o3
let o4b = o1 ++ (o2 ++ o3)

// identity
printfn "%A" (o1 ++ None = o1)
printfn "%A" (None ++ o1 = o1)


let tmp1 =
    if o1.IsSome then
        if o2.IsSome then
            Some (o1.Value ++ o2.Value)
        else
            Some o1.Value
    else
        None
let res =
    if tmp1.IsSome then
        if o3.IsSome then
            Some (tmp1.Value ++ o3.Value)
        else
            Some tmp1.Value
    else
        None

let res' = o1 ++ o2 ++ o3

let wordCount s =
    s |> String.split [ " " ] |> Seq.length

let lineCount s =
    s |> String.split [ "\n" ] |> Seq.length

let counts = wordCount (p1 ++ " " ++ p2 ++ " " ++ p3 ++ " " ++ p4 ++ " " ++ p5)

let counts' =
    [| p1; p2; p3; p4; p5 |]
    |> Array.map wordCount
    |> Array.sum

let pcounts =
    [| p1; p2; p3; p4; p5 |]
    |> Array.Parallel.map wordCount
    |> Array.sum