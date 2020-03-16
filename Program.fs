open System

let testFragment x = x + 1

let anotherFragment = List.reduce (+)

let main argv =
    let a = testFragment 5
    let b = anotherFragment [1; 2; 3]
    printfn "Hello World from F#! %d %d" a b
    0 // return an integer exit code
