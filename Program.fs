module Program

open System

[<EntryPoint>]
let main argv =
    // Actors.runTests ()
    MVU.runProgram ()
    0 // return an integer exit code
