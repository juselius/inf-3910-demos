[<AutoOpen>]
module Model

open Shared

type Model = {
    Count: int
    People : Person list
    Sort : bool option
    NewPerson : Person option
}

