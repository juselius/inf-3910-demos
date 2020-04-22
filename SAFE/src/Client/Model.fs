[<AutoOpen>]
module Model

open Shared

type PersonId = int

type Model = {
    Count: int
    People : Person list
    Sort : bool option
    NewPerson : (PersonId * Person) option
}

