namespace Shared

type Counter = { Value : int }

type Person = {
    First : string
    Last : string
    Alias : string option
} with
    static member New = {
        First = ""
        Last = ""
        Alias = None
    }


