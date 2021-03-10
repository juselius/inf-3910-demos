module TDD

open System.Text.RegularExpressions

let private rmatch pat str =
    let regex = Regex pat
    let m = regex.Match str
    if m.Success then Some str else None

let (|PhoneNr|_|) =
    rmatch "^(\+\d{1,2}\s?)?\-?\s?\(?\d{3}\)?[\s-]?\d{2}[\s-]?\d{3}$"

let (|EmailAddr|_|) =
    rmatch "^([a-zA-Z0-9_\-\.]+)@([a-zA-Z0-9_\-\.]+)\.([a-zA-Z]{2,5})$"

type Gender =  M | F

type ContactMethod =
    private
    | Phone of string
    | Email of string
    | Both of string * string
with
    static member CreateContactMethod phoneOrEmail =
        match phoneOrEmail with
        | PhoneNr x -> Phone x
        | EmailAddr x -> Email x
        | _ -> failwith "Contact method is required"

    static member CreateContactMethod (phone, ?email) =
        if email.IsSome then
            match phone, email.Value with
            | PhoneNr nr, EmailAddr addr -> Both (nr, addr)
            | _, _ -> failwith "Contact method is required"
        else
            match phone with
            | PhoneNr nr -> Phone nr
            | _ -> failwith "Contact method is required"


type Contact = {
    FirstName : string
    LastName : string
    MiddleNames : string list
    Gender : Gender option
    ContactMethod : ContactMethod
}

let tel = "+47 474 19 869"
let mail = "jonas@juselus.io"

let testContactMethod = function
    | PhoneNr x -> x
    | EmailAddr x -> x
    | _ -> "no match"

try
    let c = {
            FirstName = "Jonas"
            LastName = "Juselius"
            MiddleNames = []
            Gender = Some M
            ContactMethod = ContactMethod.CreateContactMethod  (mail, mail)
        }
    printfn "%A" c
with exn ->
    printfn "%s" exn.Message
    printfn "%s" exn.StackTrace
