// This modules contains some (contrived) examples of the most essential
// Object-Oriented Programmin features in F#.
module Oops

// Create type aliases for readabilty and domain modelling
type FirstName = string
type LastName = string
type Name = string
type Email = string
type Phone = string

type ContactMethod =
    | Email of Email
    | Phone of Phone
    | EmailAndPhone of Email * Phone

type ContactInfo = {
    FirstName : FirstName
    MiddleName : Name option
    LastName : LastName
    Contact : ContactMethod
}

// class definition with constructor
type MyContact
    ( firstName : FirstName,
      lastName : LastName,
      contact : ContactMethod,
      ?middleName : Name) = // optional argument to constuctor

    // private property with default value
    let middleOrDefault = defaultArg middleName ""
    // private mutable prop (backing store)
    let mutable dangerous = true

    // do stuff upon construction, must return ()
    do
        printf "Hello "
        printfn "%s %s" firstName lastName

    // alternative, overloaded constructor
    // must call primary constructor as final statement
    new () =
        MyContact( "Reodor", "Felgen", Phone "1234321" )

    // initialze immutable properties, with implicit getters
    member this.FirstName = firstName
    member this.LastName = lastName
    member this.MiddleName = middleOrDefault
    member this.Contact = contact

    // public property with getter and setter
    member this.Danger
        with get () = dangerous
        and set x = dangerous <- x

    // automatic (mutable) property with getter and setter
    member val DangerousToo = "careful, careful" with get, set

    // static member, without "this"
    static member Hello x = "Hello " + x

    // abstract member
    abstract member ShowContact : unit -> string

    // default implmentation. can be overridden
    default this.ShowContact () =
        match this.Contact with
        | Email e -> e
        | Phone nr -> nr
        | EmailAndPhone (e, nr)-> e + " (" + nr + ")"

    // override default ToString from obj
    override this.ToString () =
        sprintf """
           %s%s %s %A
        """ this.FirstName (
            match middleName with
            | Some x -> " x"
            | None -> ""
            ) this.LastName this.Contact

// type/class method extension
// (see https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/extension-methods)
type MyContact with
    member this.FullName =
        if this.MiddleName = "" then
            sprintf "%s %s %s" this.FirstName this.MiddleName this.LastName
        else
            sprintf "%s %s" this.FirstName this.LastName

let prettyPrint =
    function
    | Email e -> string e
    | Phone nr -> string nr
    | EmailAndPhone (e, nr)-> sprintf "e-mail: <%s>, phone: %s" e nr

// inheritance with ovveride of abstract method (virtual dispatch)
type PrettyContact
    ( firstName : FirstName,
      lastName : LastName,
      contact : ContactMethod) =
    inherit MyContact(firstName, lastName, contact)

    override this.ShowContact () =
        prettyPrint this.Contact

// interface definititon. note the "missing" () -> no constructor
type IContactInfo =
    abstract ShowContact : unit -> string

// ordinary record/union types can have members and implement interfaces
type OtherContact =
    {
        contact : ContactMethod
    }
    // interface impementation
    interface IContactInfo with
        member this.ShowContact () =
            prettyPrint this.contact

// implementation of an interface using a _object expression_
let prettyContact c =
    { new IContactInfo
      with member this.ShowContact () = prettyPrint c
    }

// empty type with overloaded members A.
type Overloaded =
    static member A (x: int) = x
    static member A (x: string) = x

    // member B is generic with a type _constraint_
    static member B<'T when 'T :> IContactInfo> (x: 'T) = x.ShowContact ()

[<EntryPoint>]
let main argv =
    let x = MyContact()
    printfn "%A" x
    printfn "%s" x.FirstName
    let y = { contact = EmailAndPhone ("hello@world.io", "123") }
    let y' = y :> IContactInfo
    printfn "%s" <| y'.ShowContact ()
    printfn "%s" <| Overloaded.B y

    0 // return an integer exit code

