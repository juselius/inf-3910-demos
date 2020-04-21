module Api

open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2
open Giraffe
open Shared

let handleInit next (ctx : HttpContext) =
    task {
        let counter = { Value = 42 }
        return! json counter next ctx
    }

let handleReadPeople next (ctx : HttpContext) =
    task {
        match Db.readPeople () with
        | Ok people -> return! json people next ctx
        | Error err -> return! RequestErrors.BAD_REQUEST (text err) next ctx
    }

let handleReadPerson
    ((firstName, lastName) : string * string)
    next (ctx : HttpContext) =
    let person = Db.readPerson firstName lastName
    task {
        try
            return! json person next ctx
        with exn ->
            return! RequestErrors.BAD_REQUEST (text exn.Message) next ctx
    }

let handleAddPerson next (ctx : HttpContext) =
    task {
        try
            let! data = ctx.BindJsonAsync<Person> ()
            let pId = Db.createPerson data
            return! json pId next ctx
        with exn ->
            return! RequestErrors.BAD_REQUEST (text exn.Message) next ctx
    }

let handleUpdatePerson next (ctx : HttpContext) =
    task {
        try
            let! pId, person = ctx.BindJsonAsync<int * Person> ()
            let r = Db.updatePerson pId person
            return! json r next ctx
        with exn ->
            return! RequestErrors.BAD_REQUEST (text exn.Message) next ctx
    }

let handleDeletePerson next (ctx : HttpContext) =
    task {
        try
            let! pId = ctx.BindJsonAsync<int> ()
            let r = Db.deletePerson pId
            return! json r next ctx
        with exn ->
            return! RequestErrors.BAD_REQUEST (text exn.Message) next ctx
    }
