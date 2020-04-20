open System
open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection

open FSharp.Control.Tasks.V2
open Thoth.Json.Net
open Giraffe
open Shared

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let dataFile =
    __SOURCE_DIRECTORY__ + "/people.json"

let handleInit next (ctx : HttpContext) =
    task {
        let counter = { Value = 42 }
        return! json counter next ctx
    }

let handleGetPeople next (ctx : HttpContext) =
    let data = System.IO.File.ReadAllText dataFile
    let decoder = Decode.Auto.generateDecoder<Person list> ()
    task {
        match Decode.fromString decoder data with
        | Ok people -> return! json people next ctx
        | Error err -> return! RequestErrors.BAD_REQUEST (text err) next ctx
    }

let handleAddPerson next (ctx : HttpContext) =
    let txt = System.IO.File.ReadAllText dataFile
    let decoder = Decode.Auto.generateDecoder<Person list> ()
    let people =
        match Decode.fromString decoder txt with
        | Ok p -> p
        | Error _ -> []
    task {
        try
            let! data = ctx.BindJsonAsync<Person> ()
            let p = Encode.Auto.toString (4, (data :: people))
            System.IO.File.WriteAllText (dataFile, p)
            return! json data next ctx
        with exn ->
            return! RequestErrors.BAD_REQUEST (text exn.Message) next ctx
    }

let webApp =
    choose [
        route "/api/init" >=> handleInit
        GET >=> route "/api/people" >=> handleGetPeople
        POST >=> route "/api/person" >=> handleAddPerson
    ]

let configureApp (app : IApplicationBuilder) =
    app.UseDefaultFiles()
       .UseStaticFiles()
       .UseGiraffe webApp

let jsonSerializer = Thoth.Json.Giraffe.ThothSerializer ()

let configureServices (services : IServiceCollection) =
    services.AddGiraffe() |> ignore
    services.AddSingleton<Serialization.Json.IJsonSerializer>(jsonSerializer) |> ignore

WebHost
    .CreateDefaultBuilder()
    .UseWebRoot(publicPath)
    .UseContentRoot(publicPath)
    .Configure(Action<IApplicationBuilder> configureApp)
    .ConfigureServices(configureServices)
    .UseUrls("http://0.0.0.0:" + port.ToString() + "/")
    .Build()
    .Run()
