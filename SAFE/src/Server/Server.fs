open System
open System.IO

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection

open Giraffe
open Api

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let webApp : HttpHandler =
    choose [
        GET >=> route "/api/init" >=> handleInit
        GET >=> route "/api/people" >=> handleReadPeople
        GET >=> routef "/api/person/%s/%s" handleReadPerson
        route "/api/person"
            >=> choose [
                POST   >=> handleAddPerson
                PUT    >=> handleUpdatePerson
                DELETE >=> handleDeletePerson
            ]
    ]

let jsonSerializer = Thoth.Json.Giraffe.ThothSerializer ()

let configureServices (services : IServiceCollection) =
    services.AddGiraffe() |> ignore
    services.AddSingleton<Serialization.Json.IJsonSerializer>(jsonSerializer) |> ignore

let configureApp (app : IApplicationBuilder) =
    app.UseDefaultFiles()
       .UseStaticFiles()
       .UseGiraffe webApp

Db.tryMigrate ()

WebHost
    .CreateDefaultBuilder()
    .UseWebRoot(publicPath)
    .UseContentRoot(publicPath)
    .Configure(Action<IApplicationBuilder> configureApp)
    .ConfigureServices(configureServices)
    .UseUrls("http://0.0.0.0:" + port.ToString() + "/")
    .Build()
    .Run()
