module Testing.UI

open OpenQA.Selenium.Chrome
open canopy.types
open canopy.runner.classic
open canopy.configuration
open canopy.classic
open System
open System.IO
open System.Diagnostics

type CanopyMode =
    | Browser = 1
    | Headless = 2

let topDir = Path.GetFullPath <| __SOURCE_DIRECTORY__ + "/.."
let deployDir = Path.GetFullPath <| topDir + "/deploy"
let contentRoot = Path.GetFullPath <| deployDir + "/public"
let homeDir =
    if Environment.OSVersion.Platform = PlatformID.Unix || Environment.OSVersion.Platform = PlatformID.MacOSX then
        Environment.GetEnvironmentVariable("HOME")
    else
        Environment.ExpandEnvironmentVariables("%HOMEDRIVE%%HOMEPATH%");

let buildProject () =
    let p = new Process()
    p.StartInfo.FileName <- "/usr/bin/env"
    p.StartInfo.Arguments <- "fake build -t debug"
    p.StartInfo.WorkingDirectory <- topDir
    p.Start () |> ignore
    p.WaitForExit ()

let runServer () =
    let p = new Process()
    System.Environment.SetEnvironmentVariable ("CONTENT_ROOT", contentRoot)
    p.StartInfo.FileName <- "/usr/bin/env"
    p.StartInfo.Arguments <- "dotnet Server.dll"
    p.StartInfo.WorkingDirectory <- deployDir
    p.Start () |> ignore
    Async.Sleep 2000 |> Async.RunSynchronously
    p

let setChromeDir () =
    let nixDir = Path.Join [| homeDir; ".nix-profile/bin" |]
    if Directory.Exists nixDir then
        chromeDir <- nixDir
    else
        ()

let testUI (mode : CanopyMode) =
    setChromeDir ()
    buildProject ()
    let server = runServer ()

    match mode with
    | CanopyMode.Headless -> start BrowserStartMode.ChromeHeadless
    | _ -> start BrowserStartMode.Chrome

    //this is how you define a test
    "taking canopy for a spin" &&& fun _ ->
        url "http://localhost:8085"
        ".title.is-3" *= "Hello People!"
        "#First" << "Foo"
        "#Last" << "Bar"
        "#Age" << "10"
        "#Height" << "155"
        click "#Save"
        "#First" << "Reodor"
        "#Last" << "Felgen"
        "#Age" << "73"
        "#Height" << "165"
        click "#Save"
        "#First" << "Frøydis"
        "#Last" << "Frukthage"
        "#Age" << "43"
        "#Height" << "171"
        click "#Save"
        reload ()
        click "#Load"
    run ()
    if mode = CanopyMode.Browser then
        Async.Sleep 5000 |> Async.RunSynchronously
    else
        ()
    quit ()
    server.Kill ()
    0
