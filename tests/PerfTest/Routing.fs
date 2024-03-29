﻿namespace PerfTest

open BenchmarkDotNet.Attributes
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection

module OxpeckerRouting =
    open Oxpecker

    let endpoints = [
        subRoute "/api1" [
            GET [ route "/users" <| text "Users received" ]
            GET [ routef "/user/{%i}/{%s}" <| fun id name -> text $"User {id} {name} received" ]
            GET [ route "/json" <| json {| Name = "User" |} ]
        ]
        subRoute "/api2" [
            GET [ route "/users" <| text "Users received" ]
            GET [ routef "/user/{%i}/{%s}" <| fun id name -> text $"User {id} {name} received" ]
            GET [ route "/json" <| json {| Name = "User" |} ]
        ]
        subRoute "/api3" [
            GET [ route "/users" <| text "Users received" ]
            GET [ routef "/user/{%i}/{%s}" <| fun id name -> text $"User {id} {name} received" ]
            GET [ route "/json" <| json {| Name = "User" |} ]
        ]
        subRoute "/api" [
            GET [ route "/users" <| text "Users received" ]
            GET [ routef "/user/{%i}/{%s}" <| fun id name -> text $"User {id} {name} received" ]
            GET [ routefOld "/user/{%i}/{%s}/{%s}/{%O:guid}/{%s}" <| fun id fstname lstname  (token: System.Guid) s1 ctx -> text $"User {id} {fstname} {lstname} {token} {s1} received" ctx ]
            GET [ routef "/typeshape/{%i}/{%s}/{%s}/{%O:guid}/{%s}" <| fun id fstname lstname (token: System.Guid) s1 ctx -> text $"User {id} {fstname} {lstname} {token} {s1} received" ctx ]
            GET [ routefBaseline "/baseline/{%i}/{%s}/{%s}/{%O:guid}/{%s}" <| fun id fstname lstname (token: System.Guid) s1 ctx -> text $"User {id} {fstname} {lstname} {token} {s1} received" ctx  ]
            GET [ route "/json" <| json {| Name = "User" |} ]
        ]
    ]

    let webApp () =
        let builder =
            WebHostBuilder()
                .UseKestrel()
                .Configure(fun app -> app.UseRouting().UseOxpecker(endpoints) |> ignore)
                .ConfigureServices(fun services -> services.AddRouting().AddOxpecker() |> ignore)
        new TestServer(builder)

module GiraffeRouting =
    open Giraffe

    let endpoints =
        choose [
            subRoute
                "/api1"
                (choose [
                    GET >=> route "/users" >=> text "Users received"
                    GET
                    >=> routef "/user/%i/%s" (fun (id, name) next -> text $"User {id} {name} received" next)
                    GET >=> route "/json" >=> json {| Name = "User" |}
                ])
            subRoute
                "/api2"
                (choose [
                    GET >=> route "/users" >=> text "Users received"
                    GET
                    >=> routef "/user/%i/%s" (fun (id, name) next -> text $"User {id} {name} received" next)
                    GET >=> route "/json" >=> json {| Name = "User" |}
                ])
            subRoute
                "/api3"
                (choose [
                    GET >=> route "/users" >=> text "Users received"
                    GET
                    >=> routef "/user/%i/%s" (fun (id, name) next -> text $"User {id} {name} received" next)
                    GET >=> route "/json" >=> json {| Name = "User" |}
                ])
            subRoute
                "/api"
                (choose [
                    GET >=> route "/users" >=> text "Users received"
                    GET >=> routef "/user/%i/%s/%s/%O:guid/%s" (fun (id, fstname, lstname, token: System.Guid, s1) next ctx -> text $"User {id} {fstname} {lstname} {token} {s1} received" next ctx)
                    GET >=> routef "/user/%i/%s" (fun (id, name) next -> text $"User {id} {name} received" next)
                    GET >=> route "/json" >=> json {| Name = "User" |}
                ])
        ]

    let webApp () =
        let builder =
            WebHostBuilder()
                .UseKestrel()
                .Configure(fun app -> app.UseGiraffe(endpoints))
                .ConfigureServices(fun services -> services.AddGiraffe() |> ignore)
        new TestServer(builder)

[<MemoryDiagnoser>]
type Routing() =

    // BenchmarkDotNet v0.13.12, Windows 11 (10.0.22631.3296/23H2/2023Update/SunValley3)
    // AMD Ryzen 5 5600H with Radeon Graphics, 1 CPU, 12 logical and 6 physical cores
    // .NET SDK 8.0.200
    //   [Host]     : .NET 8.0.3 (8.0.324.11423), X64 RyuJIT AVX2 DEBUG
    //   DefaultJob : .NET 8.0.3 (8.0.324.11423), X64 RyuJIT AVX2
    //
    //
    // | Method            | Mean      | Error     | StdDev    | Gen0   | Allocated |
    // |------------------ |----------:|----------:|----------:|-------:|----------:|
    // | GetOxpeckerRoute  |  7.886 us | 0.1379 us | 0.1475 us | 0.9766 |   8.01 KB |
    // | GetOxpeckerRoutef | 12.885 us | 0.3035 us | 0.8561 us | 1.0986 |    9.8 KB |
    // | GetGiraffeRoute   |  8.763 us | 0.1748 us | 0.4085 us | 1.0986 |    9.4 KB |
    // | GetGiraffeRoutef  | 25.607 us | 0.4973 us | 0.4885 us | 1.4648 |  13.38 KB |
    let oxpeckerServer = OxpeckerRouting.webApp()
    let giraffeServer = GiraffeRouting.webApp()
    let oxpeckerClient = oxpeckerServer.CreateClient()
    let giraffeClient = giraffeServer.CreateClient()

    [<Benchmark>]
    member this.GetOxpeckerRoute() = oxpeckerClient.GetAsync("/api/users")

    [<Benchmark>]
    member this.GetOxpeckerRoutef() =
        oxpeckerClient.GetAsync("/api/user/1/don")
    member this.GetOxpeckerRoutefDirect() = oxpeckerClient.GetAsync("/api/baseline/1/john/doe/be4fd44d-fcca-44db-bf85-d392f81532d0/a")

    [<Benchmark>]
    member this.GetOxpeckerRoutefNew() = oxpeckerClient.GetAsync("/api/typeshape/1/john/doe/be4fd44d-fcca-44db-bf85-d392f81532d0/a")

    [<Benchmark>]
    member this.GetGiraffeRoutef() =
        giraffeClient.GetAsync("/api/user/1/don")

    [<Benchmark>]
    member this.GetOxpeckerRoutefOld() = oxpeckerClient.GetAsync("/api/user/1/john/doe/be4fd44d-fcca-44db-bf85-d392f81532d0/a")

    [<Benchmark>]
    member this.GetOxpeckerJson() = oxpeckerClient.GetAsync("/api/json")

    [<Benchmark>]
    member this.GetGiraffeRoute() = giraffeClient.GetAsync("/api/users")
