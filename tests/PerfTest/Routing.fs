namespace PerfTest

open BenchmarkDotNet.Attributes
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection

module RoutefImpl =
    open System
    open FSharp.Reflection
    open Microsoft.AspNetCore.Http
    open Microsoft.AspNetCore.Routing
    open Oxpecker

    let routefTupledDirect (path : PrintfFormat<_,_,_,_, 'T>) (routeHandler : 'T -> EndpointHandler) : Endpoint =
        let template, mappings = RouteTemplateBuilder.convertToRouteTemplateOld path.Value

        let requestDelegate = fun (ctx: HttpContext) ->
            let routeData = ctx.GetRouteData()
            match box routeHandler with
            | :? (string -> EndpointHandler) as typedHandler ->
                typedHandler (unbox routeData.Values[fst mappings[0]]) ctx

            | :? (int * string -> EndpointHandler) as typedHandler ->
                typedHandler
                    ((int (string routeData.Values[fst mappings[0]])),
                    (unbox routeData.Values[fst mappings[1]]))
                    ctx

            | :? (int * string * string * Guid * string -> EndpointHandler) as typedHadler ->
                typedHadler
                    (int (string routeData.Values[fst mappings[0]]),
                     unbox routeData.Values[fst mappings[1]],
                     unbox routeData.Values[fst mappings[2]],
                     Guid (string routeData.Values[fst mappings[3]]),
                     unbox routeData.Values[fst mappings[4]])
                    ctx

            | _ -> failwithf "Unsupported handler %A" typeof<'T -> Endpoint>

        SimpleEndpoint(Any, template, requestDelegate, id)

    let routefCurriedDirect (path: PrintfFormat<'T, unit, unit, EndpointHandler>) (handler: 'T) : Endpoint =
        let template, mappings = RouteTemplateBuilder.convertToRouteTemplateOld path.Value
        
        let requestDelegate = fun (ctx: HttpContext) ->
            let routeData = ctx.GetRouteData()
            match box handler with
            | :? (string -> EndpointHandler) as typedHandler ->
                typedHandler (unbox routeData.Values[fst mappings[0]]) ctx

            | :? (int -> string -> EndpointHandler) as typedHandler ->
                typedHandler
                    (int (unbox<string> routeData.Values[fst mappings[0]]))
                    (unbox<string> routeData.Values[fst mappings[1]])
                    ctx

            | :? (int -> string -> string -> Guid -> string -> EndpointHandler) as typedHandler ->
                typedHandler
                    (int (unbox<string> routeData.Values[fst mappings[0]]))
                    (unbox routeData.Values[fst mappings[1]])
                    (unbox routeData.Values[fst mappings[2]])
                    (Guid (unbox<string> routeData.Values[fst mappings[3]]))
                    (unbox routeData.Values[fst mappings[4]])
                    ctx

            | _ -> failwithf "Unsupported handler %A" typeof<'T -> Endpoint>

        SimpleEndpoint(Any, template, requestDelegate, id)

    let private convertToTuple (mappings : (string * char) list) (ctx: HttpContext) =
        let routeData = ctx.GetRouteData()
        let endpoint = ctx.GetEndpoint() :?> RouteEndpoint
        let values =
            mappings
            |> List.map (fun (placeholderName, formatChar) ->
                let routeValue = routeData.Values.[placeholderName]
                let modifier =
                    let gotten, policyReference = endpoint.RoutePattern.ParameterPolicies.TryGetValue(placeholderName) 
                    if gotten && policyReference[0].Content = "guid" then
                        Some "guid"
                    else
                        None
                match RouteTemplateBuilder.tryGetParser formatChar modifier with
                | ValueSome parseFn -> parseFn (routeValue.ToString())
                | ValueNone         -> routeValue)
            |> List.toArray

        let result =
            match values.Length with
            | 1 -> values.[0]
            | _ ->
                let types =
                    values
                    |> Array.map (fun v -> v.GetType())
                let tupleType = FSharpType.MakeTupleType types
                FSharpValue.MakeTuple(values, tupleType)
        result

    let routefTupledReflection (path : PrintfFormat<_,_,_,_, 'T>) (routeHandler : 'T -> EndpointHandler) : Endpoint =

        let template, mappings = RouteTemplateBuilder.convertToRouteTemplateOld path.Value

        let requestDelegate = fun ctx -> routeHandler (downcast convertToTuple mappings ctx) ctx

        SimpleEndpoint (Any, template, requestDelegate, id)

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
            GET [ routefOld "/user/{%i}/{%s}" <| fun id name -> text $"User {id} {name} received" ]
            GET [ routef "/typeshape/{%i}/{%s}" <| fun id name -> text $"User {id} {name} received" ]
            GET [ RoutefImpl.routefTupledReflection "/tupled/{%i}/{%s}" <| fun (id, name) ctx -> text $"User {id} {name} received" ctx  ]
            GET [ RoutefImpl.routefTupledDirect "/direct/tupled/{%i}/{%s}" <| fun (id, name) ctx -> text $"User {id} {name} received" ctx  ]
            GET [ RoutefImpl.routefCurriedDirect "/direct/curried/{%i}/{%s}" <| fun id name ctx -> text $"User {id} {name} received" ctx  ]
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
                "/api4"
                (choose [
                    GET >=> route "/users" >=> text "Users received"
                    GET >=> routef "/user/%i/%s/%s/%O:guid/%s" (fun (id, fstname, lstname, token: System.Guid, s1) next ctx -> text $"User {id} {fstname} {lstname} {token} {s1} received" next ctx)
                    GET >=> routef "/user/%i/%s" (fun (id, name) next -> text $"User {id} {name} received" next)
                    GET >=> route "/json" >=> json {| Name = "User" |}
                ])
            subRoute
                "/api"
                (choose [
                    GET >=> route "/users" >=> text "Users received"
                    GET >=> routef "/user/%i/%s" (fun (id, name) next ctx -> text $"User {id} {name} received" next ctx)
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
    member this.GetOxpeckerRoute() =
        oxpeckerClient.GetAsync("/api/users")

    [<Benchmark>]
    member this.GetOxpeckerRoutef() =
        oxpeckerClient.GetAsync("/api/user/1/don")

    [<Benchmark>]
    member this.GetOxpeckerRoutefTupledDirect() =
        oxpeckerClient.GetAsync("/api/direct/tupled/1/done")

    [<Benchmark(Baseline = true)>]
    member this.GetOxpeckerRoutefCurriedDirect() =
        oxpeckerClient.GetAsync("/api/direct/curried/1/don")

    [<Benchmark>]
    member this.GetOxpeckerRoutefTupledReflection() =
        oxpeckerClient.GetAsync("/api/tupled/1/don")

    [<Benchmark>]
    member this.GetOxpeckerRoutefCurriedTypeshape() = 
        oxpeckerClient.GetAsync("/api/typeshape/1/don")
    
    [<Benchmark>]
    member this.GetGiraffeRoutef() =
        giraffeClient.GetAsync("/api/user/1/don")
