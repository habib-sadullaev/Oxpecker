namespace PerfTest

open System
open System.Collections.Generic
open BenchmarkDotNet.Attributes
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Primitives
open Oxpecker
open System.Globalization

type Sex =
    | Male
    | Female

[<CLIMutable>]
type Child = { Name: string | null; Age: int }

[<CLIMutable>]
type Model = {
    Id: Guid
    FirstName: string | null
    MiddleName: string option
    LastName: string | null
    Sex: Sex
    BirthDate: DateTime
    Nicknames: string list option
    Children: Child[]
}

type AnonymousType1 = {|
Value:
    {|
        Value: {| Value: {| Id: int; Name: string |} |}
    |}
|}

type AnonymousType2 = {|
Values:
    {|
        Value:
            {|
                Values:
                    {|
                        Value: {| Id: int; Name: string | null |}
                    |} array
            |}
    |} array
|}

type ComplexModel = {
    Model: Model
    Anon1: AnonymousType1
    Anon2: AnonymousType2
}

[<MemoryDiagnoser>]
type ModelBinding() =
    static let modelData =
        [
            "Id", StringValues(Guid.NewGuid().ToString())
            "FirstName", StringValues "Susan"
            "MiddleName", StringValues "Elisabeth"
            "LastName", StringValues "Doe"
            "Sex", StringValues "Female"
            "BirthDate", StringValues "1986-12-29"
            "Nicknames", StringValues [| "Susi"; "Eli"; "Liz" |]
            "Children[0].Name", StringValues "Hamed"
            "Children[0].Age", StringValues "32"
            "Children[1].Name", StringValues "Ali"
            "Children[1].Age", StringValues "22"
            "Children[2].Name", StringValues "Gholi"
            "Children[2].Age", StringValues "44"
        ]
        |> List.map KeyValuePair.Create
        |> Dictionary

    static let modelData1 =
        [
            "Value.Value.Value.Name", StringValues "foo"
            "Value.Value.Value.Id", StringValues "111"
        ]
        |> List.map KeyValuePair
        |> Dictionary

    static let modelData2 =
        [
            "Values[2].Value.Values[2].Value.Name", StringValues "foo"
            "Values[2].Value.Values[0].Value.Id", StringValues "111"
            "Values[1].Value.Values[0].Value.Name", StringValues "bar"
            "Values[2].Value.Values[2].Value.Id", StringValues "222"
        ]
        |> List.map KeyValuePair
        |> Dictionary

    let firstValue (rawValues: StringValues) =
        if rawValues.Count > 0 then rawValues[0] else null

    let parseModel (culture: CultureInfo) (data: FormCollection) = {
        Id = Guid.Parse(data["Id"] |> firstValue |> nonNull, culture)
        FirstName = data["FirstName"] |> firstValue
        MiddleName = data["MiddleName"] |> firstValue |> Option.ofObj
        LastName = data["LastName"] |> firstValue
        Sex =
            match data["Sex"] |> firstValue with
            | "Female" -> Female
            | "Male" -> Male
            | value -> failwith $"Value '{value}' could not be parsed to {typeof<Sex>}"
        BirthDate =
            let dt = data["BirthDate"] |> firstValue |> nonNull
            DateTime.Parse(dt, culture)
        Nicknames = Some [ yield! data["Nicknames"] |> Seq.cast ]
        Children = [|
            {
                Name = data["Children[0].Name"] |> firstValue
                Age = Int32.Parse(data["Children[0].Age"] |> firstValue |> nonNull, culture)
            }
            {
                Name = data["Children[1].Name"] |> firstValue
                Age = Int32.Parse(data["Children[1].Age"] |> firstValue |> nonNull, culture)
            }
            {
                Name = data["Children[2].Name"] |> firstValue
                Age = Int32.Parse(data["Children[2].Age"] |> firstValue |> nonNull, culture)
            }
        |]
    }

    // BenchmarkDotNet v0.14.0, Windows 11 (10.0.26100.3775)
    // AMD Ryzen 5 5600H with Radeon Graphics, 1 CPU, 12 logical and 6 physical cores
    // .NET SDK 9.0.201
    //   [Host]     : .NET 9.0.3 (9.0.325.11113), X64 RyuJIT AVX2 DEBUG
    //   DefaultJob : .NET 9.0.3 (9.0.325.11113), X64 RyuJIT AVX2
    //
    //
    // | Method              | Mean         | Error       | StdDev      | Ratio  | RatioSD | Gen0   | Allocated | Alloc Ratio |
    // |-------------------- |-------------:|------------:|------------:|-------:|--------:|-------:|----------:|------------:|
    // | DirectModelBinder   |     477.9 ns |     3.04 ns |     2.84 ns |   1.00 |    0.01 | 0.0801 |     672 B |        1.00 |
    // | OxpeckerModelBinder |   1,466.3 ns |    17.57 ns |    16.43 ns |   3.07 |    0.04 | 0.0725 |     608 B |        0.90 |
    // | GiraffeModelBinder  | 121,709.6 ns | 1,164.21 ns | 1,032.04 ns | 254.70 |    2.55 | 7.3242 |   62545 B |       93.07 |


    static let binder_v1 = ModelBinder() :> IModelBinder
    static let binder_v2 = v2.ModelBinder() :> v2.IModelBinder
    static let formCollection = FormCollection modelData
    static let formCollection1 = FormCollection modelData1
    static let formCollection2 = FormCollection modelData2

    //[<Benchmark(Baseline = true)>]
    //member _.DirectModelBinder() =
    //    parseModel CultureInfo.InvariantCulture formCollection

    [<Benchmark>]
    member _.OxpeckerModelBinder_v2_1() =
        let mutable res =
            {
                Model = binder_v2.Bind<Model> formCollection
                Anon1 = binder_v2.Bind<AnonymousType1> formCollection1
                Anon2 = binder_v2.Bind<AnonymousType2> formCollection2
            }
        for i in 1..200 do
            res <- {
                Model = binder_v2.Bind<Model> formCollection
                Anon1 = binder_v2.Bind<AnonymousType1> formCollection1
                Anon2 = binder_v2.Bind<AnonymousType2> formCollection2
            }

    [<Benchmark(Baseline = true)>]
    member _.OxpeckerModelBinder_v1_1() =
        let mutable res =
            {
                Model = binder_v1.Bind<Model> formCollection
                Anon1 = binder_v1.Bind<AnonymousType1> formCollection1
                Anon2 = binder_v1.Bind<AnonymousType2> formCollection2
            }

        for i in 1..200 do
            res <- {
                Model = binder_v1.Bind<Model> formCollection
                Anon1 = binder_v1.Bind<AnonymousType1> formCollection1
                Anon2 = binder_v1.Bind<AnonymousType2> formCollection2
            }

    //[<Benchmark>]
    //member _.OxpeckerModelBinder_v2_2() = binder_v2.Bind<Model> formCollection

    //[<Benchmark>]
    //member _.OxpeckerModelBinder_v1_2() = binder_v1.Bind<Model> formCollection

    //[<Benchmark>]
    //member _.OxpeckerModelBinder_v1_3() = binder_v1.Bind<Model> formCollection

    //[<Benchmark>]
    //member _.OxpeckerModelBinder_v2_3() = binder_v2.Bind<Model> formCollection

    //[<Benchmark>]
    //member _.OxpeckerModelBinder_v2_4() = binder_v2.Bind<Model> formCollection

    //[<Benchmark>]
    //member _.OxpeckerModelBinder_v1_4() = binder_v1.Bind<Model> formCollection

    //[<Benchmark>]
    //member _.GiraffeModelBinder() =
    //    Giraffe.ModelParser.parse<Model> (Some CultureInfo.InvariantCulture) modelData
