namespace FSharp.Avro.Bench

open System
open Avro.Specific
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open FSharp.Avro.Bench.Setters

type SettersBench() =
    let csPerson = Activator.CreateInstance<CSharp.AvroMsg.Person>()
    let classPerson = Activator.CreateInstance<ClassPerson>()
    let recordPerson = Activator.CreateInstance<RecordPerson>()
    let optimisedPerson = Activator.CreateInstance<OptimisedPerson>()

    member this.FillInPerson(person : ISpecificRecord) =
        person.Put(0, this.stringValue)
        person.Put(1, this.intValue)


    // [<Params(Int32.MinValue, 0, 42, 111, 987654, Int32.MaxValue)>]
    member val intValue = 987654 with get, set

    // [<Params("", "Some actually longish text, but not really")>]
    member val stringValue = "Lorem ipsus" with get, set

    [<Benchmark(Description = "C# Class", Baseline = true)>]
    member this.CSharpClass() =
        this.FillInPerson csPerson

    [<Benchmark(Description = "F# Class")>]
    member this.FSharpClass() =
        this.FillInPerson(classPerson)

    [<Benchmark(Description = "F# Record")>]
    member this.FSharpRecord() =
        this.FillInPerson(recordPerson)

    [<Benchmark(Description = "F# Record (Optimised)")>]
    member this.FSharpRecordOptimised() =
        this.FillInPerson(optimisedPerson)
