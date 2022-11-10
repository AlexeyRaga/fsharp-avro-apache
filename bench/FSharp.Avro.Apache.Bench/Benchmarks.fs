﻿namespace FSharp.Avro.Bench

open System
open Avro.Specific
open BenchmarkDotNet
open BenchmarkDotNet.Attributes

type Benchmarks () =

    let CSPerson = Activator.CreateInstance<CSharp.AvroMsg.Person>()
    let CSMessage = Activator.CreateInstance<CSharp.AvroMsg.TestMessage>()

    let FSPerson = Activator.CreateInstance<Test.AvroMsg.Person>()
    let FSMessage = Activator.CreateInstance<Test.AvroMsg.TestMessage>()

    member private this.FillInMessage md5 (person : ISpecificRecord) (message : ISpecificRecord) =
        person.Put(0, this.stringValue)
        person.Put(1, this.intValue)

        message.Put(0, Guid.NewGuid())
        message.Put(1, this.intValue)
        message.Put(2, [| this.stringValue |])
        message.Put(3, this.intValue)
        message.Put(4, this.stringValue)
        message.Put(5, "choice")
        message.Put(6, 11)
        message.Put(7, dict ["fst", this.boolValue; this.stringValue, this.boolValue])
        message.Put(8, md5)
        message.Put(9, this.suitValue)
        message.Put(10, person)
        message.Put(11, person)
        message.Put(12, this.stringValue)


    // [<Params(0uy, 73uy, 13uy, 255uy)>]
    member val byteValue = 73uy with get, set

    // [<Params(Int32.MinValue, 0, 42, 111, 987654, Int32.MaxValue)>]
    member val intValue = 987654 with get, set

    [<ParamsAllValues>]
    member val boolValue = false with get, set

    // [<Params("", "Some actually longish text, but not really")>]
    member val stringValue = "Lorem ipsus" with get, set

    // [<Params(0,1,2,3)>]
    member val suitValue = 2 with get, set

    [<Benchmark(Description = "C# Classes", Baseline = true)>]
    member this.AvroRecordClass () =
        let md5 = CSharp.AvroMsg.MD5(Value = Array.replicate 16 this.byteValue)
        this.FillInMessage md5 CSPerson CSMessage

    [<Benchmark(Description = "F# Records")>]
    member this.AvroRecord () =
        let (Ok md5) = Test.AvroMsg.MD5.Create(Array.replicate 16 this.byteValue)
        this.FillInMessage md5 FSPerson FSMessage


