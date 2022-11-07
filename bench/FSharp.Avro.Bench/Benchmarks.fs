namespace FSharp.Avro.Bench

open System
open Avro.Specific
open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open FSharp.Avro.Bench.Corpus

type Benchmarks () =

    member private this.FillInRecord md5 (record : ISpecificRecord) =
        record.Put(0, Guid.NewGuid())
        record.Put(1, this.intValue)
        record.Put(2, [| this.stringValue |])
        record.Put(3, this.intValue)
        record.Put(4, this.stringValue)
        record.Put(5, "choice")
        record.Put(6, 11)
        record.Put(7, dict ["fst", this.boolValue; this.stringValue, this.boolValue])
        record.Put(8, md5)
        record.Put(9, this.suitValue)


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

    [<Benchmark(Description = "Avro record as F# record")>]
    member this.AvroRecord () =
        let (Ok md5) = AvroRecord.MD5.Create(Array.replicate 16 this.byteValue)
        let record = Activator.CreateInstance(typeof<AvroRecord.TestMessage>) :?> ISpecificRecord
        this.FillInRecord md5 record


    [<Benchmark(Description = "Avro record as .NET class", Baseline = true)>]
    member this.AvroRecordClass () =
        let (Ok md5) = AvroRecordClass.MD5.Create(Array.replicate 16 this.byteValue)
        let record = Activator.CreateInstance(typeof<AvroRecordClass.TestMessage>) :?> ISpecificRecord
        this.FillInRecord md5 record
