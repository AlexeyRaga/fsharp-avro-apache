open System
open BenchmarkDotNet.Running
open FSharp.Avro.Bench

[<EntryPoint>]
let main argv =
    BenchmarkRunner.Run<Benchmarks>() |> ignore
    0 // return an integer exit code
