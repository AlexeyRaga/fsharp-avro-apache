module Tests

open System
open Avro.Generic
open Avro.Specific
open FSharp.Compiler.Syntax
open Fantomas.Core
open Test.AvroMsg
open Xunit
open FsToolkit.ErrorHandling

let code = """

type A() =
    static member internal FromInt(value: int) = ()


"""


[<Fact>]
let ``My test`` () =
   let input, _ = CodeFormatter.ParseAsync(false, code) |> Async.RunSynchronously |> Array.head

   match input with
    ParsedInput.ImplFile file ->
        printfn "%A" file

[<Fact>]
let ``Test`` () =
    let xs = Map.ofSeq ["a", 1; "b", 2]
    ()
