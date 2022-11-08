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

let a = 43 <> 1


"""

[<Fact>]
let ``My test`` () =
   let input, _ = CodeFormatter.ParseAsync(false, code) |> Async.RunSynchronously |> Array.head

   match input with
    ParsedInput.ImplFile file ->
        printfn "%A" file
