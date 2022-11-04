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

type Foo() =
    do base.Value <- 7

"""



[<Fact>]
let ``My test`` () =
   let input, _ = CodeFormatter.ParseAsync(false, code) |> Async.RunSynchronously |> Array.head

   // let (Ok md5) = MD5.Create([||])
   // let a = TestMessage(Guid.Empty, 2, Some 1, "foo", Choice1Of3 "bar", None, Map.empty, md5, Suit.CLUBS)


   match input with
    ParsedInput.ImplFile file ->
        printfn "%A" file

