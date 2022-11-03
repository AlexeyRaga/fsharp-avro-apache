module Tests

open System
open Avro.Generic
open Avro.Specific
open FSharp.Compiler.Syntax
open Fantomas.Core
open Xunit
open FsToolkit.ErrorHandling


type  Foo() =
    // A read-write property.
    let mutable myInternalValue = ""
    let mutable  foo = 5
    member this.MyReadWriteProperty
        with get () = myInternalValue
        and set (value) = myInternalValue <- value

let code = """

type Foo =
    {a : int}
    interface IEquatable<Foo> with

"""



[<Fact>]
let ``My test`` () =
   let input, _ = CodeFormatter.ParseAsync(false, code) |> Async.RunSynchronously |> Array.head

   match input with
    ParsedInput.ImplFile file ->
        printfn "%A" file

