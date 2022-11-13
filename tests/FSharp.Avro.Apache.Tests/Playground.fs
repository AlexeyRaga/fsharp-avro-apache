module Playground

open FSharp.Compiler.Syntax
open Fantomas.Core
open Xunit
open FsToolkit.ErrorHandling
open Xunit.Abstractions

let code =
    """

match foo with
// return a
| 1 -> "this is a"
// return b
| 2 -> "this is b"



"""

type ``AST Interrogation``(out : ITestOutputHelper) =

    [<Fact>]
    member this.``Print AST`` () =
        let input, _ = CodeFormatter.ParseAsync(false, code) |> Async.RunSynchronously |> Array.head

        match input with
        | ParsedInput.ImplFile file -> out.WriteLine $"%A{file}"
        | _ -> ()
