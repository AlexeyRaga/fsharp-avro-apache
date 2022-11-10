module Playground

open FSharp.Compiler.Syntax
open Fantomas.Core
open Xunit
open FsToolkit.ErrorHandling
open Xunit.Abstractions

let code =
    """

let x = "Hello, World"


"""

type ``AST Interrogation``(out : ITestOutputHelper) =

    [<Fact>]
    member this.``Print AST`` () =
        let input, _ = CodeFormatter.ParseAsync(false, code) |> Async.RunSynchronously |> Array.head

        match input with
        | ParsedInput.ImplFile file -> out.WriteLine $"%A{file}"
        | _ -> ()
