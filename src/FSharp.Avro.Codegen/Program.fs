// For more information see https://aka.ms/fsharp-console-apps

open FSharp.Avro.Codegen
open FSharp.Compiler.Syntax
open Fantomas
open Myriad.Core
open Myriad.Core.Ast

[<EntryPoint>]
let main args =

    printfn "Hello from F#"

    let ctx = GeneratorContext.Create(Some "avro", (fun _ -> Seq.empty), "/Users/alexey/src/test/FSharp.Avro/corpus/simple.avsc", None, dict[])
    let gen = AvroGenerator() :> IMyriadGenerator
    let res = gen.Generate(ctx)

    match gen.Generate(ctx) with
    | Output.Ast ast ->
        let parsed = ParsedInput.ImplFile(ParsedImplFileInput.CreateFs("/tmp/out.fs1", modules = ast))
        let final = CodeFormatter.FormatASTAsync(parsed, "/tmp/out.fs", [], None,  FormatConfig.FormatConfig.Default) |> Async.RunSynchronously
        printfn "%A" final
    0