// For more information see https://aka.ms/fsharp-console-apps

open System
open FSharp.Avro.Codegen
open Argu

type Output = File of string | Console
module Output =
    let parse s =
        match s with
        | "-" -> Console
        | path -> File path

    let write (txt : string) out =
        match out with
        | Console -> printfn $"%s{txt}"
        | File path ->
            System.IO.Directory.CreateDirectory(System.IO.Path.GetDirectoryName path) |> ignore
            System.IO.File.WriteAllText(path, txt)

type CLIArguments =
    | [<Unique>] Schema_File of file:string
    | [<Unique>] Output of file:string
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Schema_File _ -> "Path to .avsc file"
            | Output _ -> "Output location"

type Options =
    { SchemaFile : string
      OutputFile: Output }

[<Literal>]
let sourcePath = __SOURCE_DIRECTORY__

let parseOptions args =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "Avro")
    let results = parser.Parse(args)
    { SchemaFile = results.GetResult(<@ CLIArguments.Schema_File @>, defaultValue = $"{sourcePath}/../../corpus/simple.avsc")
      OutputFile = results.PostProcessResult(<@ CLIArguments.Output @>, Output.parse)
    }


[<EntryPoint>]
let main args =
    let options = parseOptions args
    let res = AvroGenerator.GenerateAsync options.SchemaFile |> Async.RunSynchronously
    options.OutputFile |> Output.write res
    0
