open FSharp.Avro.Codegen
open Argu
open FSharp.Avro.Codegen.Generators

type Output =
    | File of string
    | Stdout

module Output =
    let parse s =
        match s with
        | "-" -> Stdout
        | path -> File path

    let write (txt : string) out =
        match out with
        | Stdout -> printfn $"%s{txt}"
        | File path ->
            System.IO.Directory.CreateDirectory(System.IO.Path.GetDirectoryName path) |> ignore
            System.IO.File.WriteAllText(path, txt)

type CLIArguments =
    | [<Unique>] Schema_File of file : string
    | [<Unique>] Output of file : string
    | [<Unique>] Record_Repr of repr : RecordRepresentation
    | Namespace of string
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Schema_File _ -> "Path to .avsc file"
            | Output _ -> "Output location"
            | Record_Repr _ -> "Record representation, 'class' or 'record'"
            | Namespace _ ->
                """Map an Avro schema namespace to a .NET namespace.
The format is "my.avro.namespace:my.csharp.namespace"
May be specified multiple times to map multiple namespaces."""

type Options =
    { SchemaFile : string
      OutputFile : Output
      Parameters : GenParams }

let parseOptions args =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "Avro")
    let results = parser.Parse(args)

    let parseNamespace (mapping : string) =
        match mapping.Split(':') with
        | [| k; v |] -> Some(k, v)
        | _ -> None

    let parameters =
        { RecordRepr = results.GetResult(<@ CLIArguments.Record_Repr @>, defaultValue = RecordRepresentation.Record)
          NamespaceMapping = results.PostProcessResults(<@ CLIArguments.Namespace @>, parseNamespace) |> Seq.choose id |> Map.ofSeq }

    { SchemaFile = results.GetResult(<@ CLIArguments.Schema_File @>)
      OutputFile = results.PostProcessResult(<@ CLIArguments.Output @>, Output.parse)
      Parameters = parameters }

[<EntryPoint>]
let main args =
    let options = parseOptions args
    let res = AvroGenerator.generateForFile options.Parameters options.SchemaFile |> Async.RunSynchronously
    options.OutputFile |> Output.write res
    0
