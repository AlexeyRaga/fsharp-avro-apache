namespace FSharp.Avro.Codegen

open System.IO
open System.Text.RegularExpressions
open Avro
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Syntax
open Fantomas.Core
open Schema
open FSharp.Avro.Codegen.Generators

module AvroGenerator =
    let private generateType (parameters : GenParams) (schema : Schema) =
        match schema with
        | :? RecordSchema as s -> SpecificRecord.genSpecificRecord parameters s
        | :? EnumSchema as s -> SpecificEnum.genSpecificEnum parameters s
        | :? FixedSchema as s -> SpecificFixed.genSpecificFixed parameters s
        | s -> failwith $"Schema {s.Fullname} of type {s.Tag} is not expected to be generated"

    let generateForSchema (parameters : GenParams) (schema : Schema) =
        let ast =
            findAllSchemas schema
            |> Seq.map (generateType parameters)
            |> Seq.partitionByConsequentKey (fun x -> x.Namespace)
            |> Seq.map (fun (ns, xs) ->
                SynModuleOrNamespace.CreateNamespace(
                    Ident.CreateLong ns,
                    decls = List.collect (fun x -> x.Declarations) xs,
                    isRecursive = true
                ))
            |> List.ofSeq

        let input =
            ParsedInput.ImplFile(
                ParsedImplFileInput(
                    "tmp.fs",
                    false,
                    QualifiedNameOfFile(Ident.Create ""),
                    [],
                    [],
                    ast,
                    (false, false),
                    { ConditionalDirectives = []
                      CodeComments = [] }
                )
            )

        CodeFormatter.FormatASTAsync(input)

    let generateForFile (parameters : GenParams) (filename : string) =
        let schemaText = File.ReadAllText filename |> Schema.Parse
        schemaText |> generateForSchema parameters
