namespace FSharp.Avro.Codegen

open System.IO
open System.Text.RegularExpressions
open Avro
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Syntax
open Fantomas.Core
open Schema
open FSharp.Avro.Codegen.Generators

type GenParams =
    { RecordRepr : RecordRepresentation
      NamespaceMapping : Map<string, string> }

module AvroGenerator =
    let private generateType (parameters : GenParams) (schema : Schema) =
        match schema with
        | :? RecordSchema as s -> SpecificRecord.genSpecificRecord parameters.RecordRepr s
        | :? EnumSchema as s -> SpecificEnum.genSpecificEnum s
        | :? FixedSchema as s -> SpecificFixed.genSpecificFixed s
        | s -> failwith $"Schema {s.Fullname} of type {s.Tag} is not expected to be generated"

    let private applyNamespaceMapping (schema: string) (mapping : Map<string, string>) =
        let mapping = mapping |> Map.toSeq |> Seq.sortByDescending (fst >> String.length) |> Seq.toArray
        Regex.Replace(
            schema,
            @"""namespace""(\s*):(\s*)""([^""]*)""",
            fun m ->
                // m.Groups[1]: whitespaces before ':'
                // m.Groups[2]: whitespaces after ':'
                // m.Groups[3]: the namespace

                let ns = m.Groups[3].Value
                match mapping |> Seq.tryFind(fst >> ns.StartsWith) with
                | None -> m.Value
                | Some (oldNs, newNs) ->
                    let ns' = ns.Replace(oldNs, newNs)
                    $@"""namespace""{m.Groups[1].Value}:{m.Groups[2].Value}""{ns'}"""
        )

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

    let parseSchema (parameters : GenParams) (schemaText : string) =
        let schemaText =
            if Map.isEmpty parameters.NamespaceMapping then schemaText
            else applyNamespaceMapping schemaText parameters.NamespaceMapping

        schemaText |> Schema.Parse

    let generateForFile (parameters : GenParams) (filename : string) =
        let schemaText = File.ReadAllText filename
        parseSchema parameters schemaText |> generateForSchema parameters
