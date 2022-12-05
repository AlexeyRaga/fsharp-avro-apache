module FSharp.Avro.Codegen.Generators.SpecificEnum

open Avro
open FSharp.Compiler.Syntax
open FSharp.Avro.Codegen
open FSharp.Compiler.Xml

let genSpecificEnum (parameters : GenParams) (schema : EnumSchema) =
    let cases = schema.Symbols |> Seq.map Ident.Create |> List.ofSeq
    let decl = SynTypeDefn.CreateEnum(Ident.Create schema.Name, cases, ?xmldoc = Some(PreXmlDoc.Create(schema.Documentation)))

    { Namespace = parameters |> GenParams.mappedNamespace schema.Namespace
      Declarations = [ SynModuleDecl.CreateType decl ] }

