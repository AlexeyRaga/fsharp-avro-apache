namespace FSharp.Avro.Codegen

open System.Collections.Generic
open System.IO
open Avro
open Myriad.Core
open Myriad.Core.Ast
open AstExtensions
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range

module rec A =
    let getSchemasToGenerate (schema : Schema) =
        let schemas = ResizeArray()

        let rec loop (sch : Schema) =
            match sch with
            | :? FixedSchema -> schemas.Add sch
            | :? ArraySchema as s -> loop s.ItemSchema
            | :? MapSchema as s -> loop s.ValueSchema
            | :? EnumSchema -> schemas.Add sch
            | :? RecordSchema as s ->
                s.Fields |> Seq.iter (fun x -> loop x.Schema)
                schemas.Add s
            | :? UnionSchema as s -> s.Schemas |> Seq.iter loop
            | _ -> ()

        loop schema
        schemas.ToArray()

    let rec primSchemaType (schema : PrimitiveSchema) =
        match schema.Tag with
        | Schema.Type.Boolean -> SynType.Bool()
        | Schema.Type.Int -> SynType.Int()
        | Schema.Type.Long -> SynType.Int64()
        | Schema.Type.String -> SynType.String()
        | Schema.Type.Float -> SynType.Float()
        | Schema.Type.Double -> SynType.Double()
        | Schema.Type.Null -> SynType.Unit()
        | Schema.Type.Bytes -> SynType.Array(1, SynType.Byte(), range0)
        | _ -> failwith "Unexpected primitive type"

    and unionSchemaType (schema : UnionSchema) =
        let nulls, items = schema.Schemas |> List.ofSeq |> List.partition (fun x -> x.Tag = Schema.Type.Null)

        let choice =
            match items with
            | [] -> SynType.Unit()
            | [ x ] -> schemaType x
            | xs -> xs |> List.map schemaType |> SynType.Choice

        if List.isEmpty nulls then
            choice
        else
            SynType.Option(choice, true)

    and schemaType (schema : Schema) =
        match schema with
        | :? PrimitiveSchema as s -> primSchemaType s
        | :? NamedSchema as s -> SynType.Create(s.Fullname)
        | :? ArraySchema as s -> SynType.Array(1, schemaType s.ItemSchema, range0)
        | :? MapSchema as s -> SynType.Map(SynType.String(), schemaType s.ValueSchema)
        | :? UnionSchema as s -> unionSchemaType s

    let genRecord (schema : RecordSchema) =
        let mkFld (fld : Field) =
            SynField.Create(schemaType fld.Schema, name = Ident.Create(fld.Name))

        let fields = schema.Fields |> Seq.map mkFld

        let mem = SynMemberDefn.StaticMember(Ident.Create "SCHEMA", SynExpr.CreateConstString(schema.ToString()))

        let typ = SynTypeDefn.CreateRecord(Ident.Create(schema.Name), fields, members = [ mem ])

        let decl = SynModuleDecl.Types([ typ ], range0)

        SynModuleOrNamespace.CreateNamespace(Ident.CreateLong schema.Namespace, decls = [ decl ])

    let genEnum (schema : EnumSchema) =
        let mkCase name =
            SynUnionCase.Create(Ident.Create name, [])

        let schemaMember = SynMemberDefn.StaticMember(Ident.Create "SCHEMA", SynExpr.CreateConstString(schema.ToString()))

        let cases = schema.Symbols |> Seq.map mkCase |> List.ofSeq

        let typ = SynTypeDefn.CreateUnion(Ident.Create schema.Name, cases, [ schemaMember ])

        let decl = SynModuleDecl.Types([ typ ], range0)

        SynModuleOrNamespace.CreateNamespace(Ident.CreateLong schema.Namespace, decls = [ decl ])

    let genFixed (schema : FixedSchema) =
        let typeName = Ident.Create schema.Name

        let mem = SynMemberDefn.StaticMember(Ident.Create "SCHEMA", SynExpr.CreateConstString(schema.ToString()))

        let matchValueIdent = Ident.Create "x"
        let matchValueExpr = SynExpr.CreateIdent matchValueIdent

        let arrayLen = SynExpr.CreateInstanceMethodCall(LongIdentWithDots.CreateString "Array.length", matchValueExpr)
        let whenExpr = SynExpr.Condition(arrayLen, SynExpr.OpEquality, SynExpr.CreateConst(schema.Size))
        let okClause = SynMatchClause.Create(matchValueIdent, Some whenExpr, SynExpr.CreateOk matchValueExpr)

        let errClause =
            SynMatchClause.Create(
                SynPat.CreateWild,
                None,
                SynExpr.CreateStringError $"Fixed size value {schema.Fullname} is required have length {schema.Size}"
            )

        let valueExpr = SynExpr.CreateIdentString "value"
        let valuePat = SynPat.CreateLongIdent("value", [])

        let ctorBody = SynExpr.CreateMatch(valueExpr, [ okClause; errClause ])
        let ctor = SynMemberDefn.StaticMember(Ident.Create "Create", ctorBody, [ SynPat.CreateNamed(Ident.Create "value") ])

        let fld = SynField.Create(SynType.Array(1, SynType.Byte(), range0))
        let case = SynUnionCase.Create(Ident.Create schema.Name, [ fld ])
        let typ = SynTypeDefn.CreateUnion(typeName, [ case ], [ mem; ctor ], access = SynAccess.Private)
        let decl = SynModuleDecl.Types([ typ ], range0)

        let valueMatch = SynPat.CreateLongIdent(typeName, [ valuePat ]) |> SynPat.CreateParen

        let activePattern =
            SynModuleDecl.CreateLet [ SynBinding.Let(
                                          pattern = SynPat.CreateLongIdent($"|{typeName.idText}|", [ valueMatch ]),
                                          expr = valueExpr
                                      ) ]

        let attrs = SynAttributeList.Create [ SynAttribute.Create("AutoOpen") ]

        let companionModule =
            SynModuleDecl.CreateNestedModule(
                SynComponentInfo.Create(id = Ident.CreateLong(schema.Name), attributes = [ attrs ]),
                [ activePattern ]
            )

        SynModuleOrNamespace.CreateNamespace(Ident.CreateLong schema.Namespace, decls = [ decl; companionModule ])



    let genType (schema : Schema) =
        match schema with
        | :? RecordSchema as s -> genRecord s
        | :? EnumSchema as s -> genEnum s
        | :? FixedSchema as s -> genFixed s



type AvroGenerator() =
    interface IMyriadGenerator with
        member this.ValidInputExtensions = seq { ".avsc" }

        member this.Generate(context : GeneratorContext) : Output =
            let schema = context.InputFilename |> File.ReadAllText |> Schema.Parse

            A.getSchemasToGenerate schema |> Seq.map A.genType |> List.ofSeq |> Output.Ast
