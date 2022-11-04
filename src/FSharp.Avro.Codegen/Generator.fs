namespace FSharp.Avro.Codegen

open System.IO
open Avro
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open AstExtensions
open Fantomas.Core
open Schema
open FSharp.Avro.Codegen.Generators

module rec A =

    let private genericAvroTyp = SynType.Create("Avro.Generic.GenericRecord")
    let private fromAvroMethodIdent = Ident.Create "FromAvro"

    let private callFromAvro (schema : NamedSchema) (value : SynExpr) =
        SynExpr.CreateInstanceMethodCall(SynLongIdent.Create $"{schema.Fullname}.{fromAvroMethodIdent.idText}", SynExpr.EnsureParen value)

    let mkFromAvro (typ : SynType) (f : Ident -> SynExpr) =
        let valuePat = SynPat.CreateLongIdent(valueIdent, [])
        let typedVal = SynPat.CreateTyped(valuePat, typ)
        SynMemberDefn.StaticMember(fromAvroMethodIdent, f valueIdent, [ typedVal ])

    let getSchemasToGenerate (schema : Schema) =
        let schemas = ResizeArray<Schema>()

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
        | Schema.Type.Boolean -> SynType.Bool
        | Schema.Type.Int -> SynType.Int
        | Schema.Type.Long -> SynType.Int64
        | Schema.Type.String -> SynType.String
        | Schema.Type.Float -> SynType.Float
        | Schema.Type.Double -> SynType.Double
        | Schema.Type.Null -> SynType.Unit
        | Schema.Type.Bytes -> SynType.CreateArray SynType.Byte
        | _ -> failwith "Unexpected primitive type"

    and unionSchemaType (schema : UnionSchema) =
        let buildChoice xs =
            xs |> List.map (snd >> schemaType) |> SynType.Choice

        match schema with
        | UnionEmpty -> SynType.Unit
        | UnionSingleOptional (_, x) -> SynType.Option(schemaType x, true)
        | UnionOptionalCases xs -> SynType.Option(buildChoice xs, true)
        | UnionCases xs -> buildChoice xs
        | UnionSingle (_, x) -> schemaType x

    and schemaType (schema : Schema) =
        match schema with
        | :? PrimitiveSchema as s -> primSchemaType s
        | :? NamedSchema as s -> SynType.Create(s.Fullname)
        | :? ArraySchema as s -> SynType.Array(1, schemaType s.ItemSchema, range0)
        | :? MapSchema as s -> SynType.Map(SynType.String, schemaType s.ValueSchema)
        | :? UnionSchema as s -> unionSchemaType s
        | :? LogicalSchema as s -> SynType.Create(s.LogicalType.GetCSharpType(false).FullName)
        | _ -> failwith $"Unexpected schema: {schema.Fullname} of type {schema.GetType().FullName}"

    and genGetValue (schema : Schema) (typ : SynType) (expr : SynExpr) =
        match schema with
        | :? PrimitiveSchema as s -> SynExpr.Downcast(expr, typ, range0)
        | :? FixedSchema as s -> SynExpr.CreateDowncast(expr, SynType.CreateArray(SynType.Byte)) |> callFromAvro s
        | :? EnumSchema as s -> SynExpr.CreateDowncast(expr, SynType.Int) |> callFromAvro s
        | :? UnionSchema as u ->
            match u with
            | UnionSingle (ix, s) -> SynExpr.Downcast(expr, schemaType s, range0)
            | UnionSingleOptional (ix, s) -> SynExpr.CreateDowncastOption(expr, schemaType s)
            | _ -> SynExpr.CreateConst(SynConst.Unit)
        | s -> SynExpr.CreateConst(SynConst.CreateString($"Not implemented: {s}"))

    let genRecord (schema : RecordSchema) =
        let typeName = Ident.Create schema.Name

        let mkFld (fld : Field) =
            let typ = schemaType fld.Schema
            let fid = Ident.Prefixed("__", Ident.Create(fld.Name))
            let pid = Ident.Create(fld.Name)

            { field = fld
              privateFieldId = fid
              propertyId = pid
              typ = typ }

        let fields = schema.Fields |> Seq.map mkFld |> Seq.toList
        let typ = SpecificRecord.createRecord typeName schema fields
        let decl = SynModuleDecl.Types([ typ ], range0)
        SynModuleOrNamespace.CreateNamespace(Ident.CreateLong schema.Namespace, decls = [ decl ], isRecursive = true)

    let genEnum (schema : EnumSchema) =
        let fromAvro =
            mkFromAvro (SynType.Int) (fun value ->
                let cases =
                    schema.Symbols
                    |> Seq.indexed
                    |> Seq.map (fun (ix, case) ->
                        SynMatchClause.Create(
                            SynPat.CreateConst(SynConst.Int32 ix),
                            None,
                            SynExpr.CreateLongIdent(SynLongIdent.Create $"{schema.Fullname}.{case}")
                        ))

                let cases = Seq.append cases [ SynMatchClause.CreateOtherwiseFailwith($"Invalid value for enum {schema.Fullname}") ]
                SynExpr.CreateMatch(SynExpr.CreateIdent value, Seq.toList cases))

        let cases = schema.Symbols |> Seq.map (Ident.Create >> SynUnionCase.Create) |> List.ofSeq

        let enumType =
            SynTypeDefn.CreateUnion(
                Ident.Create schema.Name,
                cases,
                [ schemaMember schema; fromAvro ],
                attributes = [ SynAttributeList.Create(SynAttribute.RequireQualifiedAccess) ]
            )

        let decl = SynModuleDecl.CreateType(enumType)
        SynModuleOrNamespace.CreateNamespace(Ident.CreateLong schema.Namespace, decls = [ decl ], isRecursive = true)

    let genFixed (schema : FixedSchema) =
        let typeName = Ident.Create schema.Name
        let thisIdent = Ident.Create "this"
        let baseIdent = Ident.Create "base"
        let valueIdent = Ident.Create "value"
        let valueExpr = SynExpr.CreateIdent valueIdent

        let defaultCtor =
            SynMemberDefn.DefaultCtor([ valueIdent, SynType.CreateArray SynType.Byte ], access = SynAccess.Private(range0))

        let unsafeCtor = SynMemberDefn.CreateUnsafeCtor(typeName, [ SynType.CreateArray SynType.Byte ])

        let inheritance =
            SynMemberDefn.ImplicitInherit(
                SynType.Create "Avro.Specific.SpecificFixed",
                SynExpr.CreateParen(SynExpr.CreateApp(SynExpr.CreateIdent "uint", SynExpr.CreateConst(SynConst.Int32 schema.Size))),
                Some(Ident.Create "base"),
                range0
            )

        let ctorDo =
            let baseValue = SynExpr.CreateLongIdent([ baseIdent; Ident.Create "Value" ])
            SynMemberDefn.CreateLetBinding(SynBinding.Let(kind = SynBindingKind.Do, expr = SynExpr.Set(baseValue, valueExpr, range0)))

        let propSchema =
            SynMemberDefn.InstanceMember(
                thisIdent,
                Ident.Create "Schema",
                SynExpr.CreateApp(
                    SynExpr.CreateLongIdent "Avro.Schema.Parse",
                    SynExpr.CreateParen(SynExpr.CreateLongIdent($"{typeName.idText}.SCHEMA"))
                ),
                isOverride = true
            )

        let smartCtor =
            let arrayLen = SynExpr.CreateInstanceMethodCall(SynLongIdent.Create "Array.length", valueExpr)

            let okClause =
                let result = SynExpr.CreateOk(SynExpr.CreateParen(SynExpr.CreateApp(SynExpr.CreateIdent typeName, valueExpr)))
                SynMatchClause.Create(SynPat.CreateConst(SynConst.Int32 schema.Size), None, result)

            let errClause =
                SynMatchClause.CreateOtherwiseError $"Fixed size value {schema.Fullname} is required have length {schema.Size}"

            SynMemberDefn.StaticMember(
                Ident.Create "Create",
                SynExpr.CreateMatch(arrayLen, [ okClause; errClause ]),
                [ SynPat.CreateNamed valueIdent ]
            )

        let clazz =
            SynTypeDefn.CreateClass(
                typeName,
                members = [ defaultCtor; inheritance; ctorDo; unsafeCtor; propSchema; schemaMember schema; smartCtor ]
            )

        let typeDecl = SynModuleDecl.CreateType clazz

        let activePattern =
            let valueMatch = SynPat.CreateParen(SynPat.CreateTyped(SynPat.CreateNamed valueIdent, SynType.Create typeName))
            let getValue = SynExpr.CreateInstanceMethodCall(valueExpr, Ident.Create "Value")

            SynModuleDecl.CreateLet
                [ SynBinding.Let(pattern = SynPat.CreateLongIdent($"(|{typeName.idText}|)", [ valueMatch ]), expr = getValue) ]

        let companionModule =
            SynModuleDecl.CreateNestedModule(
                SynComponentInfo.Create(
                    id = Ident.CreateLong(schema.Name),
                    attributes = [ SynAttributeList.Create [ SynAttribute.Create("AutoOpen") ] ]
                ),
                [ activePattern ]
            )

        SynModuleOrNamespace.CreateNamespace(Ident.CreateLong schema.Namespace, decls = [ typeDecl; companionModule ])

    let genType (schema : Schema) =
        match schema with
        | :? RecordSchema as s -> genRecord s
        | :? EnumSchema as s -> genEnum s
        | :? FixedSchema as s -> genFixed s

module AvroGenerator =
    let GenerateAsync filename =
        let schema = File.ReadAllText filename |> Schema.Parse
        let ast = A.getSchemasToGenerate schema |> Seq.map A.genType |> List.ofSeq

        let input =
            ParsedInput.ImplFile(
                ParsedImplFileInput(
                    $"{filename}.fs",
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
