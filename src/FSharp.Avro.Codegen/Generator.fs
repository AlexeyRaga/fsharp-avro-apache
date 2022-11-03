namespace FSharp.Avro.Codegen

open System.IO
open Avro
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Xml
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open AstExtensions

open Fantomas.Core
open Schema


module rec A =

    let private genericAvroTyp = SynType.Create("Avro.Generic.GenericRecord")
    let private fromAvroMethodIdent = Ident.Create "FromAvro"

    let private callFromAvro (schema : NamedSchema) (value : SynExpr) =
        SynExpr.CreateInstanceMethodCall(SynLongIdent.Create $"{schema.Fullname}.{fromAvroMethodIdent.idText}", SynExpr.EnsureParen value)

    let mkFromAvro (typ : SynType) (f : Ident -> SynExpr) =
        let ident = Ident.Create "value"
        let valuePat = SynPat.CreateLongIdent(ident, [])
        let typedVal = SynPat.CreateTyped(valuePat, typ)
        SynMemberDefn.StaticMember(fromAvroMethodIdent, f ident, [ typedVal ])

    let private schemaMember (schema : Schema) =
        SynMemberDefn.StaticMember(Ident.Create "SCHEMA", SynExpr.CreateConst(SynConst.CreateString(schema.ToString())))

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

        // | :? ArraySchema as s ->

        | :? EnumSchema as s -> SynExpr.CreateDowncast(expr, SynType.Int) |> callFromAvro s

        | :? UnionSchema as u ->
            match u with
            | UnionSingle (ix, s) -> SynExpr.Downcast(expr, schemaType s, range0)
            | UnionSingleOptional (ix, s) -> SynExpr.CreateDowncastOption(expr, schemaType s)
            | _ -> SynExpr.CreateConst(SynConst.Unit)

        | s -> SynExpr.CreateConst(SynConst.CreateString($"Not implemented: {s}"))


    let genRecord (schema : RecordSchema) =
        let mkFld (fld : Field) =
            let typ = schemaType fld.Schema
            let fid = Ident.Create(fld.Name)
            fld, fid, typ, SynField.Create(typ, name = fid)

        let fields = schema.Fields |> Seq.map mkFld |> Seq.toList

        let ret = SynExpr.YieldOrReturn((false, false), SynExpr.CreateOk(SynExpr.CreateUnit), range0)

        let mem =
            mkFromAvro genericAvroTyp (fun valueIdent ->
                let getValueMethod =
                    SynLongIdent.Create [ valueIdent
                                          Ident.Create "GetValue" ]

                let getValue ix =
                    SynExpr.CreateInstanceMethodCall(getValueMethod, SynExpr.CreateConst(SynConst.Int32 ix))

                let xs =
                    fields
                    |> Seq.indexed
                    |> Seq.foldBack (fun (ix, (fld, fid, typ, _)) st ->
                        SynExpr.LetOrUseBang(
                            DebugPointAtBinding.Yes(range0),
                            false,
                            false,
                            SynPat.CreateNamed($"_{fid.idText}"),
                            genGetValue fld.Schema typ (getValue ix),
                            [],
                            st,
                            range0,
                            SynExprLetOrUseBangTrivia.Zero
                        ))

                let ys = xs ret

                SynExpr.CreateApp(SynExpr.CreateIdentString("result"), SynExpr.ComputationExpr(false, ys, range0)))

        let prop =
            SynMemberDefn.AutoProperty(
                SynAttributes.Empty,
                false,
                Ident.Create "Test",
                Some(SynType.Int),
                SynMemberKind.PropertyGet,
                (fun kind -> { SynMemberFlags.InstanceMember with Trivia = { SynMemberFlagsTrivia.Zero with MemberRange = Some range0 } }),
                PreXmlDoc.Empty,
                None,
                range0,
                SynExpr.CreateConst(SynConst.Unit),
                None,
                None,
                range0
            )

        let typ =
            SynTypeDefn.CreateRecord(
                Ident.Create(schema.Name),
                fields |> Seq.map (fun (_, _, _, x) -> x),
                members = [ prop; schemaMember schema; mem ]
            )

        let decl = SynModuleDecl.Types([ typ ], range0)

        SynModuleOrNamespace.CreateNamespace(Ident.CreateLong schema.Namespace, decls = [ decl ])

    let genEnum (schema : EnumSchema) =
        // Generate FromAvro
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
                attributes = [ SynAttributeList.Create(SynAttribute.RequireQualifiedAccess()) ]
            )

        let decl = SynModuleDecl.CreateType(enumType)

        SynModuleOrNamespace.CreateNamespace(Ident.CreateLong schema.Namespace, decls = [ decl ])

    let genFixed (schema : FixedSchema) =
        let typeName = Ident.Create schema.Name

        let valueExpr = SynExpr.CreateIdentString "value"
        let valuePat = SynPat.CreateLongIdent("value", [])

        // Generate smart constructor
        let arrayLen = SynExpr.CreateInstanceMethodCall(SynLongIdent.Create "Array.length", valueExpr)
        let okClause = SynMatchClause.Create(SynPat.CreateConst(SynConst.Int32 schema.Size), None, SynExpr.CreateOk valueExpr)

        let errClause =
            SynMatchClause.CreateOtherwiseError $"Fixed size value {schema.Fullname} is required have length {schema.Size}"

        let ctorBody = SynExpr.CreateMatch(arrayLen, [ okClause; errClause ])
        let ctor = SynMemberDefn.StaticMember(Ident.Create "Create", ctorBody, [ SynPat.CreateNamed(Ident.Create "value") ])

        // create FromAvro
        let fromAvro = mkFromAvro (SynType.CreateArray(SynType.Byte)) (fun value -> SynExpr.CreateApp(typeName, value))

        // Generate union type
        let case = SynUnionCase.Create(Ident.Create schema.Name, [ SynField.CreateArray(SynType.Byte) ])

        let fixedType =
            SynTypeDefn.CreateUnion(typeName, [ case ], [ schemaMember schema; ctor; fromAvro ], access = SynAccess.Private(range0))

        let decl = SynModuleDecl.Types([ fixedType ], range0)

        // Generate active pattern match (since the constructor is private)
        let valueMatch = SynPat.CreateLongIdent(typeName, [ valuePat ]) |> SynPat.CreateParen

        let activePattern =
            SynModuleDecl.CreateLet [ SynBinding.Let(
                                          pattern = SynPat.CreateLongIdent($"|{typeName.idText}|", [ valueMatch ]),
                                          expr = valueExpr
                                      ) ]

        let companionModule =
            SynModuleDecl.CreateNestedModule(
                SynComponentInfo.Create(
                    id = Ident.CreateLong(schema.Name),
                    attributes = [ SynAttributeList.Create [ SynAttribute.Create("AutoOpen") ] ]
                ),
                [ activePattern ]
            )

        SynModuleOrNamespace.CreateNamespace(Ident.CreateLong schema.Namespace, decls = [ decl; companionModule ])


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
