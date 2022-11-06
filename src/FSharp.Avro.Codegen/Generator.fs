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
    let private fromAvroMethodIdent = Ident.Create "FromAvro"

    let private callFromAvro (schema : NamedSchema) (value : SynExpr) =
        SynExpr.CreateInstanceMethodCall(SynLongIdent.Create $"{schema.Fullname}.{fromAvroMethodIdent.idText}", SynExpr.EnsureParen value)

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

    let genGetValue (schema : Schema) (typ : SynType) (expr : SynExpr) =
        match schema with
        | :? PrimitiveSchema as s -> SynExpr.Downcast(expr, typ, range0)
        | :? FixedSchema as s -> SynExpr.CreateDowncast(expr, SynType.CreateArray(SynType.Byte)) |> callFromAvro s
        | :? EnumSchema as s -> SynExpr.CreateDowncast(expr, SynType.Int) |> callFromAvro s
        | :? UnionSchema as u ->
            match u with
            | UnionSingle (_, s) -> SynExpr.Downcast(expr, schemaType s, range0)
            | UnionSingleOptional (_, s) -> SynExpr.CreateDowncastOption(expr, schemaType s)
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
            let valueIdent = Ident.Create "value"
            let typedVal = SynPat.CreateTyped(valueIdent, SynType.Int)

            let expr =
                let cases =
                    schema.Symbols
                    |> Seq.mapi (fun ix case ->
                        SynMatchClause.Create(
                            SynPat.CreateConst(SynConst.Int32 ix),
                            SynExpr.CreateLongIdent $"{schema.Fullname}.{case}"
                        ))

                let cases = Seq.append cases [ SynMatchClause.CreateOtherwiseFailwith($"Invalid value for enum {schema.Fullname}") ]
                SynExpr.CreateMatch(SynExpr.CreateIdent valueIdent, Seq.toList cases)

            SynMemberDefn.StaticMember(Ident.Create "FromInt", expr, [ typedVal ])

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
                SynMatchClause.Create(SynPat.CreateConst(SynConst.Int32 schema.Size), result)

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
                members =
                    [ defaultCtor
                      inheritance
                      ctorDo
                      unsafeCtor
                      propSchema
                      schemaMember schema
                      smartCtor ]
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
