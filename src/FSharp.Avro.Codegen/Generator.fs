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

type GeneratedCode =
    { Namespace : string
      Declarations : SynModuleDecl list }

module rec A =
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

        { Namespace = schema.Namespace
          Declarations = [ decl ] }

    let genEnum (schema : EnumSchema) =
        let fromInt =
            let typedVal = SynPat.CreateTyped(valueIdent, SynType.Int)

            let expr =
                let cases =
                    schema.Symbols
                    |> Seq.mapi (fun ix case -> SynMatchClause.Create(SynPat.Int32 ix, SynExpr.Create $"{schema.Fullname}.{case}"))
                    |> flip Seq.append [ SynMatchClause.OtherwiseFailwith($"Invalid value for enum {schema.Fullname}") ]

                SynExpr.CreateMatch(valueExpr, List.ofSeq cases)

            SynMemberDefn.StaticMember(Ident.Create "FromInt", expr, [ typedVal ], access = SynAccess.Internal(range0))

        let toInt =
            let expr =
                let cases =
                    schema.Symbols
                    |> Seq.mapi (fun ix case -> SynMatchClause.Create(SynPat.CreateNamed $"{schema.Fullname}.{case}", SynExpr.Int32 ix))

                SynExpr.CreateMatch(valueExpr, List.ofSeq cases)

            SynMemberDefn.StaticMember(Ident.Create "ToInt", expr, [ valuePat ], access = SynAccess.Internal(range0))

        let cases = schema.Symbols |> Seq.map (Ident.Create >> SynUnionCase.Create) |> List.ofSeq

        let enumType =
            SynTypeDefn.CreateUnion(
                Ident.Create schema.Name,
                cases,
                [ schemaMember schema; fromInt; toInt ],
                attributes = [ SynAttributeList.Create(SynAttribute.RequireQualifiedAccess) ]
            )

        let decl = SynModuleDecl.CreateType(enumType)

        { Namespace = schema.Namespace
          Declarations = [ decl ] }

    let genFixed (schema : FixedSchema) =
        let typeName = Ident.Create schema.Name
        let thisIdent = Ident.Create "this"
        let baseIdent = Ident.Create "base"
        let valueIdent = Ident.Create "value"
        let valueExpr = SynExpr.Create valueIdent

        let defaultCtor =
            SynMemberDefn.DefaultCtor([ valueIdent, SynType.CreateArray SynType.Byte ], access = SynAccess.Private(range0))

        let unsafeCtor = SynMemberDefn.UnsafeCtor(typeName, [ SynType.CreateArray SynType.Byte ])

        let inheritance =
            SynMemberDefn.ImplicitInherit(
                SynType.Create "Avro.Specific.SpecificFixed",
                SynExpr.CreateParen(SynExpr.CreateApp(SynExpr.Create "uint", SynExpr.CreateConst(SynConst.Int32 schema.Size))),
                Some(Ident.Create "base"),
                range0
            )

        let ctorDo =
            let baseValue = SynExpr.Create([ baseIdent; Ident.Create "Value" ])
            SynMemberDefn.CreateLetBinding(SynBinding.Let(kind = SynBindingKind.Do, expr = SynExpr.Set(baseValue, valueExpr, range0)))

        let propSchema =
            SynMemberDefn.InstanceMember(
                thisIdent,
                Ident.Create "Schema",
                SynExpr.CreateApp(SynExpr.Create "Avro.Schema.Parse", SynExpr.CreateParen(SynExpr.Create($"{typeName.idText}.SCHEMA"))),
                isOverride = true
            )

        let smartCtor =
            let arrayLen = SynExpr.MethodCall(SynLongIdent.Create "Array.length", valueExpr)

            let okClause =
                let result = SynExpr.Ok(SynExpr.CreateParen(SynExpr.CreateApp(SynExpr.Create typeName, valueExpr)))
                SynMatchClause.Create(SynPat.CreateConst(SynConst.Int32 schema.Size), result)

            let errClause = SynMatchClause.OtherwiseError $"Fixed size value {schema.Fullname} is required have length {schema.Size}"

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
            let getValue = SynExpr.MethodCall(valueExpr, Ident.Create "Value")
            SynModuleDecl.CreateLet [ SynBinding.Let(pattern = SynPat.Create($"(|{typeName.idText}|)", [ valueMatch ]), expr = getValue) ]

        let companionModule =
            SynModuleDecl.CreateNestedModule(
                SynComponentInfo.Create(
                    id = Ident.CreateLong(schema.Name),
                    attributes = [ SynAttributeList.Create [ SynAttribute.Create("AutoOpen") ] ]
                ),
                [ activePattern ]
            )

        { Namespace = schema.Namespace
          Declarations = [ typeDecl; companionModule ] }

    let genType (schema : Schema) =
        match schema with
        | :? RecordSchema as s -> genRecord s
        | :? EnumSchema as s -> genEnum s
        | :? FixedSchema as s -> genFixed s

module AvroGenerator =
    let GenerateAsync filename =
        let schema = File.ReadAllText filename |> Schema.Parse

        let ast =
            A.getSchemasToGenerate schema
            |> Seq.map A.genType
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
