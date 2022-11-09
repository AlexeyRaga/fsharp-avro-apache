module FSharp.Avro.Codegen.Generators.SpecificFixed

open Avro
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open FSharp.Avro.Codegen
open FSharp.Compiler.Xml

let genSpecificFixed (schema : FixedSchema) =
    let typeName = Ident.Create schema.Name
    let valuePropIdent = Ident.Create "Value"

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
        let baseValue = SynExpr.Create([ baseIdent; valuePropIdent ])

        SynMemberDefn.CreateLetBinding(
            SynBinding.Let(
                kind = SynBindingKind.Do,
                expr =
                    SynExpr.CreateIfThenElse(SynExpr.Inequality(valueExpr, SynExpr.CreateNull), SynExpr.Set(baseValue, valueExpr, range0))
            )
        )

    let propSchema =
        SynMemberDefn.InstanceMember(
            thisIdent,
            Ident.Create "Schema",
            SynExpr.Create [ typeName; Schema.schemaStaticMemberIdent ],
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
            [ SynPat.CreateNamed valueIdent ],
            xmldoc = PreXmlDoc.Create [$"Creates an instance of {typeName.idText}."; $"Only accepts byte arrays of size {schema.Size}." ]
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
                  Schema.schemaStaticMember schema
                  smartCtor ],
            xmldoc = PreXmlDoc.Create(schema.Documentation)
        )

    let typeDecl = SynModuleDecl.CreateType clazz

    let activePattern =
        let valueMatch = SynPat.CreateParen(SynPat.CreateTyped(valuePat, SynType.Create typeName))
        let getValue = SynExpr.MethodCall(valueExpr, valuePropIdent)

        SynModuleDecl.CreateLet
            [ SynBinding.Let(
                  pattern = SynPat.Create($"(|{typeName.idText}|)", [ valueMatch ]),
                  expr = getValue,
                  xmldoc = PreXmlDoc.Create $"Deconstructs {typeName.idText} by extracting an underlying byte array value"
              ) ]

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
