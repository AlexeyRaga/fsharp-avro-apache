module FSharp.Avro.Codegen.Generators.SpecificEnum

open Avro
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open FSharp.Avro.Codegen
open FSharp.Compiler.Xml

let fromAvroIdent = Ident.Create "FromAvroEnumValue"
let toAvroIdent = Ident.Create "ToAvroEnumValue"

let genSpecificEnum (schema : EnumSchema) =
    let fromInt =
        let typedVal = SynPat.CreateTyped(valueIdent, SynType.Int)

        let expr =
            let cases =
                schema.Symbols
                |> Seq.mapi (fun ix case -> SynMatchClause.Create(SynPat.Int32 ix, SynExpr.Create $"{schema.Fullname}.{case}"))
                |> flip Seq.append [ SynMatchClause.OtherwiseRaise(SynExpr.AvroRuntimeException(valueIdent, $"{schema.Fullname}.{fromAvroIdent}")) ]

            SynExpr.CreateMatch(valueExpr, List.ofSeq cases)

        SynMemberDefn.StaticMember(
            fromAvroIdent,
            expr,
            [ typedVal ],
            access = SynAccess.Internal(range0),
            xmldoc =
                PreXmlDoc.Create
                    [ "Only used in Avro serialisation."
                      "Is not intended to be used by in users code." ]
        )

    let toName =
        let expr =
            let cases =
                schema.Symbols
                |> Seq.map (fun case -> SynMatchClause.Create(SynPat.CreateNamed $"{schema.Fullname}.{case}", SynExpr.String case))

            SynExpr.CreateMatch(valueExpr, List.ofSeq cases)

        SynMemberDefn.StaticMember(
            toAvroIdent,
            expr,
            [ valuePat ],
            access = SynAccess.Internal(range0),
            xmldoc =
                PreXmlDoc.Create
                    [ "Only used in Avro serialisation."
                      "Is not intended to be used by in users code." ]
        )

    let cases = schema.Symbols |> Seq.map (Ident.Create >> SynUnionCase.Create) |> List.ofSeq

    let enumType =
        SynTypeDefn.CreateUnion(
            Ident.Create schema.Name,
            cases,
            [ Schema.schemaStaticMember schema; fromInt; toName ],
            attributes = [ SynAttributeList.Create [ SynAttribute.RequireQualifiedAccess; SynAttribute.Struct ] ],
            ?xmldoc = Some(PreXmlDoc.Create(schema.Documentation))
        )

    let decl = SynModuleDecl.CreateType(enumType)

    { Namespace = schema.Namespace
      Declarations = [ decl ] }
