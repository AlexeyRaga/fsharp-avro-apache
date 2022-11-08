module FSharp.Avro.Codegen.Generators.SpecificEnum

open Avro
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open FSharp.Avro.Codegen

let genSpecificEnum (schema : EnumSchema) =
    let fromInt =
        let typedVal = SynPat.CreateTyped(valueIdent, SynType.Int)

        let expr =
            let cases =
                schema.Symbols
                |> Seq.mapi (fun ix case -> SynMatchClause.Create(SynPat.Int32 ix, SynExpr.Create $"{schema.Fullname}.{case}"))
                |> flip Seq.append [ SynMatchClause.OtherwiseFailwith($"Invalid value for enum {schema.Fullname}") ]

            SynExpr.CreateMatch(valueExpr, List.ofSeq cases)

        SynMemberDefn.StaticMember(Ident.Create "FromInt", expr, [ typedVal ], access = SynAccess.Internal(range0))

    let toName =
        let expr =
            let cases =
                schema.Symbols
                |> Seq.map (fun case -> SynMatchClause.Create(SynPat.CreateNamed $"{schema.Fullname}.{case}", SynExpr.String case))

            SynExpr.CreateMatch(valueExpr, List.ofSeq cases)

        SynMemberDefn.StaticMember(Ident.Create "ToName", expr, [ valuePat ], access = SynAccess.Internal(range0))

    let cases = schema.Symbols |> Seq.map (Ident.Create >> SynUnionCase.Create) |> List.ofSeq

    let enumType =
        SynTypeDefn.CreateUnion(
            Ident.Create schema.Name,
            cases,
            [ Schema.schemaStaticMember schema; fromInt; toName ],
            attributes = [ SynAttributeList.Create [ SynAttribute.RequireQualifiedAccess; SynAttribute.Struct ] ]
        )

    let decl = SynModuleDecl.CreateType(enumType)

    { Namespace = schema.Namespace
      Declarations = [ decl ] }