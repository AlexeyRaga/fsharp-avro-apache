[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Avro.Codegen.AstExtensions

open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Xml
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Text.Position
open Myriad.Core
open Myriad.Core.Ast
open FSharp.Compiler.Syntax

type SynComponentInfo with
    static member Create(id: LongIdent, ?attributes, ?parameters, ?constraints, ?xmldoc, ?preferPostfix, ?access) =
        let attributes =
            defaultArg attributes SynAttributes.Empty

        let constraints = defaultArg constraints []

        let xmldoc =
            defaultArg xmldoc PreXmlDoc.Empty

        let preferPostfix =
            defaultArg preferPostfix false

        let access = defaultArg access None
        let range = range0
        SynComponentInfo(attributes, parameters, constraints, id, xmldoc, preferPostfix, access, range)

type SynTypeDefn with
    static member CreateFromRepr(name: Ident, repr: SynTypeDefnRepr, ?members: SynMemberDefns, ?xmldoc: PreXmlDoc) =
        let name =
            SynComponentInfo.Create([ name ], xmldoc = defaultArg xmldoc PreXmlDoc.Empty)

        let extraMembers, trivia =
            match members with
            | None -> SynMemberDefns.Empty, SynTypeDefnTrivia.Zero
            | Some defns -> defns, { SynTypeDefnTrivia.Zero with WithKeyword = Some range0 }

        SynTypeDefn(name, repr, extraMembers, None, range0, trivia)

    static member CreateUnion(name: Ident, cases: SynUnionCase list, ?members: SynMemberDefns, ?access: SynAccess, ?xmldoc: PreXmlDoc) =
        let repr =
            SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(access, cases, range0), range0)

        SynTypeDefn.CreateFromRepr(
            name,
            repr,
            defaultArg members SynMemberDefns.Empty,
            defaultArg xmldoc PreXmlDoc.Empty
        )

    static member CreateRecord(name: Ident, fields: SynField seq, ?members: SynMemberDefns, ?xmldoc: PreXmlDoc) =
        let repr =
            SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(None, Seq.toList fields, range0), range0)

        SynTypeDefn.CreateFromRepr(
            name,
            repr,
            defaultArg members SynMemberDefns.Empty,
            defaultArg xmldoc PreXmlDoc.Empty
        )

type SynField with
    static member Create
        (
            fieldType: SynType,
            ?name: Ident,
            ?attributes: SynAttributes,
            ?access: SynAccess,
            ?xmldoc: PreXmlDoc
        ) =
        let xmldoc =
            defaultArg xmldoc PreXmlDoc.Empty

        let attributes =
            defaultArg attributes SynAttributes.Empty

        SynField(attributes, false, name, fieldType, false, xmldoc, access, range0)

type SynUnionCase with
    static member Create
        (
            name: Ident,
            fields: SynField list,
            ?attributes: SynAttributes,
            ?access: SynAccess,
            ?xmldoc: PreXmlDoc
        ) =
        let trivia: SynUnionCaseTrivia =
            { BarRange = Some range0 }

        let attributes =
            defaultArg attributes SynAttributes.Empty

        let xmldoc =
            defaultArg xmldoc PreXmlDoc.Empty

        SynUnionCase(attributes, name, SynUnionCaseKind.Fields(fields), xmldoc, access, range0, trivia)

type SynType with
    static member Option(inner: SynType, ?isPostfix: bool) =
        let isPostfix = defaultArg isPostfix false

        SynType.App(
            typeName = SynType.CreateLongIdent(if isPostfix then "option" else "Option"),
            typeArgs = [ inner ],
            commaRanges = [],
            isPostfix = isPostfix,
            range = range0,
            greaterRange = Some range0,
            lessRange = Some range0
        )

    static member Choice(choices: SynType list) =
        SynType.App(
            typeName = SynType.CreateLongIdent "Choice",
            typeArgs = choices,
            commaRanges = [],
            isPostfix = false,
            range = range0,
            greaterRange = Some range0,
            lessRange = Some range0
        )

type SynMemberDefn with

    static member StaticMember(name : Ident, body : SynExpr, ?args : SynPat list) =
        let flags = { SynMemberFlags.StaticMember with Trivia = { SynMemberFlags.StaticMember.Trivia with MemberRange = Some range0; StaticRange = Some range0 } }
        let valData = SynValData(Some flags, SynValInfo.Empty, None)

        let memberArgs =
            match args with
            | Some xs -> [ SynPat.Tuple(false, xs, range0) |> SynPat.CreateParen ]
            | None -> []

        let pat = SynPat.CreateLongIdent(LongIdentWithDots.CreateFromLongIdent [name], memberArgs)

        let bnd = SynBinding.SynBinding(
            None,
            SynBindingKind.Normal,
            false,
            false,
            SynAttributes.Empty,
            PreXmlDoc.Empty,
            valData,
            pat,
            None,
            body,
            range0, DebugPointAtBinding.NoneAtLet, SynBindingTrivia.Zero)
        SynMemberDefn.Member(bnd, range0)

type SynMatchClause with
    static member Create(ident, whenExp, result) =
        SynMatchClause.Create(SynPat.CreateNamed ident, whenExp, result)

type SynPat with
    static member CreateLongIdent(name : string, args : SynPat list) =
        SynPat.CreateLongIdent(LongIdentWithDots.CreateString name, args)

    static member CreateLongIdent(ident : Ident, args : SynPat list) =
        SynPat.CreateLongIdent(LongIdentWithDots.CreateFromLongIdent [ident], args)

type SynExpr with
    static member CreateConst(value : int) =
        SynExpr.CreateConst(SynConst.Int32 value)
    static member OpEquality =
        SynExpr.CreateIdent(Ident.Create("op_Equality"))

    static member Condition(lhs, comp, rhs) =
        SynExpr.CreateApp(lhs, SynExpr.CreateApp(comp, rhs))

    static member CreateOk(value : SynExpr) =
        SynExpr.CreateApp(SynExpr.CreateIdentString "Ok", value)

    static member CreateError(value : SynExpr) =
        SynExpr.CreateApp(SynExpr.CreateIdentString "Error", value)

    static member CreateStringError(value : string) =
        SynExpr.CreateApp(SynExpr.CreateIdentString "Error", SynExpr.CreateConstString value )
