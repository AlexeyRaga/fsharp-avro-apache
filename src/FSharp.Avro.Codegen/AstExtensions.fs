[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Avro.Codegen.AstExtensions

open System
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Xml
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open Myriad.Core
open Myriad.Core.Ast

type SynComponentInfo with
    static member Create(id : LongIdent, ?attributes, ?parameters, ?constraints, ?xmldoc, ?preferPostfix, ?access) =
        let attributes = defaultArg attributes SynAttributes.Empty

        let constraints = defaultArg constraints []

        let xmldoc = defaultArg xmldoc PreXmlDoc.Empty

        let preferPostfix = defaultArg preferPostfix false

        let access = defaultArg access None
        let range = range0
        SynComponentInfo(attributes, parameters, constraints, id, xmldoc, preferPostfix, access, range)

type SynTypeDefn with
    static member CreateFromRepr
        (
            name : Ident,
            repr : SynTypeDefnRepr,
            ?members : SynMemberDefns,
            ?attributes : SynAttributeList list,
            ?xmldoc : PreXmlDoc
        ) =
        let name =
            SynComponentInfo.Create([ name ], attributes = defaultArg attributes [], xmldoc = defaultArg xmldoc PreXmlDoc.Empty)

        let extraMembers, trivia =
            match members with
            | None -> SynMemberDefns.Empty, SynTypeDefnTrivia.Zero
            | Some defns -> defns, { SynTypeDefnTrivia.Zero with WithKeyword = Some range0 }

        SynTypeDefn(name, repr, extraMembers, None, range0, trivia)

    static member CreateUnion
        (
            name : Ident,
            cases : SynUnionCase list,
            ?members : SynMemberDefns,
            ?access : SynAccess,
            ?attributes : SynAttributeList list,
            ?xmldoc : PreXmlDoc
        ) =
        let repr = SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(access, cases, range0), range0)

        SynTypeDefn.CreateFromRepr(
            name,
            repr,
            defaultArg members SynMemberDefns.Empty,
            defaultArg attributes [],
            defaultArg xmldoc PreXmlDoc.Empty
        )

    static member CreateRecord
        (
            name : Ident,
            fields : SynField seq,
            ?members : SynMemberDefns,
            ?attributes : SynAttributeList list,
            ?xmldoc : PreXmlDoc
        ) =
        let repr = SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(None, Seq.toList fields, range0), range0)

        SynTypeDefn.CreateFromRepr(
            name,
            repr,
            defaultArg members SynMemberDefns.Empty,
            defaultArg attributes [],
            defaultArg xmldoc PreXmlDoc.Empty
        )

type SynField with
    static member Create(fieldType : SynType, ?name : Ident, ?attributes : SynAttributes, ?access : SynAccess, ?xmldoc : PreXmlDoc) =
        let xmldoc = defaultArg xmldoc PreXmlDoc.Empty

        let attributes = defaultArg attributes SynAttributes.Empty

        SynField(attributes, false, name, fieldType, false, xmldoc, access, range0)

    static member CreateArray(elementType : SynType) =
        SynField.Create(SynType.Array(1, elementType, range0))

type SynUnionCase with
    static member Create(name : Ident, ?fields : SynField list, ?attributes : SynAttributes, ?access : SynAccess, ?xmldoc : PreXmlDoc) =
        let trivia : SynUnionCaseTrivia = { BarRange = Some range0 }

        let attributes = defaultArg attributes SynAttributes.Empty

        let xmldoc = defaultArg xmldoc PreXmlDoc.Empty

        SynUnionCase(attributes, name, SynUnionCaseKind.Fields(defaultArg fields []), xmldoc, access, range0, trivia)

type SynType with
    static member Option(inner : SynType, ?isPostfix : bool) =
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

    static member Choice(choices : SynType list) =
        SynType.App(
            typeName = SynType.CreateLongIdent "Choice",
            typeArgs = choices,
            commaRanges = [],
            isPostfix = false,
            range = range0,
            greaterRange = Some range0,
            lessRange = Some range0
        )

    static member CreateMap(key : SynType, value : SynType) =
        SynType.App(
            typeName = SynType.CreateLongIdent "Map",
            typeArgs = [ key; value ],
            commaRanges = [],
            isPostfix = false,
            range = range0,
            greaterRange = Some range0,
            lessRange = Some range0
        )

    static member CreateArray(inner : SynType, ?rank : int) =
        SynType.Array(defaultArg rank 1, inner, range0)


type SynMemberDefn with

    static member StaticMember(name : Ident, body : SynExpr, ?args : SynPat list) =
        let flags =
            { SynMemberFlags.StaticMember with
                Trivia =
                    { SynMemberFlags.StaticMember.Trivia with
                        MemberRange = Some range0
                        StaticRange = Some range0 } }

        let valData = SynValData(Some flags, SynValInfo.Empty, None)

        let memberArgs =
            match args with
            | Some xs -> [ SynPat.Tuple(false, xs, range0) |> SynPat.CreateParen ]
            | None -> []

        let pat = SynPat.CreateLongIdent(LongIdentWithDots.CreateFromLongIdent [ name ], memberArgs)

        let bnd =
            SynBinding.SynBinding(
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
                range0,
                DebugPointAtBinding.NoneAtLet,
                SynBindingTrivia.Zero
            )

        SynMemberDefn.Member(bnd, range0)

type SynPat with
    static member CreateLongIdent(name : string, args : SynPat list) =
        SynPat.CreateLongIdent(LongIdentWithDots.CreateString name, args)

    static member CreateLongIdent(ident : Ident, args : SynPat list) =
        let (i : ParsedInput) = failwith "foo"
        SynPat.CreateLongIdent(LongIdentWithDots.CreateFromLongIdent [ ident ], args)

    static member CreateNamed(name : string) = SynPat.CreateNamed(Ident.Create name)

type SynExpr with
    static member EnsureParen(value : SynExpr) =
        match value with
        | SynExpr.Paren _ -> value
        | _ -> SynExpr.CreateParen value

    static member CreateApp(func : Ident, [<ParamArray>] args : Ident array) =
        let funcExpr = SynExpr.CreateIdent func
        let argExprs = args |> Seq.map SynExpr.CreateIdent |> List.ofSeq

        match argExprs with
        | [] -> SynExpr.CreateApp(funcExpr, SynExpr.CreateConst(SynConst.Unit))
        | [ x ] -> SynExpr.CreateApp(funcExpr, SynExpr.EnsureParen x)
        | _ -> SynExpr.CreateApp(funcExpr, SynExpr.CreateParenedTuple(argExprs))

    static member CreateConst(value : int) =
        SynExpr.CreateConst(SynConst.Int32 value)

    static member OpEquality = SynExpr.CreateIdent(Ident.Create("op_Equality"))

    static member Condition(lhs, comp, rhs) =
        SynExpr.CreateApp(lhs, SynExpr.CreateApp(comp, rhs))

    static member CreateOk(value : SynExpr) =
        SynExpr.CreateApp(SynExpr.CreateIdentString "Ok", value)

    static member CreateError(value : SynExpr) =
        SynExpr.CreateApp(SynExpr.CreateIdentString "Error", value)

    static member CreateStringError(value : string) =
        SynExpr.CreateApp(SynExpr.CreateIdentString "Error", SynExpr.CreateConstString value)

    static member CreatePipeRightOp = SynExpr.CreateIdent(Ident.Create "op_PipeRight")

    static member CreatePipeRight(lhs : SynExpr, rhs : SynExpr) =
        SynExpr.CreateApp(lhs, SynExpr.CreateApp(SynExpr.CreatePipeRightOp, rhs))

    static member CreateDowncastLambda(typ : SynType) =
        SynExpr.CreateLambda([ SynPat.CreateNamed("x") ], SynExpr.Downcast(SynExpr.CreateIdentString "x", typ, range0))

    static member CreateOptionOfObj(?expr : SynExpr) =
        let func = LongIdentWithDots.CreateString "Option.ofObj"

        match expr with
        | None -> SynExpr.CreateLongIdent(func)
        | Some (SynExpr.Paren _ as e) -> SynExpr.CreateInstanceMethodCall(func, e)
        | Some e -> SynExpr.CreateInstanceMethodCall(func, e |> SynExpr.CreateParen)

    static member CreateOptionMap(expr : SynExpr) =
        SynExpr.CreateInstanceMethodCall(LongIdentWithDots.CreateString "Option.map", SynExpr.EnsureParen expr)

    static member CreateDowncast(expr : SynExpr, typ : SynType) = SynExpr.Downcast(expr, typ, range0)

    static member CreateDowncastOptionPipeline(typ : SynType) =
        SynExpr.CreatePipeRight(SynExpr.CreateOptionOfObj(), SynExpr.CreateOptionMap(SynExpr.CreateDowncastLambda typ))

    static member CreateDowncastOption(expr : SynExpr, typ : SynType) =
        SynExpr.CreatePipeRight(
            expr,
            SynExpr.CreatePipeRight(SynExpr.CreateOptionOfObj(), SynExpr.CreateOptionMap(SynExpr.CreateDowncastLambda typ))
        )

    static member CreateFailwith(err : string) =
        SynExpr.CreateApp(SynExpr.CreateIdentString "failwith", SynExpr.CreateConstString err)

type SynMatchClause with
    static member Create(ident, whenExp, result) =
        SynMatchClause.Create(SynPat.CreateNamed ident, whenExp, result)

    static member CreateOtherwiseError(errorMessage : string) =
        SynMatchClause.Create(SynPat.CreateWild, None, SynExpr.CreateStringError errorMessage)

    static member CreateWild(expr : SynExpr) =
        SynMatchClause.Create(SynPat.CreateWild, None, expr)

    static member CreateOtherwiseFailwith(errorMessage : string) =
        SynMatchClause.Create(SynPat.CreateWild, None, SynExpr.CreateFailwith errorMessage)

type SynModuleDecl with
    static member CreateTypes(typeDefs : SynTypeDefn list) = SynModuleDecl.Types(typeDefs, range0)

    static member CreateType(typeDef : SynTypeDefn) =
        SynModuleDecl.Types([ typeDef ], range0)
