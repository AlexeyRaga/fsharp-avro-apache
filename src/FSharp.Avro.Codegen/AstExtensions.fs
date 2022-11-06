[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Avro.Codegen.AstExtensions

open Avro
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml

type SynExpr with
    static member CreateDowncastLambda(typ : SynType) =
        SynExpr.CreateLambda(Ident.Create "x", SynExpr.Downcast(SynExpr.CreateIdent "x", typ, range0))

    static member CreateDowncastOptionPipeline(typ : SynType) =
        SynExpr.CreatePipeRight(SynExpr.CreateOptionOfObj(), SynExpr.CreateOptionMap(SynExpr.CreateDowncastLambda typ))

    static member CreateDowncastOption(expr : SynExpr, typ : SynType) =
        SynExpr.CreatePipeRight(
            expr,
            SynExpr.CreatePipeRight(SynExpr.CreateOptionOfObj(), SynExpr.CreateOptionMap(SynExpr.CreateDowncastLambda typ))
        )

    static member CreateFailwith(err : string) =
        SynExpr.CreateApp(SynExpr.CreateIdent "failwith", SynExpr.CreateConst(SynConst.CreateString err))

    static member UncheckedDefault(typ : SynType) =
        SynExpr.CreateInstanceMethodCall(SynLongIdent.Create("Unchecked.defaultof"), [ typ ])

type SynMatchClause with
    static member CreateOtherwiseFailwith(errorMessage : string) =
        SynMatchClause.Create(SynPat.CreateWild, SynExpr.CreateFailwith errorMessage)

type SynMemberDefn with
    static member DefaultCtor(parameters : (Ident * SynType) list, ?selfIdent : Ident, ?access : SynAccess, ?attributes : SynAttributes) =
        let ctorPats = parameters |> List.map SynSimplePat.CreateTyped

        SynMemberDefn.ImplicitCtor(
            access,
            defaultArg attributes SynAttributes.Empty,
            SynSimplePats.Create ctorPats,
            selfIdent,
            PreXmlDoc.Empty,
            range0
        )

    static member CreateUnsafeCtor(typeName : Ident, types : SynType list) =
        let unsafeArgs = types |> List.map SynExpr.UncheckedDefault
        let callCtor = SynExpr.CreateApp(SynExpr.Ident typeName, unsafeArgs)

        SynMemberDefn.Member(
            SynBinding.Create(
                SynMemberFlags.Create(SynMemberKind.Constructor),
                SynPat.CreateLongIdent("new", [ SynPat.CreateUnit ]),
                callCtor,
                attributes = [ SynAttributeList.Create SynAttribute.NotForFSharp ]
            ),
            range0
        )

    static member CreatePropertyWithPrivateSetter(thisIdent : Ident, typ : SynType, fieldIdent : Ident, propIdent : Ident) =
        let fld = SynMemberDefn.Let(isMutable = true, pattern = SynPat.CreateNamed fieldIdent, expr = SynExpr.CreateIdent propIdent)
        let prop = SynMemberDefn.CreateGetSetProperty(thisIdent, propIdent, fieldIdent, setterAccess = SynAccess.Private(range0))
        fld, prop

    static member CreateGetterForField(thisIdent : Ident, typ : SynType, fieldIdent : Ident, propIdent : Ident) =
        let fld = SynMemberDefn.Let(isMutable = true, pattern = SynPat.CreateNamed fieldIdent, expr = SynExpr.CreateIdent propIdent)
        let prop = SynMemberDefn.InstanceMember(thisIdent, propIdent, SynExpr.CreateIdent fieldIdent)
        fld, prop
