[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Avro.Codegen.AstExtensions

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml

type Ident with
    static member Choice(num : int, ofN : int) = Ident.Create $"Choice{num}Of{ofN}"

type SynPat with
    static member Choice(num : int, ofN : int, value : Ident) =
        SynPat.Create($"Choice{num}Of{ofN}", [ SynPat.CreateNamed value ])

    static member Some(value : SynPat) =
        SynPat.Create("Some", [ SynPat.CreateParen value ])

    static member Int32(value : int) =
        SynPat.CreateConst(SynConst.Int32 value)

    static member String(value : string) =
        SynPat.CreateConst(SynConst.CreateString value)

type SynExpr with
    static member IsNull(value : SynExpr) =
        SynExpr.Condition(SynExpr.Boxed value, SynExpr.OpEquality, SynExpr.CreateNull)

    static member Int32(value : int) =
        SynExpr.CreateConst(SynConst.Int32 value)

    static member String(value : string) =
        SynExpr.CreateConst(SynConst.CreateString value)

    static member Box = SynExpr.Create "box"
    static member Boxed(expr : SynExpr) = SynExpr.CreateApp(SynExpr.Box, expr)

    static member Choice(num : int, ofN : int, value : SynExpr) =
        SynExpr.CreateApp(SynExpr.Create(Ident.Choice(num, ofN)), value)

    static member OptionMap(expr : SynExpr) =
        SynExpr.MethodCall(SynLongIdent.Create "Option.map", expr)

    static member OptionDefault(defaultValue : SynExpr) =
        SynExpr.MethodCall(SynLongIdent.Create "Option.defaultValue", defaultValue)

    static member Some(value : SynExpr) =
        SynExpr.CreateApp(SynExpr.Create "Some", SynExpr.EnsureParen value)

    static member None = SynExpr.Create "None"

    static member Ok(value : SynExpr) =
        SynExpr.CreateApp(SynExpr.Create "Ok", SynExpr.EnsureParen value)

    static member Error(value : SynExpr) =
        SynExpr.CreateApp(SynExpr.Create "Error", SynExpr.EnsureParen value)

    static member Error(value : string) =
        SynExpr.CreateApp(SynExpr.Create "Error", SynExpr.CreateConst(SynConst.String(value, SynStringKind.Regular, range0)))

    static member SeqMap(mapFunction : SynExpr) =
        SynExpr.MethodCall(SynLongIdent.Create "Seq.map", mapFunction)

    static member Failwith(err : string) =
        SynExpr.CreateApp(SynExpr.Create "failwith", SynExpr.CreateConst(SynConst.CreateString err))

    static member UncheckedDefault(typ : SynType) =
        SynExpr.MethodCall(SynLongIdent.Create("Unchecked.defaultof"), [ typ ])

type SynMatchClause with
    static member OtherwiseError(errorMessage : string) =
        SynMatchClause.Otherwise(SynExpr.Error errorMessage)

    static member OtherwiseFailwith(errorMessage : string) =
        SynMatchClause.Otherwise(SynExpr.Failwith errorMessage)

    static member OtherwiseNull = SynMatchClause.Otherwise SynExpr.CreateNull

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

    static member UnsafeCtor(typeName : Ident, types : SynType list) =
        let unsafeArgs = types |> List.map SynExpr.UncheckedDefault
        let callCtor = SynExpr.CreateApp(SynExpr.Ident typeName, unsafeArgs)

        SynMemberDefn.Member(
            SynBinding.Create(
                SynMemberFlags.Create(SynMemberKind.Constructor),
                SynPat.Create("new", [ SynPat.CreateUnit ]),
                callCtor,
                attributes = [ SynAttributeList.Create [ SynAttribute.NotForFSharp() ] ]
            ),
            range0
        )

    static member GetterForField(thisIdent : Ident, typ : SynType, fieldIdent : Ident, propIdent : Ident, ?xmldoc : PreXmlDoc) =
        let fld = SynMemberDefn.Let(isMutable = true, pattern = SynPat.CreateNamed fieldIdent, expr = SynExpr.Create propIdent)
        let prop = SynMemberDefn.InstanceMember(thisIdent, propIdent, SynExpr.Create fieldIdent, ?xmldoc = xmldoc)
        fld, prop
