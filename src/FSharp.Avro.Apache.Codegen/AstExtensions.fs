[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Avro.Codegen.AstExtensions

open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
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
    static member CreateParenTuple(items : SynExpr list) =
        SynExpr.CreateParen(SynExpr.CreateTuple items)

    static member IsNull(value : SynExpr) =
        SynExpr.Condition(SynExpr.Boxed value, SynExpr.OpEquality, SynExpr.CreateNull)

    static member Int32(value : int) =
        SynExpr.CreateConst(SynConst.Int32 value)

    static member String(value : string) =
        SynExpr.CreateConst(SynConst.CreateString value)

    static member PlusAll(values : SynExpr list) =
        match values with
        | [] -> failwith "Cannot proceed with an empty list"
        | x :: xs -> xs |> Seq.fold (fun s e -> SynExpr.Plus(s, e)) x

    static member Box = SynExpr.Create "box"

    static member Boxed(expr : SynExpr) =
        SynExpr.CreateApp(SynExpr.Box, SynExpr.EnsureParen expr)

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

    static member Raise(exceptionExpr : SynExpr) =
        SynExpr.CreateApp(SynExpr.Create "raise", exceptionExpr)

    static member SeqMap(mapFunction : SynExpr) =
        SynExpr.MethodCall(SynLongIdent.Create "Seq.map", mapFunction)

    static member Failwith(err : string) =
        SynExpr.CreateApp(SynExpr.Create "failwith", SynExpr.CreateConst(SynConst.CreateString err))

    static member UncheckedDefault(typ : SynType) =
        SynExpr.MethodCall(SynLongIdent.Create("Unchecked.defaultof"), [ typ ])

    static member AvroRuntimeException(message : SynExpr) =
        SynExpr.CreateApp(SynExpr.Create "Avro.AvroRuntimeException", SynExpr.EnsureParen message)

    static member AvroRuntimeException(ident : Ident, where : string) =
        let message =
            SynExpr.PlusAll
                [ SynExpr.String "Bad index "
                  SynExpr.CreateApp(SynExpr.Create "string", SynExpr.Create ident)
                  SynExpr.String $" in {where}" ]

        SynExpr.CreateApp(SynExpr.Create "Avro.AvroRuntimeException", SynExpr.EnsureParen message)

    static member CreateLetOrUse(bindings : SynBinding list, expr : SynExpr, ?isUse : bool, ?isRecursive : bool) =
        SynExpr.LetOrUse(defaultArg isRecursive false, defaultArg isUse false, bindings, expr, range0, { InKeyword = None })

type SynMatchClause with
    static member OtherwiseError(errorMessage : string) =
        SynMatchClause.Otherwise(SynExpr.Error errorMessage)

    static member OtherwiseFailwith(errorMessage : string) =
        SynMatchClause.Otherwise(SynExpr.Failwith errorMessage)

    static member OtherwiseRaise(exn : SynExpr) =
        SynMatchClause.Otherwise(SynExpr.Raise(SynExpr.EnsureParen exn))

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
        let fld =
            SynMemberDefn.Let(
                isMutable = true,
                pattern = SynPat.CreateNamed fieldIdent,
                expr = SynExpr.Create propIdent,
                trivia = SynBindingTrivia.Let
            )

        let prop = SynMemberDefn.InstanceMember(thisIdent, propIdent, SynExpr.Create fieldIdent, ?xmldoc = xmldoc)
        fld, prop
