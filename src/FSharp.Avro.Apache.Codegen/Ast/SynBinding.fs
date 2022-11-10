[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.SynBinding

open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml

type SynArgInfo with
    static member Empty = SynArgInfo(SynAttributes.Empty, false, None)

    static member CreateId id =
        SynArgInfo(SynAttributes.Empty, false, Some id)

    static member CreateIdString id = SynArgInfo.CreateId(Ident.Create id)

type SynValInfo with
    static member Empty = SynValInfo([], SynArgInfo.Empty)

type SynBinding with
    static member Let(?kind, ?access, ?isInline, ?isMutable, ?attributes, ?xmldoc, ?valData, ?pattern, ?returnInfo, ?expr, ?trivia) =
        let kind = defaultArg kind SynBindingKind.Normal
        let isInline = defaultArg isInline false
        let isMutable = defaultArg isMutable false
        let attributes = defaultArg attributes SynAttributes.Empty
        let xmldoc = defaultArg xmldoc PreXmlDoc.Empty
        let valData = defaultArg valData (SynValData(None, SynValInfo([], SynArgInfo.Empty), None))
        let headPat = defaultArg pattern SynPat.CreateNull
        let expr = defaultArg expr (SynExpr.CreateTyped(SynExpr.CreateNull, SynType.Unit))
        let bind = DebugPointAtBinding.NoneAtLet

        let trivia =
            defaultArg trivia
                { LetKeyword = Some range0
                  EqualsRange = Some range0 }

        SynBinding(
            access,
            kind,
            isInline,
            isMutable,
            attributes,
            xmldoc,
            valData,
            headPat,
            returnInfo,
            expr,
            range0,
            bind,
            trivia
        )

    static member Create
        (
            flags : SynMemberFlags,
            pattern : SynPat,
            expr : SynExpr,
            ?isMutable : bool,
            ?attributes : SynAttributes,
            ?xmlDoc : PreXmlDoc
        ) =
        let valData = SynValData(Some flags, SynValInfo.Empty, None)

        SynBinding.SynBinding(
            None,
            SynBindingKind.Normal,
            false,
            defaultArg isMutable false,
            defaultArg attributes SynAttributes.Empty,
            defaultArg xmlDoc PreXmlDoc.Empty,
            valData,
            pattern,
            None,
            expr,
            range0,
            DebugPointAtBinding.NoneAtLet,
            SynBindingTrivia.Zero
        )
