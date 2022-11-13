[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.SynMemberDefn

open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml

type SynMemberFlags with
    static member Create(kind : SynMemberKind, ?isStatic : bool) : SynMemberFlags =
        { IsInstance = not (defaultArg isStatic false)
          MemberKind = kind
          IsDispatchSlot = false
          IsOverrideOrExplicitImpl = false
          IsFinal = false
          GetterOrSetterIsCompilerGenerated = false }

    static member InstanceMember = SynMemberFlags.Create(SynMemberKind.Member, false)
    static member StaticMember = SynMemberFlags.Create(SynMemberKind.Member, true)

type SynMemberDefn with
    static member CreateLetBinding(binding : SynBinding, ?isInline : bool, ?isRecursive : bool) =
        SynMemberDefn.LetBindings([ binding ], defaultArg isInline false, defaultArg isRecursive false, range0)

    static member Let(?access, ?isInline, ?isRecursive, ?isMutable, ?attributes, ?xmldoc, ?valData, ?pattern, ?returnInfo, ?expr, ?trivia) =
        SynMemberDefn.CreateLetBinding(
            SynBinding.Let(
                ?access = access,
                ?isInline = isInline,
                ?isMutable = isMutable,
                ?attributes = attributes,
                ?xmldoc = xmldoc,
                ?valData = valData,
                ?pattern = pattern,
                ?returnInfo = returnInfo,
                ?expr = expr,
                ?trivia = trivia
            ),
            ?isInline = isInline,
            ?isRecursive = isRecursive
        )

    static member StaticMember
        (
            name : Ident,
            body : SynExpr,
            ?args : SynPat list,
            ?attributes : SynAttributeList list,
            ?access : SynAccess,
            ?xmldoc : PreXmlDoc
        ) =
        let flags = SynMemberFlags.StaticMember
        let valData = SynValData(Some flags, SynValInfo.Empty, None)

        let memberArgs =
            match args with
            | Some xs -> [ SynPat.Tuple(false, xs, range0) |> SynPat.CreateParen ]
            | None -> []

        let pat = SynPat.Create(SynLongIdent.Create [ name ], memberArgs, ?access = access)

        let bnd =
            SynBinding.Let(
                valData = valData,
                pattern = pat,
                expr = body,
                trivia = SynBindingTrivia.StaticMember,
                ?attributes = attributes,
                ?access = access,
                ?xmldoc = xmldoc
            )

        SynMemberDefn.Member(bnd, range0)

    static member InstanceMember
        (
            thisIdent : Ident,
            name : Ident,
            body : SynExpr,
            ?args : SynPat list,
            ?isOverride : bool,
            ?attributes : SynAttributeList list,
            ?access : SynAccess,
            ?xmldoc : PreXmlDoc
        ) =
        let flags, trivia =
            if defaultArg isOverride false then
                { SynMemberFlags.InstanceMember with IsOverrideOrExplicitImpl = true }, SynBindingTrivia.Override
            else
                SynMemberFlags.InstanceMember, SynBindingTrivia.InstanceMember

        let valData = SynValData(Some flags, SynValInfo.Empty, None)

        let memberArgs =
            match args with
            | Some xs -> [ SynPat.Tuple(false, xs, range0) |> SynPat.CreateParen ]
            | None -> []

        let pat = SynPat.Create(SynLongIdent.Create [ thisIdent; name ], memberArgs, ?access = access)

        let bnd =
            SynBinding.Let(
                valData = valData,
                pattern = pat,
                expr = body,
                trivia = trivia,
                ?attributes = attributes,
                ?access = access,
                ?xmldoc = xmldoc
            )

        SynMemberDefn.Member(bnd, range0)
