[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.SynMemberDefn

open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml

type SynMemberFlags with
    static member Create(kind : SynMemberKind, ?isStatic : bool, ?trivia) : SynMemberFlags =
        { IsInstance = not (defaultArg isStatic false)
          MemberKind = kind
          IsDispatchSlot = false
          IsOverrideOrExplicitImpl = false
          IsFinal = false
          GetterOrSetterIsCompilerGenerated = false
          Trivia = defaultArg trivia SynMemberFlagsTrivia.Zero }

    static member InstanceMember = SynMemberFlags.Create(SynMemberKind.Member, false, SynMemberFlagsTrivia.InstanceMember)
    static member StaticMember = SynMemberFlags.Create(SynMemberKind.Member, true, SynMemberFlagsTrivia.StaticMember)

type SynMemberDefn with
    static member CreateLetBinding(binding : SynBinding, ?isInline : bool, ?isRecursive : bool) =
        SynMemberDefn.LetBindings([ binding ], defaultArg isInline false, defaultArg isRecursive false, range0)

    static member Let(?access, ?isInline, ?isMutable, ?attributes, ?xmldoc, ?valData, ?pattern, ?returnInfo, ?expr) =
        SynBinding.Let(
            ?access = access,
            ?isInline = isInline,
            ?isMutable = isMutable,
            ?attributes = attributes,
            ?xmldoc = xmldoc,
            ?valData = valData,
            ?pattern = pattern,
            ?returnInfo = returnInfo,
            ?expr = expr
        )
        |> SynMemberDefn.CreateLetBinding

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
            SynBinding.Let(valData = valData, pattern = pat, expr = body, ?attributes = attributes, ?access = access, ?xmldoc = xmldoc)

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
        let flags =
            if defaultArg isOverride false then
                { SynMemberFlags.InstanceMember with
                    IsOverrideOrExplicitImpl = true
                    Trivia = { SynMemberFlags.InstanceMember.Trivia with OverrideRange = Some range0 } }
            else
                SynMemberFlags.InstanceMember

        let valData = SynValData(Some flags, SynValInfo.Empty, None)

        let memberArgs =
            match args with
            | Some xs -> [ SynPat.Tuple(false, xs, range0) |> SynPat.CreateParen ]
            | None -> []

        let pat = SynPat.Create(SynLongIdent.Create [ thisIdent; name ], memberArgs, ?access = access)

        let bnd =
            SynBinding.Let(valData = valData, pattern = pat, expr = body, ?attributes = attributes, ?access = access, ?xmldoc = xmldoc)

        SynMemberDefn.Member(bnd, range0)
