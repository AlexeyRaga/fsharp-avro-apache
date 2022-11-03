[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.SynTypeDefn

open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml

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

    static member CreateClass(name : Ident, members : SynMemberDefns, ?attributes : SynAttributeList list, ?xmldoc : PreXmlDoc) =
        SynTypeDefn.CreateFromRepr(
            name,
            SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.Class, members, range0),
            ?members = None,
            ?attributes = attributes,
            ?xmldoc = xmldoc
        )

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
