[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.SynTypeDefn

open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml

type SynTypeDefn with
    static member FromRepr
        (
            name : Ident,
            repr : SynTypeDefnRepr,
            kwd : SynTypeDefnLeadingKeyword,
            ?members : SynMemberDefns,
            ?attributes : SynAttributeList list,
            ?xmldoc : PreXmlDoc
        ) =
        let name =
            SynComponentInfo.Create([ name ], attributes = defaultArg attributes [], xmldoc = defaultArg xmldoc PreXmlDoc.Empty)

        let extraMembers, trivia =
            match members with
            | None -> SynMemberDefns.Empty, { SynTypeDefnTrivia.Zero with LeadingKeyword = kwd }
            | Some defns ->
                defns,
                { SynTypeDefnTrivia.Zero with
                    WithKeyword = Some range0
                    LeadingKeyword = kwd }

        SynTypeDefn(name, repr, extraMembers, None, range0, trivia)

    static member CreateClass(name : Ident, members : SynMemberDefns, ?attributes : SynAttributeList list, ?xmldoc : PreXmlDoc) =
        SynTypeDefn.FromRepr(
            name,
            SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.Class, members, range0),
            SynTypeDefnLeadingKeyword.Type(range0),
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

        SynTypeDefn.FromRepr(
            name,
            repr,
            SynTypeDefnLeadingKeyword.Type(range0),
            ?members = members,
            ?attributes = attributes,
            ?xmldoc = xmldoc
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

        SynTypeDefn.FromRepr(
            name,
            repr,
            SynTypeDefnLeadingKeyword.Type(range0),
            ?members = members,
            ?attributes = attributes,
            ?xmldoc = xmldoc
        )

    static member CreateEnum(name : Ident, items : Ident list, ?attributes : SynAttributeList list, ?xmldoc : PreXmlDoc) =
        let cases =
            items
            |> List.mapi (fun i case ->
                SynEnumCase(
                    SynAttributes.Empty,
                    SynIdent(case, None),
                    SynConst.Int32 i,
                    range0,
                    PreXmlDoc.Empty,
                    range0,
                    { BarRange = Some range0
                      EqualsRange = range0 }
                ))

        let repr = SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Enum(cases, range0), range0)

        SynTypeDefn.FromRepr(
            name,
            repr,
            SynTypeDefnLeadingKeyword.Type(range0),
            ?members = None,
            ?attributes = attributes,
            ?xmldoc = xmldoc
        )
