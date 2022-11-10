[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.SynUnionCase

open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Xml

open FSharp.Compiler.Text.Range

type SynUnionCase with
    static member Create
        (
            name : Ident,
            ?fields : SynField list,
            ?attributes : SynAttributes,
            ?access : SynAccess,
            ?xmldoc : PreXmlDoc
        ) =
        let trivia : SynUnionCaseTrivia = { BarRange = Some range0 }
        let attributes = defaultArg attributes SynAttributes.Empty
        let xmldoc = defaultArg xmldoc PreXmlDoc.Empty

        SynUnionCase(
            attributes,
            SynIdent(name, None),
            SynUnionCaseKind.Fields(defaultArg fields []),
            xmldoc,
            access,
            range0,
            trivia
        )
