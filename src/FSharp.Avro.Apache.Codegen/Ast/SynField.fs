[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.SynField

open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml

type SynField with
    static member Create(fieldType : SynType, ?name : Ident, ?attributes : SynAttributes, ?access : SynAccess, ?xmldoc : PreXmlDoc) =
        let xmldoc = defaultArg xmldoc PreXmlDoc.Empty
        let attributes = defaultArg attributes SynAttributes.Empty
        SynField(attributes, false, name, fieldType, false, xmldoc, access, range0)

    static member CreateArray(elementType : SynType, ?rank : int, ?name : Ident, ?access : SynAccess, ?attributes : SynAttributes) =
        SynField.Create(SynType.Array(defaultArg rank 1, elementType, range0), ?name = name, ?access = access, ?attributes = attributes)
