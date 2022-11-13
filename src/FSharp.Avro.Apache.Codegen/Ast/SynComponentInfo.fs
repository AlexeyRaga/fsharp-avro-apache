[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.SynComponentInfo

open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml

type SynComponentInfo with
    static member Create(id : LongIdent, ?attributes, ?parameters, ?constraints, ?xmldoc, ?preferPostfix, ?access) =
        let attributes = defaultArg attributes SynAttributes.Empty
        let constraints = defaultArg constraints []
        let xmldoc = defaultArg xmldoc PreXmlDoc.Empty
        let preferPostfix = defaultArg preferPostfix false
        let range = range0
        SynComponentInfo(attributes, parameters, constraints, id, xmldoc, preferPostfix, access, range)
