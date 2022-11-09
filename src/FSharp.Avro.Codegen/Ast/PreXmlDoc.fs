[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Xml.PreXmlDoc

open FSharp.Compiler.Xml
open FSharp.Compiler.Text.Range

type PreXmlDoc with
    static member Create(doc : string list) =
        match doc with
        | [] -> PreXmlDoc.Empty
        | xs -> PreXmlDoc.Create(List.toArray xs, range0)

    static member Create(doc : string) =
        if System.String.IsNullOrEmpty doc then
            PreXmlDoc.Empty
        else
            PreXmlDoc.Create [ doc ]
