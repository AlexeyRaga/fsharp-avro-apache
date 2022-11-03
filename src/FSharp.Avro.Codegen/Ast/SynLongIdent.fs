[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.SynLongIdent

type SynLongIdent with
    static member Create(longIdent : LongIdent) =
        SynLongIdent(longIdent, [], List.replicate longIdent.Length None)

    static member Create(ident : Ident) = SynLongIdent.Create [ ident ]

    static member Create(texts : string list) =
        SynLongIdent.Create(texts |> List.map Ident.Create)

    static member Create(text : string) =
        SynLongIdent.Create(Ident.CreateLong text)
