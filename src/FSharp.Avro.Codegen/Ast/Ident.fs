[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.Ident

open FSharp.Compiler.Text.Range

type Ident with
    static member Create text = Ident(text, range0)

    static member CreateLong(text : string) =
        text.Split([| '.' |]) |> List.ofArray |> List.map Ident.Create

    static member AsCamelCase(ident : Ident) =
        Ident.Create(ident.idText.Substring(0, 1).ToLowerInvariant() + ident.idText.Substring(1))

    static member Prefixed(prefix : string, ident : Ident) = Ident.Create($"{prefix}{ident.idText}")
