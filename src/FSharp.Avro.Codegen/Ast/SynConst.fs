[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.SynConst

open FSharp.Compiler.Text.Range

type SynConst with
    static member CreateString s =
        SynConst.String(s, SynStringKind.Regular, range0)
