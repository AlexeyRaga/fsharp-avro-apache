[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.SynConst

open FSharp.Compiler.Text.Range

type SynConst with
    static member CreateString(value : string, ?kind : SynStringKind) =
        SynConst.String(value, defaultArg kind SynStringKind.Regular, range0)
