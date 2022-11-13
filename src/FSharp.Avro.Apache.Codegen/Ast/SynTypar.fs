[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.SynTypar

open FSharp.Compiler.Text.Range

type SynTypar with
    static member Create(ident : Ident) =
        SynTypar(ident, TyparStaticReq.None, false)

    static member Create(ident : string) =
        SynTypar.Create(Ident.Create ident)

type SynTyparDecl with
    static member Create(ident : SynTypar, ?attributes : SynAttributes) =
        SynTyparDecl(defaultArg attributes SynAttributes.Empty, ident)

    static member Create(ident : Ident, ?attributes : SynAttributes) =
        SynTyparDecl(defaultArg attributes SynAttributes.Empty, SynTypar.Create ident)

    static member Create(ident : string, ?attributes : SynAttributes) =
        SynTyparDecl(defaultArg attributes SynAttributes.Empty, SynTypar.Create ident)

type SynTyparDecls with
    static member CreatePostfix(decls : SynTyparDecl list) =
        SynTyparDecls.PostfixList(decls, [], range0)

    static member CreatePostfix(decls : SynTypar list) =
        SynTyparDecls.CreatePostfix(decls |> List.map SynTyparDecl.Create)
