[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.SynModuleOrNamespace

open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml

type SynModuleOrNamespace with
    static member private Create
        (
            ident,
            kind : SynModuleOrNamespaceKind,
            ?isRecursive : bool,
            ?decls : SynModuleDecl list,
            ?docs : PreXmlDoc,
            ?attribs : SynAttributeList list,
            ?access : SynAccess
        ) =
        let range = range0
        let isRecursive = defaultArg isRecursive false
        let decls = defaultArg decls []
        let docs = defaultArg docs PreXmlDoc.Empty
        let attribs = defaultArg attribs SynAttributes.Empty

        let trivia =
            match kind with
            | SynModuleOrNamespaceKind.NamedModule -> SynModuleOrNamespaceTrivia.Module
            | SynModuleOrNamespaceKind.AnonModule -> SynModuleOrNamespaceTrivia.Module
            | SynModuleOrNamespaceKind.DeclaredNamespace -> SynModuleOrNamespaceTrivia.Namespace
            | SynModuleOrNamespaceKind.GlobalNamespace -> SynModuleOrNamespaceTrivia.Namespace

        SynModuleOrNamespace(ident, isRecursive, kind, decls, docs, attribs, access, range, trivia)

    static member CreateNamespace
        (
            ident : LongIdent,
            ?isRecursive : bool,
            ?decls : SynModuleDecl list,
            ?docs : PreXmlDoc,
            ?attribs : SynAttributeList list,
            ?access : SynAccess
        ) =
        SynModuleOrNamespace.Create(
            ident,
            SynModuleOrNamespaceKind.DeclaredNamespace,
            ?isRecursive = isRecursive,
            ?decls = decls,
            ?docs = docs,
            ?attribs = attribs,
            ?access = access
        )

    static member CreateModule(ident, ?isRecursive, ?decls, ?docs, ?attribs, ?access) =
        SynModuleOrNamespace.Create(
            ident,
            SynModuleOrNamespaceKind.NamedModule,
            ?isRecursive = isRecursive,
            ?decls = decls,
            ?docs = docs,
            ?attribs = attribs,
            ?access = access
        )
