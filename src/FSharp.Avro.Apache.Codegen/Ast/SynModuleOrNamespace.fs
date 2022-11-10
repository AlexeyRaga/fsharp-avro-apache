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
        SynModuleOrNamespace(ident, isRecursive, kind, decls, docs, attribs, access, range, SynModuleOrNamespaceTrivia.Zero)

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

    static member CreateAnonModule(?ident, ?isRecursive, ?decls, ?docs, ?attribs, ?access) =
        let ident = defaultArg ident (Ident.CreateLong "Tmp")

        SynModuleOrNamespace.Create(
            ident,
            SynModuleOrNamespaceKind.AnonModule,
            ?isRecursive = isRecursive,
            ?decls = decls,
            ?docs = docs,
            ?attribs = attribs,
            ?access = access
        )
