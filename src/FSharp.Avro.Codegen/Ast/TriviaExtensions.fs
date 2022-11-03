[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.TriviaExtensions

open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text.Range

type ParsedImplFileInputTrivia with
    static member Zero =
        { ParsedImplFileInputTrivia.ConditionalDirectives = []
          CodeComments = [] }

type SynModuleOrNamespaceTrivia with
    static member Zero =
        { SynModuleOrNamespaceTrivia.ModuleKeyword = Some range0
          NamespaceKeyword = Some range0 }

type SynExprMatchTrivia with
    static member Zero =
        { MatchKeyword = range0
          WithKeyword = range0 }
