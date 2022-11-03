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

type SynMemberGetSetTrivia with
    static member Zero : SynMemberGetSetTrivia =
        { WithKeyword = range0
          GetKeyword = Some range0
          AndKeyword = Some range0
          SetKeyword = Some range0 }

type SynMemberFlagsTrivia with
    static member InstanceMember =
        { SynMemberFlagsTrivia.Zero with MemberRange = Some range0 }

    static member StaticMember =
        { SynMemberFlagsTrivia.Zero with MemberRange = Some range0; StaticRange = Some range0 }
