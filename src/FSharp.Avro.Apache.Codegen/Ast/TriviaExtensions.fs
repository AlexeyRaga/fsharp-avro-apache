[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.TriviaExtensions

open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text.Range

type ParsedImplFileInputTrivia with
    static member Zero =
        { ParsedImplFileInputTrivia.ConditionalDirectives = []
          CodeComments = [] }

type SynModuleOrNamespaceTrivia with
    static member Module =
        { SynModuleOrNamespaceTrivia.LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.Module(range0) }

    static member Namespace =
        { SynModuleOrNamespaceTrivia.LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.Namespace(range0) }

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

type SynBindingTrivia with
    static member InstanceMember : SynBindingTrivia =
        { LeadingKeyword = SynLeadingKeyword.Member(range0); EqualsRange = Some range0 }

    static member Override : SynBindingTrivia =
        { LeadingKeyword = SynLeadingKeyword.Override(range0); EqualsRange = Some range0 }

    static member StaticMember : SynBindingTrivia =
        { LeadingKeyword = SynLeadingKeyword.StaticMember(range0, range0); EqualsRange = Some range0 }

    static member Let : SynBindingTrivia =
        { LeadingKeyword = SynLeadingKeyword.Let(range0); EqualsRange = Some range0 }

    static member Do : SynBindingTrivia =
        { LeadingKeyword = SynLeadingKeyword.Do(range0); EqualsRange = Some range0 }
