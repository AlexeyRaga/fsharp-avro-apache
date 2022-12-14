[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.SynModuleDecl

open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range

type SynModuleDecl with
    static member CreateTypes(typeDefs : SynTypeDefn list) = SynModuleDecl.Types(typeDefs, range0)

    static member CreateType(typeDef : SynTypeDefn) =
        SynModuleDecl.Types([ typeDef ], range0)

    static member CreateLet(bindings, ?isRecursive) =
        let isRecursive = defaultArg isRecursive false
        SynModuleDecl.Let(isRecursive, bindings, range0)

    static member CreateLet(?access, ?isInline, ?isRecursive, ?isMutable, ?attributes, ?xmldoc, ?valData, ?pattern, ?returnInfo, ?expr) =
        SynModuleDecl.CreateLet(
            [ SynBinding.Let(
                  ?access = access,
                  ?isInline = isInline,
                  ?isMutable = isMutable,
                  ?attributes = attributes,
                  ?xmldoc = xmldoc,
                  ?valData = valData,
                  ?pattern = pattern,
                  ?returnInfo = returnInfo,
                  ?expr = expr,
                  trivia = SynBindingTrivia.Let
              ) ],
            ?isRecursive = isRecursive
        )

    static member CreateNestedModule(ci, decls, ?isRec, ?isCont) =
        let isRec = defaultArg isRec false
        let isCont = defaultArg isCont false

        let trivia =
            { SynModuleDeclNestedModuleTrivia.EqualsRange = Some range0
              ModuleKeyword = Some range0 }

        SynModuleDecl.NestedModule(ci, isRec, decls, isCont, range0, trivia)
