[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.SynMatchClause

open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text.Range

type SynMatchClause with
    static member Create(pattern : SynPat, whenExp, result) =
        let trivia =
            { SynMatchClauseTrivia.ArrowRange = Some range0
              BarRange = Some range0 }

        SynMatchClause(pattern, whenExp, result, range0, DebugPointAtTarget.No, trivia)

    static member Create(ident, whenExp, result) =
        SynMatchClause.Create(SynPat.CreateNamed ident, whenExp, result)

    static member CreateOtherwise(expr : SynExpr) =
        SynMatchClause.Create(SynPat.CreateWild, None, expr)

    static member CreateOtherwiseError(errorMessage : string) =
        SynMatchClause.CreateOtherwise(SynExpr.CreateStringError errorMessage)

    static member CreateWild(expr : SynExpr) =
        SynMatchClause.Create(SynPat.CreateWild, None, expr)
