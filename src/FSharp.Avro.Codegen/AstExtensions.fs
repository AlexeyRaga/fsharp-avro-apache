[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Avro.Codegen.AstExtensions

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range

type SynExpr with
    static member CreateDowncastLambda(typ : SynType) =
        SynExpr.CreateLambda(Ident.Create "x", SynExpr.Downcast(SynExpr.CreateIdentString "x", typ, range0))

    static member CreateDowncastOptionPipeline(typ : SynType) =
        SynExpr.CreatePipeRight(SynExpr.CreateOptionOfObj(), SynExpr.CreateOptionMap(SynExpr.CreateDowncastLambda typ))

    static member CreateDowncastOption(expr : SynExpr, typ : SynType) =
        SynExpr.CreatePipeRight(
            expr,
            SynExpr.CreatePipeRight(SynExpr.CreateOptionOfObj(), SynExpr.CreateOptionMap(SynExpr.CreateDowncastLambda typ))
        )

    static member CreateFailwith(err : string) =
        SynExpr.CreateApp(SynExpr.CreateIdentString "failwith", SynExpr.CreateConst(SynConst.CreateString err))


type SynMatchClause with
    static member CreateOtherwiseFailwith(errorMessage : string) =
        SynMatchClause.Create(SynPat.CreateWild, None, SynExpr.CreateFailwith errorMessage)
