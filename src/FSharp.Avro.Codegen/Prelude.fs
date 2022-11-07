[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Avro.Codegen.Prelude

open FSharp.Compiler.Syntax

let thisIdent = Ident.Create "this"
let thisExpr = SynExpr.Create thisIdent

let baseIdent = Ident.Create "base"

let valueIdent = Ident.Create "value"
let valuePat = SynPat.CreateNamed valueIdent
let valueExpr = SynExpr.Create valueIdent

let inline flip f a b = f b a

let (|->) lhs rhs = SynExpr.PipeRight(lhs, rhs)

