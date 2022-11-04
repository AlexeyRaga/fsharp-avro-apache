[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Avro.Codegen.Prelude

open FSharp.Compiler.Syntax

let thisIdent = Ident.Create "this"
let thisExpr = SynExpr.CreateIdent thisIdent

let baseIdent = Ident.Create "base"

let valueIdent = Ident.Create "value"
let valueExpr = SynExpr.CreateIdent valueIdent

let inline flip f a b = f b a
