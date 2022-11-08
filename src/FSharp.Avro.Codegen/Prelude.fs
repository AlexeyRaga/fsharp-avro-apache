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

module Seq =
    let partitionByConsequentKey (f : 'v -> 'k) (xs : 'v seq) =
        let group = ResizeArray()
        let mutable currentK = None

        let init k v =
            group.Clear()
            group.Add v
            currentK <- Some k

        let grouped =
            seq {
                for v in xs do
                    let k = f v
                    match currentK with
                    | Some k' when k' = k -> group.Add v
                    | Some k' ->
                        yield (k', List.ofSeq group)
                        init k v
                    | None -> init k v
            }

        let tail =
            seq {
                match currentK with
                | Some k -> yield (k, List.ofSeq group)
                | _ -> ()
            }

        Seq.append grouped tail
