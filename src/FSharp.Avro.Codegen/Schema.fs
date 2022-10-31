module FSharp.Avro.Codegen.Schema

open Avro

let nullSchema = PrimitiveSchema.Create(Schema.Type.Null) :> Schema

let (|UnionSingleOptional|UnionCases|UnionOptionalCases|UnionSingle|UnionEmpty|) (schema : UnionSchema) =
    let nulls, items =
        schema.Schemas
        |> Seq.indexed
        |> List.ofSeq
        |> List.partition (fun (ix, x) -> x.Tag = Schema.Type.Null)

    let isOptional = not (List.isEmpty nulls)

    match isOptional, items with
    | false, [] -> UnionEmpty
    | false, [ x ] -> UnionSingle x
    | false, xs -> UnionCases xs

    | true, [] -> UnionSingleOptional(0, nullSchema)
    | true, [ x ] -> UnionSingleOptional x
    | true, xs -> UnionOptionalCases xs
