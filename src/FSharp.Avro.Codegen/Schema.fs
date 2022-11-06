module FSharp.Avro.Codegen.Schema

open Avro
open FSharp.Compiler.Syntax

let schemaMember (schema : Schema) =
    SynMemberDefn.StaticMember(Ident.Create "SCHEMA", SynExpr.CreateConst(SynConst.CreateString(schema.ToString())))
let nullSchema = PrimitiveSchema.Create(Schema.Type.Null) :> Schema

let (|UnionSingleOptional|UnionCases|UnionOptionalCases|UnionSingle|UnionEmpty|) (schema : UnionSchema) =
    let nulls, items =
        schema.Schemas
        |> Seq.indexed
        |> List.ofSeq
        |> List.partition (fun (_, x) -> x.Tag = Schema.Type.Null)

    let isOptional = not (List.isEmpty nulls)

    match isOptional, items with
    | false, [] -> UnionEmpty
    | false, [ x ] -> UnionSingle x
    | false, xs -> UnionCases xs

    | true, [] -> UnionSingleOptional(0, nullSchema)
    | true, [ x ] -> UnionSingleOptional x
    | true, xs -> UnionOptionalCases xs


let rec primSchemaType (schema : PrimitiveSchema) =
    match schema.Tag with
    | Schema.Type.Boolean -> SynType.Bool
    | Schema.Type.Int -> SynType.Int
    | Schema.Type.Long -> SynType.Int64
    | Schema.Type.String -> SynType.String
    | Schema.Type.Float -> SynType.Float
    | Schema.Type.Double -> SynType.Double
    | Schema.Type.Null -> SynType.Unit
    | Schema.Type.Bytes -> SynType.CreateArray SynType.Byte
    | _ -> failwith "Unexpected primitive type"

let rec schemaType (schema : Schema) =
    match schema with
    | :? PrimitiveSchema as s -> primSchemaType s
    | :? NamedSchema as s -> SynType.Create(s.Fullname)
    | :? ArraySchema as s -> SynType.CreateArray(schemaType s.ItemSchema)
    | :? MapSchema as s -> SynType.Map(SynType.String, schemaType s.ValueSchema)
    | :? UnionSchema as s -> unionSchemaType s
    | :? LogicalSchema as s -> SynType.Create(s.LogicalType.GetCSharpType(false).FullName)
    | _ -> failwith $"Unexpected schema: {schema.Fullname} of type {schema.GetType().FullName}"

and unionSchemaType (schema : UnionSchema) =
    let buildChoice xs =
        xs |> List.map (snd >> schemaType) |> SynType.Choice

    match schema with
    | UnionEmpty -> SynType.Unit
    | UnionSingleOptional (_, x) -> SynType.Option(schemaType x, true)
    | UnionOptionalCases xs -> SynType.Option(buildChoice xs, true)
    | UnionCases xs -> buildChoice xs
    | UnionSingle (_, x) -> schemaType x
