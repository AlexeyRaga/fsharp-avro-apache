module FSharp.Avro.Codegen.Schema

open Avro
open FSharp.Compiler.Syntax

let schemaStaticMemberIdent = Ident.Create "_SCHEMA"
let schemaStaticMember (schema : Schema) =
    let schemaString = SynExpr.CreateConst(SynConst.CreateString(schema.ToString()))
    let parsedSchema = SynExpr.CreateApp(SynExpr.Create "Avro.Schema.Parse", SynExpr.CreateParen schemaString)
    SynMemberDefn.StaticMember(schemaStaticMemberIdent, parsedSchema)

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

let findAllSchemas (schema : Schema) =
    let schemas = ResizeArray<Schema>()

    let rec loop (sch : Schema) =
        match sch with
        | :? FixedSchema -> schemas.Add sch
        | :? ArraySchema as s -> loop s.ItemSchema
        | :? MapSchema as s -> loop s.ValueSchema
        | :? EnumSchema -> schemas.Add sch
        | :? RecordSchema as s ->
            s.Fields |> Seq.iter (fun x -> loop x.Schema)
            schemas.Add s
        | :? UnionSchema as s -> s.Schemas |> Seq.iter loop
        | _ -> ()

    loop schema

    schemas |> Seq.distinct |> Array.ofSeq


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

let rec schemaType (parameters : GenParams) (schema : Schema) =
    match schema with
    | :? PrimitiveSchema as s -> primSchemaType s
    | :? NamedSchema as s -> SynType.Create(GenParams.fullTypeName s.SchemaName parameters)
    | :? ArraySchema as s -> SynType.CreateArray(schemaType parameters s.ItemSchema)
    | :? MapSchema as s -> SynType.Map(SynType.String, schemaType parameters s.ValueSchema)
    | :? UnionSchema as s -> unionSchemaType parameters s
    | :? LogicalSchema as s -> SynType.Create(s.LogicalType.GetCSharpType(false).FullName)
    | _ -> failwith $"Unexpected schema: {schema.Fullname} of type {schema.GetType().FullName}"

and unionSchemaType (parameters : GenParams) (schema : UnionSchema) =
    let buildChoice xs =
        xs |> List.map (snd >> schemaType parameters) |> SynType.Choice

    match schema with
    | UnionEmpty -> SynType.Unit
    | UnionSingleOptional (_, x) -> SynType.Option(schemaType parameters x, true)
    | UnionOptionalCases xs -> SynType.Option(buildChoice xs, true)
    | UnionCases xs -> buildChoice xs
    | UnionSingle (_, x) -> schemaType parameters x
