[<Microsoft.FSharp.Core.RequireQualifiedAccess>]
module FSharp.Avro.Tests.AvroCodec

open System
open System.IO
open Avro
open Avro.IO
open Avro.Specific


let encode (value : 'a :> ISpecificRecord) =
    let writer = SpecificWriter<'a>(value.Schema)
    use ms = new MemoryStream()
    writer.Write(value, BinaryEncoder(ms))
    ms.ToArray()

let decode<'a when 'a :> ISpecificRecord> (schema : Schema) (bytes : byte array) =
    let reader = SpecificReader<'a>(schema, schema)
    use ms = new MemoryStream(bytes)
    let stub = Activator.CreateInstance(typeof<'a>) :?> 'a
    reader.Read(stub, BinaryDecoder(ms))
