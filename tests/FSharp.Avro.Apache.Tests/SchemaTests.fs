module FSharp.Avro.Tests.SchemaTests

open Avro
open FSharp.Avro.Codegen
open FSharp.Avro.Codegen.Generators
open Xunit

let simpleSchema =
    """
{
  "type": "record",
  "namespace": "Test.Schema.AvroMsg",
  "name": "TestMessage",
  "fields": [
    { "name": "id", "type": { "type": "string", "logicalType": "uuid" } },
    { "name": "num",  "type": "int" },
    { "name": "array", "type": { "type": "array", "items":  "string" } },
    { "name": "optional_num",  "type": ["null", "int"] },
    { "name": "str", "type": "string" },
    { "name": "choice", "type": ["string", "int", "boolean"] },
    { "name": "optional_choice", "type": ["null", "string", "int", "boolean"] },
    { "name": "map", "type": { "type": "map", "values": "boolean" } },
    {
      "name": "md5",
      "type": {"type": "fixed", "size": 16, "name": "MD5", "doc": "MD5 Hash sum"}
    },
    {
      "name": "suit",
      "type": {
        "type": "enum",
        "name": "Suit",
        "doc": "Your usual card deck suit",
        "symbols": ["SPADES", "HEARTS", "DIAMONDS", "CLUBS"]
      }
    },
    { "name": "second_suit", "type": ["null", "string", "Suit"] },
    { "name": "owner",
      "doc": "Who owns this thing anyway?!",
      "type": {
        "type": "record",
        "name": "Person",
        "fields": [
          { "name": "name", "type": "string" },
          { "name": "age", "type": "int" }
        ]
      }
    },
    { "name": "contact", "type": ["null", "Person"] },
    { "name": "supervisor", "type": ["null", "string", "Person"] }
  ]
}
"""

let getUniqueNamespaces schema =
    schema |> Schema.findAllSchemas |> Seq.map (fun x -> (x :?> NamedSchema).Namespace) |> Seq.distinct |> List.ofSeq

[<Fact>]
let ``Should use exact namespace`` () =
    let parameters =
        { RecordRepr = RecordRepresentation.Record
          NamespaceMapping = Map.ofSeq [ "Test.Schema.AvroMsg", "Updated.Message.Contract" ] }

    simpleSchema |> AvroGenerator.parseSchema parameters |> getUniqueNamespaces === [ "Updated.Message.Contract" ]

[<Fact>]
let ``Should update partial namespace`` () =
    let parameters =
        { RecordRepr = RecordRepresentation.Record
          NamespaceMapping = Map.ofSeq [ "Test.Schema", "Message.Contract" ] }

    simpleSchema |> AvroGenerator.parseSchema parameters |> getUniqueNamespaces === [ "Message.Contract.AvroMsg" ]

[<Fact>]
let ``Should prefer most specific namespace`` () =
    let parameters =
        { RecordRepr = RecordRepresentation.Record
          NamespaceMapping =
            Map.ofSeq
                [ "Test.Schema", "Message.Contract"
                  "Test.Schema.AvroMsg", "Updated.Message.Contract" ] }

    simpleSchema |> AvroGenerator.parseSchema parameters |> getUniqueNamespaces === [ "Updated.Message.Contract" ]
