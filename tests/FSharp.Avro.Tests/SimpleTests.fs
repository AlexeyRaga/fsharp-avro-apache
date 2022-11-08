module FSharp.Avro.Tests.SimpleTests

open Test.AvroMsg
open FsToolkit.ErrorHandling
open Xunit
open Hedgehog

let genTestMessage =
    let md5Gen =
        Gen.byte (Range.linearBounded ()) |> Gen.array (Range.singleton 16) |> Gen.map (MD5.Create >> Result.either id failwith)

    let msgConf = GenX.defaults |> AutoGenConfig.addGenerator md5Gen
    GenX.autoWith<TestMessage> msgConf

[<Fact>]
let ``Should encode and decode record`` () =
    property {
        let! msg = genTestMessage
        let decoded = msg |> AvroCodec.encode |> AvroCodec.decode<TestMessage> TestMessage._SCHEMA
        decoded = msg
    }
    |> Property.checkBool

[<Fact>]
let ``Should encode and decode as C#`` () =
    property {
        let! msg = genTestMessage
        let bytes = AvroCodec.encode msg
        let csRecord = AvroCodec.decode<CSharp.AvroMsg.TestMessage> CSharp.AvroMsg.TestMessage._SCHEMA bytes
        let csEncoded = AvroCodec.encode csRecord
        let decoded = AvroCodec.decode<TestMessage> TestMessage._SCHEMA csEncoded
        decoded = msg
    }
    |> Property.checkBool
