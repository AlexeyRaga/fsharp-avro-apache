module FSharp.Avro.Tests.SimpleTests

open Test.AvroMsg
open FsToolkit.ErrorHandling
open Xunit
open Hedgehog

let genPerson : Gen<Person> =
    gen {
        let! name = Gen.string (Range.linear 0 255) Gen.unicode
        let! age = Gen.int32 (Range.linearBounded ())
        // return Person(name, age)
        return { name = name; age = age }
    }

let genTestMessage : Gen<TestMessage> =
    gen {
        let! mid = Gen.guid
        let! num = Gen.int32 (Range.linearBounded ())
        let! optNum = Gen.int32 (Range.linearBounded ()) |> Gen.option
        let! str = Gen.string (Range.linear 0 255) Gen.alphaNum
        let! array = Gen.string (Range.linear 0 255) Gen.alphaNum |> Gen.array (Range.linear 0 13)
        let! choice = GenX.auto<Choice<string, int, bool>>
        let! optChoice = GenX.auto<Choice<string, int, bool>> |> Gen.option
        let! map = GenX.auto<Map<string, bool>>
        let! md5 = Gen.byte (Range.linearBounded ()) |> Gen.array (Range.singleton 16) |> Gen.map (MD5.Create >> Result.either id failwith)
        let! suit = GenX.auto<Suit>
        let! owner = genPerson
        let! contact = genPerson |> Gen.option

        let! super =
            Gen.choice
                [ genPerson |> Gen.map Choice2Of2
                  Gen.string (Range.linear 0 10) Gen.unicode |> Gen.map Choice1Of2 ]
            |> Gen.option

        return
            // TestMessage(mid, num, array, optNum, str, choice, optChoice, map, md5, suit, owner, contact, Some super)
            { id = mid; num = num; array = array; optional_num = optNum; str = str; choice = choice; optional_choice = optChoice; map = map; md5 = md5; suit = suit; owner = owner; contact = contact; supervisor = super }
    }

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
    |> Property.recheckBool "0_4700450397287374730_11584491185472260345_"
