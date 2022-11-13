module FSharp.Avro.Tests.SimpleTests

open System.Collections.Generic
open Microsoft.FSharp.Reflection
open Test.AvroMsg
open FsToolkit.ErrorHandling
open Hedgehog
open Hedgehog.Xunit

let md5BytesGen = Gen.byte (Range.linearBounded ()) |> Gen.array (Range.singleton 16)
let fsMd5Gen = md5BytesGen |> Gen.map (MD5.Create >> Result.either id failwith)

let csMd5Gen =
    gen {
        let value = CSharp.AvroMsg.MD5()
        let! bytes = md5BytesGen
        value.Value <- bytes
        return value
    }

let genCsPerson : Gen<CSharp.AvroMsg.Person> =
    gen {
        let! name = Gen.string (Range.linear 0 255) Gen.unicode
        let! age = Gen.int32 (Range.linearBounded ())
        return CSharp.AvroMsg.Person(name = name, age = age)
    }

let genChoice3<'a, 'b, 'c> =
    Gen.choice
        [ GenX.auto<'a> |> Gen.map box
          GenX.auto<'b> |> Gen.map box
          GenX.auto<'c> |> Gen.map box ]

let genChoice2<'a, 'b> = Gen.choice [ GenX.auto<'a> |> Gen.map box; GenX.auto<'b> |> Gen.map box ]

let unwrapUnion value =
    match FSharpValue.GetUnionFields(value, value.GetType()) with
    | _, [| x |] -> x
    | _ -> failwith "Expected union type that has one value"

let toCsPerson (msg : Test.AvroMsg.Person) =
    CSharp.AvroMsg.Person(name = msg.name, age = msg.age)

let toCsMessage (msg : Test.AvroMsg.TestMessage) =
    let md5 = CSharp.AvroMsg.MD5()
    md5.Value <- msg.md5.Value

    CSharp.AvroMsg.TestMessage(
        id = msg.id,
        num = msg.num,
        array = msg.array,
        optional_num = (msg.optional_num |> Option.toNullable),
        str = msg.str,
        choice = unwrapUnion msg.choice,
        optional_choice = (msg.optional_choice |> Option.map unwrapUnion |> Option.toObj),
        map = Dictionary(msg.map),
        md5 = md5,
        suit = enum (int msg.suit),
        second_suit =
            (match msg.second_suit with
             | None -> null
             | Some (Choice1Of2 str) -> box str
             | Some (Choice2Of2 suit) -> box (enum<CSharp.AvroMsg.Suit> (int suit))),
        owner = toCsPerson msg.owner,
        contact = (msg.contact |> Option.map toCsPerson |> Option.toObj),
        supervisor =
            match msg.supervisor with
            | None -> null
            | Some (Choice1Of2 str) -> box str
            | Some (Choice2Of2 sup) -> box sup
    )

type Generators =
    static member __ =
        let genStringArray = GenX.auto<string []> |> Gen.map (fun x -> x :> IList<string>)
        let genBoolMap = GenX.auto<Dictionary<string, bool>> |> Gen.map (fun x -> x :> IDictionary<string, bool>)

        GenX.defaults
        |> AutoGenConfig.addGenerator fsMd5Gen
        |> AutoGenConfig.addGenerator csMd5Gen
        |> AutoGenConfig.addGenerator genStringArray
        |> AutoGenConfig.addGenerator genBoolMap

[<Property(typeof<Generators>)>]
let ``Should roundtrip FSharp message`` (msg : Test.AvroMsg.TestMessage) =
    let decoded = msg |> AvroCodec.encode |> AvroCodec.decode<TestMessage> TestMessage._SCHEMA
    decoded = msg

[<Property(typeof<Generators>)>]
let ``Should decode CSharp message`` (msg : Test.AvroMsg.TestMessage) =
    let csMsg = toCsMessage msg
    let decoded = msg |> AvroCodec.encode |> AvroCodec.decode<TestMessage> TestMessage._SCHEMA
    decoded = msg
