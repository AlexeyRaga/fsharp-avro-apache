namespace Test.AvroMsg

type MD5 private (value : byte []) =
    class
        inherit Avro.Specific.SpecificFixed(uint 16)
        do base.Value <- value

        [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
        new() = MD5 Unchecked.defaultof<byte []>

        override this.Schema = Avro.Schema.Parse(MD5.SCHEMA)
        static member SCHEMA = "{\"type\":\"fixed\",\"name\":\"MD5\",\"namespace\":\"Test.AvroMsg\",\"size\":16}"

        static member Create(value) =
            match Array.length value with
            | 16 -> Ok(MD5 value)
            | _ -> Error "Fixed size value Test.AvroMsg.MD5 is required have length 16"
    end

[<AutoOpen>]
module MD5 =
    let (|MD5|) (value : MD5) = value.Value
namespace rec Test.AvroMsg

[<RequireQualifiedAccess>]
type Suit =
    | SPADES
    | HEARTS
    | DIAMONDS
    | CLUBS
    static member SCHEMA =
        "{\"type\":\"enum\",\"name\":\"Suit\",\"namespace\":\"Test.AvroMsg\",\"symbols\":[\"SPADES\",\"HEARTS\",\"DIAMONDS\",\"CLUBS\"]}"

    static member FromInt(value : int) =
        match value with
        | 0 -> Test.AvroMsg.Suit.SPADES
        | 1 -> Test.AvroMsg.Suit.HEARTS
        | 2 -> Test.AvroMsg.Suit.DIAMONDS
        | 3 -> Test.AvroMsg.Suit.CLUBS
        | _ -> failwith "Invalid value for enum Test.AvroMsg.Suit"
namespace rec Test.AvroMsg

[<CLIMutable>]
type TestMessage =
    { id : System.Guid
      num : int
      array : string []
      optional_num : option<int>
      str : string
      choice : Choice<string, int, bool>
      optional_choice : option<Choice<string, int, bool>>
      map : Map<string, bool>
      md5 : Test.AvroMsg.MD5
      suit : Test.AvroMsg.Suit }
    interface Avro.Specific.ISpecificRecord with
        [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
        member this.Get(pos : int) = ()

        [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
        member this.Put(pos : int, value : obj) =
            match pos, value with
            | 0, (:? System.Guid as x) ->
                this
                    .GetType()
                    .GetProperty("id")
                    .SetMethod.Invoke(this, [| x |])
                |> ignore
            | 1, (:? int as x) ->
                this
                    .GetType()
                    .GetProperty("num")
                    .SetMethod.Invoke(this, [| x |])
                |> ignore
            | 2, (:? (string []) as x) ->
                this
                    .GetType()
                    .GetProperty("array")
                    .SetMethod.Invoke(this, [| x |])
                |> ignore
            | 3, (:? int as x) ->
                this
                    .GetType()
                    .GetProperty("optional_num")
                    .SetMethod.Invoke(this, [| Some(x) |])
                |> ignore
            | 3, _ ->
                this
                    .GetType()
                    .GetProperty("optional_num")
                    .SetMethod.Invoke(this, [| None |])
                |> ignore
            | 4, (:? string as x) ->
                this
                    .GetType()
                    .GetProperty("str")
                    .SetMethod.Invoke(this, [| x |])
                |> ignore
            | 5, (:? string as x) ->
                this
                    .GetType()
                    .GetProperty("choice")
                    .SetMethod.Invoke(this, [| Choice1Of3 x |])
                |> ignore
            | 5, (:? int as x) ->
                this
                    .GetType()
                    .GetProperty("choice")
                    .SetMethod.Invoke(this, [| Choice2Of3 x |])
                |> ignore
            | 5, (:? bool as x) ->
                this
                    .GetType()
                    .GetProperty("choice")
                    .SetMethod.Invoke(this, [| Choice3Of3 x |])
                |> ignore
            | 6, (:? string as x) ->
                this
                    .GetType()
                    .GetProperty("optional_choice")
                    .SetMethod.Invoke(this, [| Some(Choice1Of3 x) |])
                |> ignore
            | 6, (:? int as x) ->
                this
                    .GetType()
                    .GetProperty("optional_choice")
                    .SetMethod.Invoke(this, [| Some(Choice2Of3 x) |])
                |> ignore
            | 6, (:? bool as x) ->
                this
                    .GetType()
                    .GetProperty("optional_choice")
                    .SetMethod.Invoke(this, [| Some(Choice3Of3 x) |])
                |> ignore
            | 6, _ ->
                this
                    .GetType()
                    .GetProperty("optional_choice")
                    .SetMethod.Invoke(this, [| None |])
                |> ignore
            | 7, (:? System.Collections.Generic.IDictionary<string, bool> as x) ->
                this
                    .GetType()
                    .GetProperty("map")
                    .SetMethod.Invoke(this, [| Map.ofSeq (Seq.map (|KeyValue|) x) |])
                |> ignore
            | 8, (:? Test.AvroMsg.MD5 as x) ->
                this
                    .GetType()
                    .GetProperty("md5")
                    .SetMethod.Invoke(this, [| x |])
                |> ignore
            | 9, (:? int as x) ->
                this
                    .GetType()
                    .GetProperty("suit")
                    .SetMethod.Invoke(this, [| Test.AvroMsg.Suit.FromInt x |])
                |> ignore

        member this.Schema = Avro.Schema.Parse(TestMessage.SCHEMA)

    static member SCHEMA =
        "{\"type\":\"record\",\"name\":\"TestMessage\",\"namespace\":\"Test.AvroMsg\",\"fields\":[{\"name\":\"id\",\"type\":{\"type\":\"string\",\"logicalType\":\"uuid\"}},{\"name\":\"num\",\"type\":\"int\"},{\"name\":\"array\",\"type\":{\"type\":\"array\",\"items\":\"string\"}},{\"name\":\"optional_num\",\"type\":[\"null\",\"int\"]},{\"name\":\"str\",\"type\":\"string\"},{\"name\":\"choice\",\"type\":[\"string\",\"int\",\"boolean\"]},{\"name\":\"optional_choice\",\"type\":[\"null\",\"string\",\"int\",\"boolean\"]},{\"name\":\"map\",\"type\":{\"type\":\"map\",\"values\":\"boolean\"}},{\"name\":\"md5\",\"type\":{\"type\":\"fixed\",\"name\":\"MD5\",\"namespace\":\"Test.AvroMsg\",\"size\":16}},{\"name\":\"suit\",\"type\":{\"type\":\"enum\",\"name\":\"Suit\",\"namespace\":\"Test.AvroMsg\",\"symbols\":[\"SPADES\",\"HEARTS\",\"DIAMONDS\",\"CLUBS\"]}}]}"
