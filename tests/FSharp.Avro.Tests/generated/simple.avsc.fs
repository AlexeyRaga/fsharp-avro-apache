namespace rec Test.AvroMsg

type MD5 private (value: byte[]) =
    class
        inherit Avro.Specific.SpecificFixed(uint 16)

        do
            if value <> null then
                base.Value <- value

        [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
        new () = MD5 Unchecked.defaultof<byte[]>

        override this.Schema = MD5._SCHEMA

        static member _SCHEMA =
            Avro.Schema.Parse("{\"type\":\"fixed\",\"name\":\"MD5\",\"namespace\":\"Test.AvroMsg\",\"size\":16}")

        static member Create(value) =
            match Array.length value with
            | 16 -> Ok(MD5 value)
            | _ -> Error "Fixed size value Test.AvroMsg.MD5 is required have length 16"
    end

[<AutoOpen>]
module MD5 =
    let (|MD5|) (value: MD5) = value.Value

[<RequireQualifiedAccess; Struct>]
type Suit =
    | SPADES
    | HEARTS
    | DIAMONDS
    | CLUBS

    static member _SCHEMA =
        Avro.Schema.Parse(
            "{\"type\":\"enum\",\"name\":\"Suit\",\"namespace\":\"Test.AvroMsg\",\"symbols\":[\"SPADES\",\"HEARTS\",\"DIAMONDS\",\"CLUBS\"]}"
        )

    static member internal FromInt(value: int) =
        match value with
        | 0 -> Test.AvroMsg.Suit.SPADES
        | 1 -> Test.AvroMsg.Suit.HEARTS
        | 2 -> Test.AvroMsg.Suit.DIAMONDS
        | 3 -> Test.AvroMsg.Suit.CLUBS
        | _ -> failwith "Invalid value for enum Test.AvroMsg.Suit"

    static member internal ToName(value) =
        match value with
        | Test.AvroMsg.Suit.SPADES -> "SPADES"
        | Test.AvroMsg.Suit.HEARTS -> "HEARTS"
        | Test.AvroMsg.Suit.DIAMONDS -> "DIAMONDS"
        | Test.AvroMsg.Suit.CLUBS -> "CLUBS"

[<CLIMutable>]
type Person =
    { name: string
      age: int }

    interface Avro.Specific.ISpecificRecord with
        [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
        member this.Get(pos: int) =
            match pos with
            | 0 -> box this.name
            | 1 -> box this.age

        [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
        member this.Put(pos: int, value: obj) =
            match pos, value with
            | 0, (:? string as x) -> this.GetType().GetProperty("name").SetMethod.Invoke (this, [| x |]) |> ignore
            | 1, (:? int as x) -> this.GetType().GetProperty("age").SetMethod.Invoke (this, [| x |]) |> ignore

        member this.Schema = Person._SCHEMA

    static member _SCHEMA =
        Avro.Schema.Parse(
            "{\"type\":\"record\",\"name\":\"Person\",\"namespace\":\"Test.AvroMsg\",\"fields\":[{\"name\":\"name\",\"type\":\"string\"},{\"name\":\"age\",\"type\":\"int\"}]}"
        )

[<CLIMutable>]
type TestMessage =
    { id: System.Guid
      num: int
      array: string[]
      optional_num: int option
      str: string
      choice: Choice<string, int, bool>
      optional_choice: Choice<string, int, bool> option
      map: Map<string, bool>
      md5: Test.AvroMsg.MD5
      suit: Test.AvroMsg.Suit
      owner: Test.AvroMsg.Person
      contact: Test.AvroMsg.Person option
      supervisor: Choice<string, Test.AvroMsg.Person> option }

    interface Avro.Specific.ISpecificRecord with
        [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
        member this.Get(pos: int) =
            match pos with
            | 0 -> box this.id
            | 1 -> box this.num
            | 2 -> box this.array
            | 3 -> this.optional_num |> Option.map box |> Option.defaultValue null
            | 4 -> box this.str
            | 5 ->
                match this.choice with
                | value when box value = null -> null
                | Choice1Of3 value -> box value
                | Choice2Of3 value -> box value
                | Choice3Of3 value -> box value
                | _ -> null
            | 6 ->
                match this.optional_choice with
                | Some (Choice1Of3 value) -> box value
                | Some (Choice2Of3 value) -> box value
                | Some (Choice3Of3 value) -> box value
                | _ -> null
            | 7 when box this.map = null -> null
            | 7 -> this.map |> Map.toSeq |> dict |> System.Collections.Generic.Dictionary |> box
            | 8 -> box this.md5
            | 9 -> Test.AvroMsg.Suit.ToName this.suit |> box
            | 10 -> box this.owner
            | 11 -> this.contact |> Option.map box |> Option.defaultValue null
            | 12 ->
                match this.supervisor with
                | Some (Choice1Of2 value) -> box value
                | Some (Choice2Of2 value) -> box value
                | _ -> null

        [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
        member this.Put(pos: int, value: obj) =
            match pos, value with
            | 0, (:? System.Guid as x) -> this.GetType().GetProperty("id").SetMethod.Invoke (this, [| x |]) |> ignore
            | 1, (:? int as x) -> this.GetType().GetProperty("num").SetMethod.Invoke (this, [| x |]) |> ignore
            | 2, (:? (string seq) as x) ->
                this.GetType().GetProperty("array").SetMethod.Invoke (this, [| x |> Array.ofSeq |]) |> ignore
            | 3, (:? int as x) ->
                this.GetType().GetProperty("optional_num").SetMethod.Invoke (this, [| Some(x) |]) |> ignore
            | 3, _ -> this.GetType().GetProperty("optional_num").SetMethod.Invoke (this, [| None |]) |> ignore
            | 4, (:? string as x) -> this.GetType().GetProperty("str").SetMethod.Invoke (this, [| x |]) |> ignore
            | 5, (:? string as x) ->
                this.GetType().GetProperty("choice").SetMethod.Invoke
                    (this, [| (Choice1Of3 x: Choice<string, int, bool>) |])
                    |> ignore
            | 5, (:? int as x) ->
                this.GetType().GetProperty("choice").SetMethod.Invoke
                    (this, [| (Choice2Of3 x: Choice<string, int, bool>) |])
                    |> ignore
            | 5, (:? bool as x) ->
                this.GetType().GetProperty("choice").SetMethod.Invoke
                    (this, [| (Choice3Of3 x: Choice<string, int, bool>) |])
                    |> ignore
            | 6, (:? string as x) ->
                this.GetType().GetProperty("optional_choice").SetMethod.Invoke
                    (this, [| (Some(Choice1Of3 x): Choice<string, int, bool> option) |])
                    |> ignore
            | 6, (:? int as x) ->
                this.GetType().GetProperty("optional_choice").SetMethod.Invoke
                    (this, [| (Some(Choice2Of3 x): Choice<string, int, bool> option) |])
                    |> ignore
            | 6, (:? bool as x) ->
                this.GetType().GetProperty("optional_choice").SetMethod.Invoke
                    (this, [| (Some(Choice3Of3 x): Choice<string, int, bool> option) |])
                    |> ignore
            | 6, _ -> this.GetType().GetProperty("optional_choice").SetMethod.Invoke (this, [| None |]) |> ignore
            | 7, (:? System.Collections.Generic.IDictionary<string, bool> as x) ->
                this.GetType().GetProperty("map").SetMethod.Invoke
                    (this, [| Map.ofSeq (Seq.map (|KeyValue|) x) |])
                    |> ignore
            | 8, (:? Test.AvroMsg.MD5 as x) ->
                this.GetType().GetProperty("md5").SetMethod.Invoke (this, [| x |]) |> ignore
            | 9, (:? int as x) ->
                this.GetType().GetProperty("suit").SetMethod.Invoke (this, [| Test.AvroMsg.Suit.FromInt x |]) |> ignore
            | 10, (:? Test.AvroMsg.Person as x) ->
                this.GetType().GetProperty("owner").SetMethod.Invoke (this, [| x |]) |> ignore
            | 11, (:? Test.AvroMsg.Person as x) ->
                this.GetType().GetProperty("contact").SetMethod.Invoke (this, [| Some(x) |]) |> ignore
            | 11, _ -> this.GetType().GetProperty("contact").SetMethod.Invoke (this, [| None |]) |> ignore
            | 12, (:? string as x) ->
                this.GetType().GetProperty("supervisor").SetMethod.Invoke
                    (this, [| (Some(Choice1Of2 x): Choice<string, Test.AvroMsg.Person> option) |])
                    |> ignore
            | 12, (:? Test.AvroMsg.Person as x) ->
                this.GetType().GetProperty("supervisor").SetMethod.Invoke
                    (this, [| (Some(Choice2Of2 x): Choice<string, Test.AvroMsg.Person> option) |])
                    |> ignore
            | 12, _ -> this.GetType().GetProperty("supervisor").SetMethod.Invoke (this, [| None |]) |> ignore

        member this.Schema = TestMessage._SCHEMA

    static member _SCHEMA =
        Avro.Schema.Parse(
            "{\"type\":\"record\",\"name\":\"TestMessage\",\"namespace\":\"Test.AvroMsg\",\"fields\":[{\"name\":\"id\",\"type\":{\"type\":\"string\",\"logicalType\":\"uuid\"}},{\"name\":\"num\",\"type\":\"int\"},{\"name\":\"array\",\"type\":{\"type\":\"array\",\"items\":\"string\"}},{\"name\":\"optional_num\",\"type\":[\"null\",\"int\"]},{\"name\":\"str\",\"type\":\"string\"},{\"name\":\"choice\",\"type\":[\"string\",\"int\",\"boolean\"]},{\"name\":\"optional_choice\",\"type\":[\"null\",\"string\",\"int\",\"boolean\"]},{\"name\":\"map\",\"type\":{\"type\":\"map\",\"values\":\"boolean\"}},{\"name\":\"md5\",\"type\":{\"type\":\"fixed\",\"name\":\"MD5\",\"namespace\":\"Test.AvroMsg\",\"size\":16}},{\"name\":\"suit\",\"type\":{\"type\":\"enum\",\"name\":\"Suit\",\"namespace\":\"Test.AvroMsg\",\"symbols\":[\"SPADES\",\"HEARTS\",\"DIAMONDS\",\"CLUBS\"]}},{\"name\":\"owner\",\"type\":{\"type\":\"record\",\"name\":\"Person\",\"namespace\":\"Test.AvroMsg\",\"fields\":[{\"name\":\"name\",\"type\":\"string\"},{\"name\":\"age\",\"type\":\"int\"}]}},{\"name\":\"contact\",\"type\":[\"null\",\"Person\"]},{\"name\":\"supervisor\",\"type\":[\"null\",\"string\",\"Person\"]}]}"
        )
