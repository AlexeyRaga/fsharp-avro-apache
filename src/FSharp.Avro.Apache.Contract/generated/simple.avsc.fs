namespace rec Test.AvroMsg

///MD5 Hash sum
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
            Avro.Schema.Parse(
                "{\"type\":\"fixed\",\"name\":\"MD5\",\"doc\":\"MD5 Hash sum\",\"namespace\":\"Test.AvroMsg\",\"size\":16}"
            )

        ///Creates an instance of MD5.
        ///Only accepts byte arrays of size 16.
        static member Create(value) =
            match Array.length (value) with
            | 16 -> Ok(MD5 value)
            | _ -> Error "Fixed size value Test.AvroMsg.MD5 is required have length 16"
    end

[<AutoOpen>]
module MD5 =
    ///Deconstructs MD5 by extracting an underlying byte array value
    let (|MD5|) (value: MD5) = value.Value

///Your usual card deck suit
[<RequireQualifiedAccess; Struct>]
type Suit =
    | SPADES
    | HEARTS
    | DIAMONDS
    | CLUBS

    static member _SCHEMA =
        Avro.Schema.Parse(
            "{\"type\":\"enum\",\"name\":\"Suit\",\"doc\":\"Your usual card deck suit\",\"namespace\":\"Test.AvroMsg\",\"symbols\":[\"SPADES\",\"HEARTS\",\"DIAMONDS\",\"CLUBS\"]}"
        )

    ///Only used in Avro serialisation.
    ///Is not intended to be used by in users code.
    static member internal FromAvroEnumValue(value: int) =
        match value with
        | 0 -> Test.AvroMsg.Suit.SPADES
        | 1 -> Test.AvroMsg.Suit.HEARTS
        | 2 -> Test.AvroMsg.Suit.DIAMONDS
        | 3 -> Test.AvroMsg.Suit.CLUBS
        | _ ->
            raise (Avro.AvroRuntimeException("Bad index " + string value + " in Test.AvroMsg.Suit.FromAvroEnumValue"))

    ///Only used in Avro serialisation.
    ///Is not intended to be used by in users code.
    static member internal ToAvroEnumValue(value) =
        match value with
        | Test.AvroMsg.Suit.SPADES -> "SPADES"
        | Test.AvroMsg.Suit.HEARTS -> "HEARTS"
        | Test.AvroMsg.Suit.DIAMONDS -> "DIAMONDS"
        | Test.AvroMsg.Suit.CLUBS -> "CLUBS"

[<AutoOpen>]
module internal Person =
    let mkSetter<'T> (name: string) =
        System.Delegate.CreateDelegate(
            typeof<System.Action<Test.AvroMsg.Person, 'T>>,
            typeof<Test.AvroMsg.Person>.GetProperty(name).GetSetMethod()
        )
        :?> System.Action<Test.AvroMsg.Person, 'T>

    let set_name = mkSetter<string> "name"
    let set_age = mkSetter<int> "age"

[<CLIMutable>]
type Person =
    { name: string
      age: int }

    interface Avro.Specific.ISpecificRecord with
        [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
        member this.Get(pos: int) =
            match pos with
            | 0 -> box (this.name)
            | 1 -> box (this.age)
            | _ -> raise (Avro.AvroRuntimeException("Bad index " + string pos + " in Get()"))

        [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
        member this.Put(pos: int, value: obj) =
            match pos, value with
            | 0, (:? string as x) -> set_name.Invoke(this, x)
            | 1, (:? int as x) -> set_age.Invoke(this, x)
            | _ -> raise (Avro.AvroRuntimeException("Bad index " + string pos + " in Get()"))

        member this.Schema = Person._SCHEMA

    static member _SCHEMA =
        Avro.Schema.Parse(
            "{\"type\":\"record\",\"name\":\"Person\",\"namespace\":\"Test.AvroMsg\",\"fields\":[{\"name\":\"name\",\"type\":\"string\"},{\"name\":\"age\",\"type\":\"int\"}]}"
        )

[<AutoOpen>]
module internal TestMessage =
    let mkSetter<'T> (name: string) =
        System.Delegate.CreateDelegate(
            typeof<System.Action<Test.AvroMsg.TestMessage, 'T>>,
            typeof<Test.AvroMsg.TestMessage>.GetProperty(name).GetSetMethod()
        )
        :?> System.Action<Test.AvroMsg.TestMessage, 'T>

    let set_id = mkSetter<System.Guid> "id"
    let set_num = mkSetter<int> "num"
    let set_array = mkSetter<string[]> "array"
    let set_optional_num = mkSetter<int option> "optional_num"
    let set_str = mkSetter<string> "str"
    let set_choice = mkSetter<Choice<string, int, bool>> "choice"

    let set_optional_choice =
        mkSetter<Choice<string, int, bool> option> "optional_choice"

    let set_map = mkSetter<Map<string, bool>> "map"
    let set_md5 = mkSetter<Test.AvroMsg.MD5> "md5"
    let set_suit = mkSetter<Test.AvroMsg.Suit> "suit"

    let set_second_suit =
        mkSetter<Choice<string, Test.AvroMsg.Suit> option> "second_suit"

    let set_owner = mkSetter<Test.AvroMsg.Person> "owner"
    let set_contact = mkSetter<Test.AvroMsg.Person option> "contact"

    let set_supervisor =
        mkSetter<Choice<string, Test.AvroMsg.Person> option> "supervisor"

[<CLIMutable>]
type TestMessage =
    {
        id: System.Guid
        num: int
        array: string[]
        optional_num: int option
        str: string
        choice: Choice<string, int, bool>
        optional_choice: Choice<string, int, bool> option
        map: Map<string, bool>
        md5: Test.AvroMsg.MD5
        suit: Test.AvroMsg.Suit
        second_suit: Choice<string, Test.AvroMsg.Suit> option
        ///Who owns this thing anyway?!
        owner: Test.AvroMsg.Person
        contact: Test.AvroMsg.Person option
        supervisor: Choice<string, Test.AvroMsg.Person> option
    }

    interface Avro.Specific.ISpecificRecord with
        [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
        member this.Get(pos: int) =
            match pos with
            | 0 -> box (this.id)
            | 1 -> box (this.num)
            | 2 -> box (this.array)
            | 3 -> this.optional_num |> Option.map (box) |> Option.defaultValue (null)
            | 4 -> box (this.str)
            | 5 ->
                match this.choice with
                | value when box (value) = null -> null
                | Choice1Of3 value -> box (value)
                | Choice2Of3 value -> box (value)
                | Choice3Of3 value -> box (value)
            | 6 ->
                match this.optional_choice with
                | Some (Choice1Of3 value) -> box (value)
                | Some (Choice2Of3 value) -> box (value)
                | Some (Choice3Of3 value) -> box (value)
                | _ -> null
            | 7 when box (this.map) = null -> null
            | 7 -> this.map |> Map.toSeq |> dict |> System.Collections.Generic.Dictionary |> box
            | 8 -> box (this.md5)
            | 9 -> Test.AvroMsg.Suit.ToAvroEnumValue (this.suit) |> box
            | 10 ->
                match this.second_suit with
                | Some (Choice1Of2 value) -> box (value)
                | Some (Choice2Of2 value) -> box (Test.AvroMsg.Suit.ToAvroEnumValue(value))
                | _ -> null
            | 11 -> box (this.owner)
            | 12 -> this.contact |> Option.map (box) |> Option.defaultValue (null)
            | 13 ->
                match this.supervisor with
                | Some (Choice1Of2 value) -> box (value)
                | Some (Choice2Of2 value) -> box (value)
                | _ -> null
            | _ -> raise (Avro.AvroRuntimeException("Bad index " + string pos + " in Get()"))

        [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
        member this.Put(pos: int, value: obj) =
            match pos, value with
            | 0, (:? System.Guid as x) -> set_id.Invoke(this, x)
            | 1, (:? int as x) -> set_num.Invoke(this, x)
            | 2, (:? (string seq) as x) -> set_array.Invoke(this, x |> Array.ofSeq)
            | 3, (:? int as x) -> set_optional_num.Invoke(this, Some(x))
            | 3, _ -> set_optional_num.Invoke(this, None)
            | 4, (:? string as x) -> set_str.Invoke(this, x)
            | 5, (:? string as x) -> set_choice.Invoke(this, (Choice1Of3 x: Choice<string, int, bool>))
            | 5, (:? int as x) -> set_choice.Invoke(this, (Choice2Of3 x: Choice<string, int, bool>))
            | 5, (:? bool as x) -> set_choice.Invoke(this, (Choice3Of3 x: Choice<string, int, bool>))
            | 6, (:? string as x) ->
                set_optional_choice
                    .Invoke(this, (Some(Choice1Of3 x): Choice<string, int, bool> option))
            | 6, (:? int as x) ->
                set_optional_choice
                    .Invoke(this, (Some(Choice2Of3 x): Choice<string, int, bool> option))
            | 6, (:? bool as x) ->
                set_optional_choice
                    .Invoke(this, (Some(Choice3Of3 x): Choice<string, int, bool> option))
            | 6, _ -> set_optional_choice.Invoke(this, None)
            | 7, (:? System.Collections.Generic.IDictionary<string, bool> as x) ->
                set_map.Invoke(this, Map.ofSeq (Seq.map (|KeyValue|) x))
            | 8, (:? Test.AvroMsg.MD5 as x) -> set_md5.Invoke(this, x)
            | 9, (:? int as x) -> set_suit.Invoke(this, Test.AvroMsg.Suit.FromAvroEnumValue(x))
            | 10, (:? string as x) ->
                set_second_suit
                    .Invoke(this, (Some(Choice1Of2 x): Choice<string, Test.AvroMsg.Suit> option))
            | 10, (:? Test.AvroMsg.Suit as x) ->
                set_second_suit
                    .Invoke(this, (Some(Choice2Of2 x): Choice<string, Test.AvroMsg.Suit> option))
            | 10, _ -> set_second_suit.Invoke(this, None)
            | 11, (:? Test.AvroMsg.Person as x) -> set_owner.Invoke(this, x)
            | 12, (:? Test.AvroMsg.Person as x) -> set_contact.Invoke(this, Some(x))
            | 12, _ -> set_contact.Invoke(this, None)
            | 13, (:? string as x) ->
                set_supervisor
                    .Invoke(this, (Some(Choice1Of2 x): Choice<string, Test.AvroMsg.Person> option))
            | 13, (:? Test.AvroMsg.Person as x) ->
                set_supervisor
                    .Invoke(this, (Some(Choice2Of2 x): Choice<string, Test.AvroMsg.Person> option))
            | 13, _ -> set_supervisor.Invoke(this, None)
            | _ -> raise (Avro.AvroRuntimeException("Bad index " + string pos + " in Get()"))

        member this.Schema = TestMessage._SCHEMA

    static member _SCHEMA =
        Avro.Schema.Parse(
            "{\"type\":\"record\",\"name\":\"TestMessage\",\"namespace\":\"Test.AvroMsg\",\"fields\":[{\"name\":\"id\",\"type\":{\"type\":\"string\",\"logicalType\":\"uuid\"}},{\"name\":\"num\",\"type\":\"int\"},{\"name\":\"array\",\"type\":{\"type\":\"array\",\"items\":\"string\"}},{\"name\":\"optional_num\",\"type\":[\"null\",\"int\"]},{\"name\":\"str\",\"type\":\"string\"},{\"name\":\"choice\",\"type\":[\"string\",\"int\",\"boolean\"]},{\"name\":\"optional_choice\",\"type\":[\"null\",\"string\",\"int\",\"boolean\"]},{\"name\":\"map\",\"type\":{\"type\":\"map\",\"values\":\"boolean\"}},{\"name\":\"md5\",\"type\":{\"type\":\"fixed\",\"name\":\"MD5\",\"doc\":\"MD5 Hash sum\",\"namespace\":\"Test.AvroMsg\",\"size\":16}},{\"name\":\"suit\",\"type\":{\"type\":\"enum\",\"name\":\"Suit\",\"doc\":\"Your usual card deck suit\",\"namespace\":\"Test.AvroMsg\",\"symbols\":[\"SPADES\",\"HEARTS\",\"DIAMONDS\",\"CLUBS\"]}},{\"name\":\"second_suit\",\"type\":[\"null\",\"string\",\"Suit\"]},{\"name\":\"owner\",\"doc\":\"Who owns this thing anyway?!\",\"type\":{\"type\":\"record\",\"name\":\"Person\",\"namespace\":\"Test.AvroMsg\",\"fields\":[{\"name\":\"name\",\"type\":\"string\"},{\"name\":\"age\",\"type\":\"int\"}]}},{\"name\":\"contact\",\"type\":[\"null\",\"Person\"]},{\"name\":\"supervisor\",\"type\":[\"null\",\"string\",\"Person\"]}]}"
        )
