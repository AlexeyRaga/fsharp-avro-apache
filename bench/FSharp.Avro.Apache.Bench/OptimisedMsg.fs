namespace rec FSharp.Avro.Bench.OptimisedMsg

open System

module internal Opt =
    let mkSetter<'TType, 'TValue> (name : string) =
        let refl = typeof<'TType>.GetProperty(name).GetSetMethod()
        Delegate.CreateDelegate(typeof<Action<'TType, 'TValue>>, refl) :?> Action<'TType, 'TValue>

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
            match Array.length value with
            | 16 -> Ok(MD5 value)
            | _ -> Error "Fixed size value MD5 is required have length 16"
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
        | 0 -> Suit.SPADES
        | 1 -> Suit.HEARTS
        | 2 -> Suit.DIAMONDS
        | 3 -> Suit.CLUBS
        | _ ->
            raise (Avro.AvroRuntimeException("Bad index " + string value + " in Suit.FromAvroEnumValue"))

    ///Only used in Avro serialisation.
    ///Is not intended to be used by in users code.
    static member internal ToAvroEnumValue(value) =
        match value with
        | Suit.SPADES -> "SPADES"
        | Suit.HEARTS -> "HEARTS"
        | Suit.DIAMONDS -> "DIAMONDS"
        | Suit.CLUBS -> "CLUBS"

module private Person =
    let setName = Opt.mkSetter<Person, String> "name"
    let setAge = Opt.mkSetter<Person, Int32> "age"

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
            | _ -> raise (Avro.AvroRuntimeException("Bad index " + string pos + " in Get()"))

        [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
        member this.Put(pos: int, value: obj) =
            match pos, value with
            | 0, (:? string as x) -> Person.setName.Invoke(this, x)
            | 1, (:? int as x) -> Person.setAge.Invoke(this, x)
            | _ -> raise (Avro.AvroRuntimeException("Bad index " + string pos + " in Get()"))

        member this.Schema = Person._SCHEMA

    static member _SCHEMA =
        Avro.Schema.Parse(
            "{\"type\":\"record\",\"name\":\"Person\",\"namespace\":\"Test.AvroMsg\",\"fields\":[{\"name\":\"name\",\"type\":\"string\"},{\"name\":\"age\",\"type\":\"int\"}]}"
        )

module TestMessage =
    let set_id = Opt.mkSetter<TestMessage, System.Guid> "id"
    let set_num = Opt.mkSetter<TestMessage, int> "num"
    let set_array = Opt.mkSetter<TestMessage, string[]> "array"
    let set_optional_num = Opt.mkSetter<TestMessage, int option> "optional_num"
    let set_str = Opt.mkSetter<TestMessage, string> "str"
    let set_choice = Opt.mkSetter<TestMessage, Choice<string, int, bool>> "choice"
    let set_optional_choice = Opt.mkSetter<TestMessage, Choice<string, int, bool> option> "optional_choice"
    let set_map = Opt.mkSetter<TestMessage, Map<string, bool>> "map"
    let set_md5 = Opt.mkSetter<TestMessage, MD5> "md5"
    let set_suit = Opt.mkSetter<TestMessage, Suit> "suit"
    let set_owner = Opt.mkSetter<TestMessage, Person> "owner"
    let set_contact = Opt.mkSetter<TestMessage, Person option> "contact"
    let set_supervisor = Opt.mkSetter<TestMessage, Choice<string, Person> option> "supervisor"

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
        md5: MD5
        suit: Suit
        owner: Person
        contact: Person option
        supervisor: Choice<string, Person> option
    }

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
            | 6 ->
                match this.optional_choice with
                | Some (Choice1Of3 value) -> box value
                | Some (Choice2Of3 value) -> box value
                | Some (Choice3Of3 value) -> box value
                | _ -> null
            | 7 when box this.map = null -> null
            | 7 -> this.map |> Map.toSeq |> dict |> System.Collections.Generic.Dictionary |> box
            | 8 -> box this.md5
            | 9 -> Suit.ToAvroEnumValue this.suit |> box
            | 10 -> box this.owner
            | 11 -> this.contact |> Option.map box |> Option.defaultValue null
            | 12 ->
                match this.supervisor with
                | Some (Choice1Of2 value) -> box value
                | Some (Choice2Of2 value) -> box value
                | _ -> null
            | _ -> raise (Avro.AvroRuntimeException("Bad index " + string pos + " in Get()"))

        [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
        member this.Put(pos: int, value: obj) =
            match pos, value with
            | 0, (:? System.Guid as x) -> TestMessage.set_id.Invoke(this, x)
            | 1, (:? int as x) -> TestMessage.set_num.Invoke(this, x)
            | 2, (:? (string seq) as x) ->
                TestMessage.set_array.Invoke(this, Array.ofSeq x)
            | 3, (:? int as x) ->
                TestMessage.set_optional_num.Invoke(this, Some x)
            | 3, _ -> TestMessage.set_optional_num.Invoke(this, None)
            | 4, (:? string as x) -> TestMessage.set_str.Invoke(this, x)
            | 5, (:? string as x) ->
                TestMessage.set_choice.Invoke(this, Choice1Of3 x)
            | 5, (:? int as x) ->
                TestMessage.set_choice.Invoke(this, Choice2Of3 x)
            | 5, (:? bool as x) ->
                TestMessage.set_choice.Invoke(this, Choice3Of3 x)
            | 6, (:? string as x) ->
                TestMessage.set_optional_choice.Invoke(this, Some(Choice1Of3 x))
            | 6, (:? int as x) ->
                TestMessage.set_optional_choice.Invoke(this, Some(Choice2Of3 x))
            | 6, (:? bool as x) ->
                TestMessage.set_optional_choice.Invoke(this, Some(Choice3Of3 x))
            | 6, _ -> TestMessage.set_optional_choice.Invoke(this, None)
            | 7, (:? System.Collections.Generic.IDictionary<string, bool> as x) ->
                TestMessage.set_map.Invoke(this, Map.ofSeq (Seq.map (|KeyValue|) x))
            | 8, (:? MD5 as x) ->
                TestMessage.set_md5.Invoke(this, x)
            | 9, (:? int as x) ->
                TestMessage.set_suit.Invoke(this, Suit.FromAvroEnumValue x)
            | 10, (:? Person as x) ->
                TestMessage.set_owner.Invoke(this, x)
            | 11, (:? Person as x) ->
                TestMessage.set_contact.Invoke(this, Some x)
            | 11, _ -> TestMessage.set_contact.Invoke(this, None)
            | 12, (:? string as x) ->
                TestMessage.set_supervisor.Invoke(this, Some(Choice1Of2 x))
            | 12, (:? Person as x) ->
                TestMessage.set_supervisor.Invoke(this, Some(Choice2Of2 x))
            | 12, _ -> TestMessage.set_supervisor.Invoke(this, None)
            | _ -> raise (Avro.AvroRuntimeException("Bad index " + string pos + " in Get()"))

        member this.Schema = TestMessage._SCHEMA

    static member _SCHEMA =
        Avro.Schema.Parse(
            "{\"type\":\"record\",\"name\":\"TestMessage\",\"namespace\":\"Test.AvroMsg\",\"fields\":[{\"name\":\"id\",\"type\":{\"type\":\"string\",\"logicalType\":\"uuid\"}},{\"name\":\"num\",\"type\":\"int\"},{\"name\":\"array\",\"type\":{\"type\":\"array\",\"items\":\"string\"}},{\"name\":\"optional_num\",\"type\":[\"null\",\"int\"]},{\"name\":\"str\",\"type\":\"string\"},{\"name\":\"choice\",\"type\":[\"string\",\"int\",\"boolean\"]},{\"name\":\"optional_choice\",\"type\":[\"null\",\"string\",\"int\",\"boolean\"]},{\"name\":\"map\",\"type\":{\"type\":\"map\",\"values\":\"boolean\"}},{\"name\":\"md5\",\"type\":{\"type\":\"fixed\",\"name\":\"MD5\",\"doc\":\"MD5 Hash sum\",\"namespace\":\"Test.AvroMsg\",\"size\":16}},{\"name\":\"suit\",\"type\":{\"type\":\"enum\",\"name\":\"Suit\",\"doc\":\"Your usual card deck suit\",\"namespace\":\"Test.AvroMsg\",\"symbols\":[\"SPADES\",\"HEARTS\",\"DIAMONDS\",\"CLUBS\"]}},{\"name\":\"owner\",\"doc\":\"Who owns this thing anyway?!\",\"type\":{\"type\":\"record\",\"name\":\"Person\",\"namespace\":\"Test.AvroMsg\",\"fields\":[{\"name\":\"name\",\"type\":\"string\"},{\"name\":\"age\",\"type\":\"int\"}]}},{\"name\":\"contact\",\"type\":[\"null\",\"Person\"]},{\"name\":\"supervisor\",\"type\":[\"null\",\"string\",\"Person\"]}]}"
        )
