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
            match Array.length value with
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

[<Sealed>]
type Person(name: string, age: int) =
    class
        let mutable __name = name
        let mutable __age = age

        [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
        new () = Person(Unchecked.defaultof<string>, Unchecked.defaultof<int>)

        member this.name = __name
        member this.age = __age

        static member _SCHEMA =
            Avro.Schema.Parse(
                "{\"type\":\"record\",\"name\":\"Person\",\"namespace\":\"Test.AvroMsg\",\"fields\":[{\"name\":\"name\",\"type\":\"string\"},{\"name\":\"age\",\"type\":\"int\"}]}"
            )

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
                | 0, (:? string as x) -> __name <- x
                | 1, (:? int as x) -> __age <- x
                | _ -> raise (Avro.AvroRuntimeException("Bad index " + string pos + " in Get()"))

            member this.Schema = Person._SCHEMA

        interface System.IEquatable<Person> with
            member this.Equals other =
                other.name = this.name && other.age = this.age

        override this.Equals(other) =
            match other with
            | :? Person as x -> (this :> System.IEquatable<Person>).Equals x
            | _ -> false

        override this.GetHashCode() = hash (this.name, this.age)
    end

[<Sealed>]
type TestMessage
    (
        id: System.Guid,
        num: int,
        array: string[],
        optional_num: int option,
        str: string,
        choice: Choice<string, int, bool>,
        optional_choice: Choice<string, int, bool> option,
        map: Map<string, bool>,
        md5: Test.AvroMsg.MD5,
        suit: Test.AvroMsg.Suit,
        owner: Test.AvroMsg.Person,
        contact: Test.AvroMsg.Person option,
        supervisor: Choice<string, Test.AvroMsg.Person> option
    ) =
    class
        let mutable __id = id
        let mutable __num = num
        let mutable __array = array
        let mutable __optional_num = optional_num
        let mutable __str = str
        let mutable __choice = choice
        let mutable __optional_choice = optional_choice
        let mutable __map = map
        let mutable __md5 = md5
        let mutable __suit = suit
        let mutable __owner = owner
        let mutable __contact = contact
        let mutable __supervisor = supervisor

        [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
        new () =
            TestMessage(
                Unchecked.defaultof<System.Guid>,
                Unchecked.defaultof<int>,
                Unchecked.defaultof<string[]>,
                Unchecked.defaultof<int option>,
                Unchecked.defaultof<string>,
                Unchecked.defaultof<Choice<string, int, bool>>,
                Unchecked.defaultof<Choice<string, int, bool> option>,
                Unchecked.defaultof<Map<string, bool>>,
                Unchecked.defaultof<Test.AvroMsg.MD5>,
                Unchecked.defaultof<Test.AvroMsg.Suit>,
                Unchecked.defaultof<Test.AvroMsg.Person>,
                Unchecked.defaultof<Test.AvroMsg.Person option>,
                Unchecked.defaultof<Choice<string, Test.AvroMsg.Person> option>
            )

        member this.id = __id
        member this.num = __num
        member this.array = __array
        member this.optional_num = __optional_num
        member this.str = __str
        member this.choice = __choice
        member this.optional_choice = __optional_choice
        member this.map = __map
        member this.md5 = __md5
        member this.suit = __suit
        ///Who owns this thing anyway?!
        member this.owner = __owner
        member this.contact = __contact
        member this.supervisor = __supervisor

        static member _SCHEMA =
            Avro.Schema.Parse(
                "{\"type\":\"record\",\"name\":\"TestMessage\",\"namespace\":\"Test.AvroMsg\",\"fields\":[{\"name\":\"id\",\"type\":{\"type\":\"string\",\"logicalType\":\"uuid\"}},{\"name\":\"num\",\"type\":\"int\"},{\"name\":\"array\",\"type\":{\"type\":\"array\",\"items\":\"string\"}},{\"name\":\"optional_num\",\"type\":[\"null\",\"int\"]},{\"name\":\"str\",\"type\":\"string\"},{\"name\":\"choice\",\"type\":[\"string\",\"int\",\"boolean\"]},{\"name\":\"optional_choice\",\"type\":[\"null\",\"string\",\"int\",\"boolean\"]},{\"name\":\"map\",\"type\":{\"type\":\"map\",\"values\":\"boolean\"}},{\"name\":\"md5\",\"type\":{\"type\":\"fixed\",\"name\":\"MD5\",\"doc\":\"MD5 Hash sum\",\"namespace\":\"Test.AvroMsg\",\"size\":16}},{\"name\":\"suit\",\"type\":{\"type\":\"enum\",\"name\":\"Suit\",\"doc\":\"Your usual card deck suit\",\"namespace\":\"Test.AvroMsg\",\"symbols\":[\"SPADES\",\"HEARTS\",\"DIAMONDS\",\"CLUBS\"]}},{\"name\":\"owner\",\"doc\":\"Who owns this thing anyway?!\",\"type\":{\"type\":\"record\",\"name\":\"Person\",\"namespace\":\"Test.AvroMsg\",\"fields\":[{\"name\":\"name\",\"type\":\"string\"},{\"name\":\"age\",\"type\":\"int\"}]}},{\"name\":\"contact\",\"type\":[\"null\",\"Person\"]},{\"name\":\"supervisor\",\"type\":[\"null\",\"string\",\"Person\"]}]}"
            )

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
                | 9 -> Test.AvroMsg.Suit.ToAvroEnumValue this.suit |> box
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
                | 0, (:? System.Guid as x) -> __id <- x
                | 1, (:? int as x) -> __num <- x
                | 2, (:? (string seq) as x) -> __array <- x |> Array.ofSeq
                | 3, (:? int as x) -> __optional_num <- Some(x)
                | 3, _ -> __optional_num <- None
                | 4, (:? string as x) -> __str <- x
                | 5, (:? string as x) -> __choice <- (Choice1Of3 x: Choice<string, int, bool>)
                | 5, (:? int as x) -> __choice <- (Choice2Of3 x: Choice<string, int, bool>)
                | 5, (:? bool as x) -> __choice <- (Choice3Of3 x: Choice<string, int, bool>)
                | 6, (:? string as x) -> __optional_choice <- (Some(Choice1Of3 x): Choice<string, int, bool> option)
                | 6, (:? int as x) -> __optional_choice <- (Some(Choice2Of3 x): Choice<string, int, bool> option)
                | 6, (:? bool as x) -> __optional_choice <- (Some(Choice3Of3 x): Choice<string, int, bool> option)
                | 6, _ -> __optional_choice <- None
                | 7, (:? System.Collections.Generic.IDictionary<string, bool> as x) ->
                    __map <- Map.ofSeq (Seq.map (|KeyValue|) x)
                | 8, (:? Test.AvroMsg.MD5 as x) -> __md5 <- x
                | 9, (:? int as x) -> __suit <- Test.AvroMsg.Suit.FromAvroEnumValue x
                | 10, (:? Test.AvroMsg.Person as x) -> __owner <- x
                | 11, (:? Test.AvroMsg.Person as x) -> __contact <- Some(x)
                | 11, _ -> __contact <- None
                | 12, (:? string as x) ->
                    __supervisor <- (Some(Choice1Of2 x): Choice<string, Test.AvroMsg.Person> option)
                | 12, (:? Test.AvroMsg.Person as x) ->
                    __supervisor <- (Some(Choice2Of2 x): Choice<string, Test.AvroMsg.Person> option)
                | 12, _ -> __supervisor <- None
                | _ -> raise (Avro.AvroRuntimeException("Bad index " + string pos + " in Get()"))

            member this.Schema = TestMessage._SCHEMA

        interface System.IEquatable<TestMessage> with
            member this.Equals other =
                other.id
                    = this.id
                    && other.num = this.num
                    && other.array = this.array
                    && other.optional_num = this.optional_num
                    && other.str = this.str
                    && other.choice = this.choice
                    && other.optional_choice = this.optional_choice
                    && other.map = this.map
                    && other.md5 = this.md5
                    && other.suit = this.suit
                    && other.owner = this.owner
                    && other.contact = this.contact
                    && other.supervisor = this.supervisor

        override this.Equals(other) =
            match other with
            | :? TestMessage as x -> (this :> System.IEquatable<TestMessage>).Equals x
            | _ -> false

        override this.GetHashCode() =
            hash (
                this.id,
                this.num,
                this.array,
                this.optional_num,
                this.str,
                this.choice,
                this.optional_choice,
                this.map,
                this.md5,
                this.suit,
                this.owner,
                this.contact,
                this.supervisor
            )
    end
