namespace rec FSharp.Avro.Bench.Corpus.AvroRecordClass

type MD5 private (value: byte[]) =
    class
        inherit Avro.Specific.SpecificFixed(uint 16)
        do base.Value <- value

        [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
        new () = MD5 Unchecked.defaultof<byte[]>

        override this.Schema = Avro.Schema.Parse(MD5.SCHEMA)

        static member SCHEMA =
            "{\"type\":\"fixed\",\"name\":\"MD5\",\"namespace\":\"FSharp.Avro.Bench.Corpus.AvroRecordClass\",\"size\":16}"

        static member Create(value) =
            match Array.length value with
            | 16 -> Ok(MD5 value)
            | _ -> Error "Fixed size value FSharp.Avro.Bench.Corpus.AvroRecordClass.MD5 is required have length 16"
    end

[<AutoOpen>]
module MD5 =
    let (|MD5|) (value: MD5) = value.Value

[<RequireQualifiedAccess>]
type Suit =
    | SPADES
    | HEARTS
    | DIAMONDS
    | CLUBS

    static member SCHEMA =
        "{\"type\":\"enum\",\"name\":\"Suit\",\"namespace\":\"FSharp.Avro.Bench.Corpus.AvroRecordClass\",\"symbols\":[\"SPADES\",\"HEARTS\",\"DIAMONDS\",\"CLUBS\"]}"

    static member FromInt(value: int) =
        match value with
        | 0 -> FSharp.Avro.Bench.Corpus.AvroRecordClass.Suit.SPADES
        | 1 -> FSharp.Avro.Bench.Corpus.AvroRecordClass.Suit.HEARTS
        | 2 -> FSharp.Avro.Bench.Corpus.AvroRecordClass.Suit.DIAMONDS
        | 3 -> FSharp.Avro.Bench.Corpus.AvroRecordClass.Suit.CLUBS
        | _ -> failwith "Invalid value for enum FSharp.Avro.Bench.Corpus.AvroRecordClass.Suit"

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
        md5: MD5,
        suit: Suit
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
                Unchecked.defaultof<MD5>,
                Unchecked.defaultof<Suit>
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

        static member SCHEMA =
            "{\"type\":\"record\",\"name\":\"TestMessage\",\"namespace\":\"Test.AvroMsg\",\"fields\":[{\"name\":\"id\",\"type\":{\"type\":\"string\",\"logicalType\":\"uuid\"}},{\"name\":\"num\",\"type\":\"int\"},{\"name\":\"array\",\"type\":{\"type\":\"array\",\"items\":\"string\"}},{\"name\":\"optional_num\",\"type\":[\"null\",\"int\"]},{\"name\":\"str\",\"type\":\"string\"},{\"name\":\"choice\",\"type\":[\"string\",\"int\",\"boolean\"]},{\"name\":\"optional_choice\",\"type\":[\"null\",\"string\",\"int\",\"boolean\"]},{\"name\":\"map\",\"type\":{\"type\":\"map\",\"values\":\"boolean\"}},{\"name\":\"md5\",\"type\":{\"type\":\"fixed\",\"name\":\"MD5\",\"namespace\":\"Test.AvroMsg\",\"size\":16}},{\"name\":\"suit\",\"type\":{\"type\":\"enum\",\"name\":\"Suit\",\"namespace\":\"Test.AvroMsg\",\"symbols\":[\"SPADES\",\"HEARTS\",\"DIAMONDS\",\"CLUBS\"]}}]}"

        interface Avro.Specific.ISpecificRecord with
            [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
            member this.Get(pos: int) = ()

            [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
            member this.Put(pos: int, value: obj) =
                match pos, value with
                | 0, (:? System.Guid as x) -> __id <- x
                | 1, (:? int as x) -> __num <- x
                | 2, (:? (string[]) as x) -> __array <- x
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
                | 8, (:? MD5 as x) -> __md5 <- x
                | 9, (:? int as x) -> __suit <- Suit.FromInt x

            member this.Schema = Avro.Schema.Parse(TestMessage.SCHEMA)

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
                this.suit
            )
    end


