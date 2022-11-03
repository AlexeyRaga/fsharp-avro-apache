namespace Test.AvroMsg

type MD5 =
    private
    | MD5 of byte[]

    static member SCHEMA =
        "{\"type\":\"fixed\",\"name\":\"MD5\",\"namespace\":\"Test.AvroMsg\",\"size\":16}"

    static member Create(value) =
        match Array.length value with
        | 16 -> Ok value
        | _ -> Error "Fixed size value Test.AvroMsg.MD5 is required have length 16"

    static member FromAvro((value: byte[])) = MD5(value)

[<AutoOpen>]
module MD5 =
    let (|MD5|) (MD5 value) = value
namespace rec Test.AvroMsg

[<RequireQualifiedAccess>]
type Suit =
    | SPADES
    | HEARTS
    | DIAMONDS
    | CLUBS

    static member SCHEMA =
        "{\"type\":\"enum\",\"name\":\"Suit\",\"namespace\":\"Test.AvroMsg\",\"symbols\":[\"SPADES\",\"HEARTS\",\"DIAMONDS\",\"CLUBS\"]}"

    static member FromAvro((value: int)) =
        match value with
        | 0 -> Test.AvroMsg.Suit.SPADES
        | 1 -> Test.AvroMsg.Suit.HEARTS
        | 2 -> Test.AvroMsg.Suit.DIAMONDS
        | 3 -> Test.AvroMsg.Suit.CLUBS
        | _ -> failwith "Invalid value for enum Test.AvroMsg.Suit"
namespace rec Test.AvroMsg

[<Sealed>]
type TestMessage
    (
        id: System.Guid,
        num: int option,
        str: string,
        choice: Choice<string, int, bool>,
        optional_choice: Choice<string, int, bool> option,
        map: Map<string, bool>,
        md5: Test.AvroMsg.MD5,
        suit: Test.AvroMsg.Suit
    ) =
    class
        let mutable __id = id
        let mutable __num = num
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
                Unchecked.defaultof<int option>,
                Unchecked.defaultof<string>,
                Unchecked.defaultof<Choice<string, int, bool>>,
                Unchecked.defaultof<Choice<string, int, bool> option>,
                Unchecked.defaultof<Map<string, bool>>,
                Unchecked.defaultof<Test.AvroMsg.MD5>,
                Unchecked.defaultof<Test.AvroMsg.Suit>
            )

        member this.id
            with private set (value) = __id <- value
            and get () = __id

        member this.num
            with private set (value) = __num <- value
            and get () = __num

        member this.str
            with private set (value) = __str <- value
            and get () = __str

        member this.choice
            with private set (value) = __choice <- value
            and get () = __choice

        member this.optional_choice
            with private set (value) = __optional_choice <- value
            and get () = __optional_choice

        member this.map
            with private set (value) = __map <- value
            and get () = __map

        member this.md5
            with private set (value) = __md5 <- value
            and get () = __md5

        member this.suit
            with private set (value) = __suit <- value
            and get () = __suit

        static member SCHEMA =
            "{\"type\":\"record\",\"name\":\"TestMessage\",\"namespace\":\"Test.AvroMsg\",\"fields\":[{\"name\":\"id\",\"type\":{\"type\":\"string\",\"logicalType\":\"uuid\"}},{\"name\":\"num\",\"type\":[\"null\",\"int\"]},{\"name\":\"str\",\"type\":\"string\"},{\"name\":\"choice\",\"type\":[\"string\",\"int\",\"boolean\"]},{\"name\":\"optional_choice\",\"type\":[\"null\",\"string\",\"int\",\"boolean\"]},{\"name\":\"map\",\"type\":{\"type\":\"map\",\"values\":\"boolean\"}},{\"name\":\"md5\",\"type\":{\"type\":\"fixed\",\"name\":\"MD5\",\"namespace\":\"Test.AvroMsg\",\"size\":16}},{\"name\":\"suit\",\"type\":{\"type\":\"enum\",\"name\":\"Suit\",\"namespace\":\"Test.AvroMsg\",\"symbols\":[\"SPADES\",\"HEARTS\",\"DIAMONDS\",\"CLUBS\"]}}]}"

        interface Avro.Specific.ISpecificRecord with
            [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
            member this.Get(pos: int) = ()

            [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
            member this.Put(pos: int, value: obj) = ()

            member this.Schema = Avro.Schema.Parse(TestMessage.SCHEMA)

        interface System.IEquatable<TestMessage> with
            member this.Equals other =
                other.id
                    = this.id
                    && other.num = this.num
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
            hash (this.id, this.num, this.str, this.choice, this.optional_choice, this.map, this.md5, this.suit)
    end
