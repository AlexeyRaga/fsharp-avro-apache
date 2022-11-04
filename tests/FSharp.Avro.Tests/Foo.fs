namespace Test.AvroMsg

type MD5 private (value: byte[]) =
    class
        inherit Avro.Specific.SpecificFixed(uint 16)
        do base.Value <- value

        [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
        new () = MD5(Unchecked.defaultof<byte[]>)

        override this.Schema = Avro.Schema.Parse(MD5.SCHEMA)

        static member SCHEMA =
            "{\"type\":\"fixed\",\"name\":\"MD5\",\"namespace\":\"Test.AvroMsg\",\"size\":16}"

        static member Create(value) =
            match Array.length value with
            | 16 -> Ok(MD5 value)
            | _ -> Error "Fixed size value Test.AvroMsg.MD5 is required have length 16"
    end

[<AutoOpen>]
module MD5 =
    let (|MD5|) (value: MD5) = value.Value
