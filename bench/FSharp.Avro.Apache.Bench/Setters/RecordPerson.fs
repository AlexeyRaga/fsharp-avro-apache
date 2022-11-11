namespace FSharp.Avro.Bench.Setters

[<CLIMutable>]
type RecordPerson =
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
            | 0, (:? string as x) -> this.GetType().GetProperty("name").SetMethod.Invoke (this, [| x |]) |> ignore
            | 1, (:? int as x) -> this.GetType().GetProperty("age").SetMethod.Invoke (this, [| x |]) |> ignore
            | _ -> raise (Avro.AvroRuntimeException("Bad index " + string pos + " in Get()"))

        member this.Schema = RecordPerson._SCHEMA

    static member _SCHEMA =
        Avro.Schema.Parse(
            "{\"type\":\"record\",\"name\":\"Person\",\"namespace\":\"Test.AvroMsg\",\"fields\":[{\"name\":\"name\",\"type\":\"string\"},{\"name\":\"age\",\"type\":\"int\"}]}"
        )
