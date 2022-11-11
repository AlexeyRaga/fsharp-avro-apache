namespace rec FSharp.Avro.Bench.Setters

open System

module internal Opt =
    let mkSetter<'TType, 'TValue> (name : string) =
        let refl = typeof<'TType>.GetProperty(name).GetSetMethod()
        Delegate.CreateDelegate(typeof<Action<'TType, 'TValue>>, refl) :?> Action<'TType, 'TValue>

    let nameSetter = mkSetter<OptimisedPerson, String> "name"

    let ageSetter = mkSetter<OptimisedPerson, Int32> "age"

[<CLIMutable>]
type OptimisedPerson =
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
            | 0, _ -> Opt.nameSetter.Invoke(this, downcast value)  //(this, x) // this x |> ignore
            | 1, _ -> Opt.ageSetter.Invoke(this, downcast value)
            | _ -> raise (Avro.AvroRuntimeException("Bad index " + string pos + " in Get()"))

        member this.Schema = RecordPerson._SCHEMA

    static member _SCHEMA =
        Avro.Schema.Parse(
            "{\"type\":\"record\",\"name\":\"Person\",\"namespace\":\"Test.AvroMsg\",\"fields\":[{\"name\":\"name\",\"type\":\"string\"},{\"name\":\"age\",\"type\":\"int\"}]}"
        )
