namespace FSharp.Avro.Bench.Setters

[<Sealed>]
type ClassPerson(name: string, age: int) =
    class
        let mutable __name = name
        let mutable __age = age

        [<CompilerMessage("This method is not intended for use from F#.", 10001, IsError = true, IsHidden = true)>]
        new () = ClassPerson(Unchecked.defaultof<string>, Unchecked.defaultof<int>)

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
                | 0, _ -> __name <- (downcast value)
                | 1, _ -> __age <- (downcast value)
                | _ -> raise (Avro.AvroRuntimeException("Bad index " + string pos + " in Get()"))

            member this.Schema = ClassPerson._SCHEMA

        interface System.IEquatable<ClassPerson> with
            member this.Equals other =
                other.name = this.name && other.age = this.age

        override this.Equals(other) =
            match other with
            | :? ClassPerson as x -> (this :> System.IEquatable<ClassPerson>).Equals x
            | _ -> false

        override this.GetHashCode() = hash (this.name, this.age)
    end

