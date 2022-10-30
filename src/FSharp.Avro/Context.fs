namespace rec FSharp.Avro

open System
open System.Collections.Concurrent
open System.Collections.Generic

type FromAvro<'T> =
    abstract member Decode: obj -> Result<'T, string>


type Decoder =
    static member int32(value: byte array) = 42
    static member int64(value: byte array) = 43L
    static member string(value: byte array) = "hey"

type Address =
    { street: string
      city: string
      region: string option
      country: string }
    
module Address =
    let fromAvro (value : byte array) = failwith "todo"

type School = { name: string; address: Address }

module Decoders =

    type Int32 with
        static member Foo() = ()

    type Int32 with
        static member Bar() = ()

    let private buildDecoder<'T> (f: byte array -> Result<'T, string>) =
        KeyValuePair(typeof<'T>, f >> Result.map box)


    let private decoders =
        ConcurrentDictionary<Type, byte array -> Result<obj, string>>(
            [ buildDecoder<int> (fun _ -> Result.Ok 42)
              buildDecoder<string> (fun _ -> Result.Ok "hey")
              buildDecoder<bool> (fun _ -> Result.Ok true) ]
        )

    let Decode<'T> (value: byte array) =
        match decoders.TryGetValue typeof<'T> with
        | true, f -> f value |> Result.map (fun x -> x :?> 'T)
        | false, _ -> Result.Error "Nooo"


type FromAvro =
    static member DecodeInt(value: byte array) = 0
    static member DecodeString(value: byte array) = 0

// static member DecodeRecord (val)
