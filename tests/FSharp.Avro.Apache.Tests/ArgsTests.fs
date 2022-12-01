module FSharp.Avro.Tests.ArgsTests

open FSharp.Avro.Codegen
open Xunit

let baseArgs =
    [| "--schema-file"
       "/tmp/fake.avsc"
       "--output"
       "/tmp/gen"
    |]

[<Fact>]
let ``Should not fail when namespace mapping is not provided``() =
    let opts = Program.parseOptions baseArgs
    opts.Parameters.NamespaceMapping === Map.empty

[<Fact>]
let ``Should parse namespace mapping``() =
    let args = baseArgs |> Array.append [| "--namespace"; "foo:bar" |]
    let opts = Program.parseOptions args
    opts.Parameters.NamespaceMapping === Map.ofSeq ["foo", "bar"]

[<Fact>]
let ``Should parse multiple namespace mappings``() =
    let args = baseArgs |> Array.append [| "--namespace"; "foo:bar"; "--namespace"; "boo:buzz" |]
    let opts = Program.parseOptions args
    opts.Parameters.NamespaceMapping === Map.ofSeq ["foo", "bar"; "boo", "buzz"]

[<Fact>]
let ``Should ignore wrong mappings``() =
    let args = baseArgs |> Array.append [| "--namespace"; "foo"; "--namespace"; "bar:buzz:goo" |]
    let opts = Program.parseOptions args
    opts.Parameters.NamespaceMapping === Map.empty
