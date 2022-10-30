module Tests

open System
open Avro.Specific
open Foo
open Xunit
open FsToolkit.ErrorHandling

type Bar = Bar of string

let (|Bar|) (Bar x) = x



[<CLIMutable>]
type Foo =
    { Name : string
      Age : int }

[<Fact>]
let ``My test`` () =
    let a = Bar("foo")
    let b = Bar("foo")

    match a with
    | Bar x -> Assert.Equal(x, "foo")

    let res = a = b
    Assert.Equal(res, true)
