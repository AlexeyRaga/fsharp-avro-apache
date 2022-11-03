module Tests

open System
open Avro.Generic
open Avro.Specific
open Foo
open Xunit
open FsToolkit.ErrorHandling


type Bar =
    Bar of string

let (|Bar|) (Bar x) = x

type Person(name : string, age : int) =
    /// Full name
    member val Name = name with get, set
    /// Age in years
    member val Age = age with get, set

[<CLIMutable>]
type Foo =
    { Name : string
      Age : float32 }

[<Fact>]
let ``My test`` () =
    let (g : obj) = null

    let (u : Option<int []>) = g |> Option.ofObj  |> Option.map (fun x -> x :?> int [])
    Assert.Equal(u, None)
