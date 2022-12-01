[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Avro.Tests.Prelude

open FsUnit.Xunit

let (===) (a:'a) (b:'a) = a |> should equal b
