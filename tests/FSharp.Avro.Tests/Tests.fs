module Tests

open System
open Avro.Generic
open Avro.Specific
open FSharp.Compiler.Syntax
open Fantomas.Core
open Test.AvroMsg
open Xunit
open FsToolkit.ErrorHandling

let code = """

let a = Array.singleton 42
let b = [| 42 |]

"""


[<Fact>]
let ``My test`` () =
   let input, _ = CodeFormatter.ParseAsync(false, code) |> Async.RunSynchronously |> Array.head

   match input with
    ParsedInput.ImplFile file ->
        printfn "%A" file

[<Fact>]
let ``Test`` () =
    let xs = dict ["a", 1; "b", 2]

    Map.ofSeq(Seq.map (|KeyValue|) xs)

    let (Ok md5) = MD5.Create(Array.replicate 16 0uy)
    let msg =
        { id = Guid.Empty
          num = 42
          array = [||]
          optional_num = Some 12
          str = "todo"
          choice = Choice1Of3 "choice"
          optional_choice = Some (Choice2Of3 11)
          map = Map.empty
          md5 = md5
          suit = Suit.CLUBS }

    let sr = msg :> ISpecificRecord
    sr.Put(1, box 666)
    sr.Put(8, box 0)

    Assert.Equal(msg.num, 666)
    Assert.Equal(msg.suit, Suit.SPADES)
    ()
