// For more information see https://aka.ms/fsharp-console-apps

open FSharp.Avro.Codegen


[<EntryPoint>]
let main args =

    printfn "Hello from F#"

    let filename = "/Users/alexey/src/test/FSharp.Avro/corpus/simple.avsc"
    let res = AvroGenerator.GenerateAsync filename |> Async.RunSynchronously

    printfn $"{res}"
    0
