# Benchmarks

| Method       | boolValue |       Mean |    Error |   StdDev | Ratio | RatioSD |
|--------------|---------- |-----------:|---------:|---------:|------:|--------:|
| 'C# Classes' |     False |   735.2 ns |  4.11 ns |  3.64 ns |  1.00 |    0.00 |
| 'F# Classes' |     False | 1,201.6 ns | 22.63 ns | 20.06 ns |  1.69 |    0.04 |
| 'F# Records' |     False | 4,478.9 ns | 14.35 ns | 13.42 ns |  6.09 |    0.03 |
|              |           |            |          |          |       |         |
|              |           |            |          |          |       |         |
| 'C# Classes' |      True |   760.3 ns |  1.98 ns |  1.76 ns |  1.00 |    0.00 |
| 'F# Classes' |      True | 1,138.7 ns | 12.60 ns | 11.17 ns |  1.60 |    0.02 |
| 'F# Records' |      True | 4,443.0 ns | 15.16 ns | 14.18 ns |  5.84 |    0.02 |


## Optimisations:

### Cache reflection

This optimisation has a very significant impact and helps bringing `F# Records` case to the same level as `F# Classes`:

```fsharp
    let mkSetter<'TType, 'TValue> (name : string) =
        let refl = typeof<'TType>.GetProperty(name).GetSetMethod()
        Delegate.CreateDelegate(typeof<Action<'TType, 'TValue>>, refl) :?> Action<'TType, 'TValue>

    let nameSetter = mkSetter<OptimisedPerson, String> "name"
    let ageSetter = mkSetter<OptimisedPerson, Int32> "age"
```

The results are:

|                   Method |       Mean |    Error |   StdDev | Ratio | RatioSD |
|------------------------- |-----------:|---------:|---------:|------:|--------:|
|             'C# Classes' |   672.2 ns |  2.02 ns |  1.79 ns |  1.00 |    0.00 |
|             'F# Records' | 4,446.3 ns | 14.99 ns | 14.02 ns |  6.62 |    0.03 |
| 'F# Records (Optimised)' | 1,167.5 ns |  2.53 ns |  2.11 ns |  1.74 |    0.01 |

### Micro-optimise setters

Instead of `x :?> type` and `:? type as x` use `downcast` for primitive cases. This brings `F# Class` to the same performance with `C# Class`.

Example:

```fsharp
member this.Put(pos: int, value: obj) =
    match pos, value with
    | 0, _ -> __name <- (downcast value)
    | 1, _ -> __age <- (downcast value)
    | _ -> raise (Avro.AvroRuntimeException("Bad index " + string pos + " in Get()"))

```
