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
