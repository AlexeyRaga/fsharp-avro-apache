``` ini

BenchmarkDotNet=v0.12.1, OS=macOS 13.0 (22A380) [Darwin 22.1.0]
Intel Core i7-9750H CPU 2.60GHz, 1 CPU, 12 logical and 6 physical cores
.NET Core SDK=6.0.402
  [Host]     : .NET Core 6.0.10 (CoreCLR 6.0.1022.47605, CoreFX 6.0.1022.47605), X64 RyuJIT DEBUG
  DefaultJob : .NET Core 6.0.10 (CoreCLR 6.0.1022.47605, CoreFX 6.0.1022.47605), X64 RyuJIT


```
|                      Method | boolValue |     Mean |     Error |    StdDev |   Median | Ratio | RatioSD |
|---------------------------- |---------- |---------:|----------:|----------:|---------:|------:|--------:|
|  **&#39;Avro record as F# record&#39;** |     **False** | **5.075 μs** | **0.0982 μs** | **0.1091 μs** | **5.048 μs** |  **3.12** |    **0.15** |
| &#39;Avro record as .NET class&#39; |     False | 1.591 μs | 0.0316 μs | 0.0601 μs | 1.565 μs |  1.00 |    0.00 |
|                             |           |          |           |           |          |       |         |
|  **&#39;Avro record as F# record&#39;** |      **True** | **4.774 μs** | **0.0906 μs** | **0.0757 μs** | **4.784 μs** |  **2.81** |    **0.06** |
| &#39;Avro record as .NET class&#39; |      True | 1.702 μs | 0.0320 μs | 0.0283 μs | 1.696 μs |  1.00 |    0.00 |
