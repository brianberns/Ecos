namespace Ecos

open System
open BenchmarkDotNet.Running

module Program =

    BenchmarkRunner.Run<Benchmarks>() |> ignore
