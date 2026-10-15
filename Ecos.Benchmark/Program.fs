namespace Ecos

open System
open BenchmarkDotNet.Running

module Program =

    let runDirect () =
        for _ in [1 .. 2] do
            let stopwatch = Diagnostics.Stopwatch.StartNew()
            Benchmarks().Run()
            printfn $"{stopwatch.Elapsed}"

    let runBenchmark () =
        BenchmarkRunner.Run<Benchmarks>() |> ignore

    // runDirect ()
    runBenchmark ()
