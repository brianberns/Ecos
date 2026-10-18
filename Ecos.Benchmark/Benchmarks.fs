namespace Ecos 

open System

open BenchmarkDotNet
open BenchmarkDotNet.Attributes

open Ecos.Experiment

type Benchmarks() =

    [<Benchmark>]
    member this.Run() =
        let scenario = Scenario.createScenarioB true
        let schedule =
            scenario.Schedule
                |> Schedule.getPhases
                |> Array.map (fun phase ->
                    { phase with
                        NumSweeps = phase.NumSweeps / 2 })
                |> Cycle
        let run =
            { scenario with Schedule = schedule }
                |> Run.create 0
        (run, Schedule.getPhases schedule)
            ||> Seq.fold (fun run phase ->
                Run.advance phase.NumSweeps run)
