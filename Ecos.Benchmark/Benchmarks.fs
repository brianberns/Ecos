namespace Ecos 

open System

open BenchmarkDotNet
open BenchmarkDotNet.Attributes

open Ecos.Experiment

type Benchmarks() =

    let runPhase (parms : Parameters) rng phase world =
        let parms =
            { parms with Temperature = phase.Temperature }
        World.sweep parms rng phase.NumSweeps world

    let runCycle parms rng phases world =
        (world, phases)
            ||> Seq.fold (fun world phase ->
                runPhase parms rng phase world)

    let run parms rng phases world nCycles =
        (world, [1 .. nCycles])
            ||> Seq.fold (fun world _ ->
                runCycle parms rng phases world)

    [<Benchmark>]
    member this.Run() =
        let scenario = Scenario.createScenarioB true 0
        let phases =
            match scenario.TemperatureType with
                | Adjustable _ -> failwith "Not supported"
                | Cycle phases ->
                    Array.map (fun phase ->
                        { phase with
                            NumSweeps = phase.NumSweeps / 10 })
                        phases
        run
            scenario.Parameters
            scenario.Random
            phases
            scenario.World
            1
            |> ignore
