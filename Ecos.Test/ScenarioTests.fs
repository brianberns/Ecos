namespace Ecos.Test

open System
open Xunit
open Ecos
open Ecos.Experiment

/// Tests of scenarios, schedules and runs, which the experiments and
/// the desktop app share.
module ScenarioTests =

    let private fingerprint = RegressionTests.fingerprint

    /// Scenario B's world and parameters, with a short cycle, so that a
    /// test can cross several changes of temperature quickly.
    /// The given schedule's temperature after the given number of
    /// sweeps (at the origin, since these schedules are uniform), and
    /// the number of sweeps until it next changes.
    let private kTAt numSweeps schedule =
        let getTemperature, untilChangeOpt =
            Schedule.getTemperature numSweeps schedule
        getTemperature Point.zero, untilChangeOpt

    let private shortCycle =
        { Scenario.createScenarioB true with
            Schedule =
                Cycle [|
                    { Temperature = 0.3; NumSweeps = 70 }
                    { Temperature = 3.0; NumSweeps = 30 }
                |] }

    [<Fact>]
    let ``Sweeping in pieces matches sweeping all at once`` () =
        let run = Run.create 1 shortCycle
        let parms = Run.getParameters run
        let whole =
            World.sweep parms (Random(5)) 90 run.World
        let pieces =
            let rng = Random(5)
            run.World
                |> World.sweep parms rng 40
                |> World.sweep parms rng 1
                |> World.sweep parms rng 49
        Assert.Equal(fingerprint whole, fingerprint pieces)

    [<Fact>]
    let ``A schedule gives each phase's temperature and the time left in it`` () =
        let schedule =
            Cycle [|
                { Temperature = 1.0; NumSweeps = 3 }
                { Temperature = 2.0; NumSweeps = 2 }
            |]
        let expected =
            [
                0, (1.0, Some 3)
                2, (1.0, Some 1)
                3, (2.0, Some 2)
                4, (2.0, Some 1)
                5, (1.0, Some 3)   // the cycle starts again
                12, (1.0, Some 1)
            ]
        for numSweeps, result in expected do
            Assert.Equal(result, kTAt numSweeps schedule)
        Assert.Equal((0.7, None), kTAt 1234 (Constant 0.7))

    [<Fact>]
    let ``Advancing a run in pieces matches running each phase in turn`` () =

            // oracle: each phase with its own parameters
        let expected =
            let run = Run.create 2 shortCycle
            let mutable world = run.World
            for _ in 1 .. 2 do
                for phase in Schedule.getPhases shortCycle.Schedule do
                    let parms =
                        { shortCycle.Parameters with
                            GetTemperature = Parameters.getUniformTemperature phase.Temperature }
                    world <- World.sweep parms run.Random phase.NumSweeps world
            fingerprint world

            // pieces that straddle the changes of temperature
        let run =
            (Run.create 2 shortCycle, [ 45; 45; 1; 59; 50 ])
                ||> List.fold (fun run n -> Run.advance n run)
        Assert.Equal(200, run.NumSweeps)
        Assert.Equal(expected, fingerprint run.World)

    [<Fact>]
    let ``A run reports the temperature of its schedule`` () =
        let run = Run.create 0 shortCycle
        Assert.Equal(0.3, Run.getTemperature run Point.zero)
        let run = Run.advance 70 run
        Assert.Equal(3.0, Run.getTemperature run Point.zero)
        Assert.Equal(3.0, (Run.getParameters run).GetTemperature Point.zero)
        let run = Run.advance 30 run
        Assert.Equal(0.3, Run.getTemperature run Point.zero)

            // a replaced schedule takes over
        let run = { run with Schedule = Constant 1.25 }
        Assert.Equal(1.25, Run.getTemperature (Run.advance 10 run) Point.zero)

    [<Fact>]
    let ``Runs of the same scenario and seed are the same`` () =
        let runA = Run.create 3 shortCycle
        let runB = Run.create 3 shortCycle
        Assert.Equal(fingerprint runA.World, fingerprint runB.World)
        Assert.Equal<AtomId[]>(runA.Template, runB.Template)

            // and independent: advancing one doesn't disturb the other
        let runA = Run.advance 100 runA
        let runB = Run.advance 100 runB
        Assert.Equal(fingerprint runA.World, fingerprint runB.World)

            // a different seed gives a different template
        let runC = Run.create 4 shortCycle
        let types (run : Run) =
            run.Template |> Array.map (fun id -> run.World.Atoms[id].Type)
        Assert.NotEqual<AtomType[]>(types runA, types runC)

    [<Fact>]
    let ``Every registered scenario is well formed`` () =
        for group in Registry.groups do
            Assert.NotEmpty(group.Scenarios)
            let names = group.Scenarios |> Array.map _.Name
            Assert.Equal(names.Length, (Array.distinct names).Length)
            for scenario in group.Scenarios do
                Assert.True(
                    Double.IsNaN (scenario.Parameters.GetTemperature Point.zero),
                    $"{group.Name}, {scenario.Name}: temperature should come from the schedule")
                match scenario.Schedule with
                    | Cycle phases ->
                        Assert.NotEmpty(phases)
                        for phase in phases do
                            Assert.True(phase.NumSweeps > 0)
                    | Constant _ -> ()

                // building is slow, so build one scenario per group
            let run = Run.create 0 group.Scenarios[group.Scenarios.Length - 1]
            Assert.NotEmpty(run.World.Atoms)
            for id in run.Template do
                Assert.InRange(id, 0, run.World.Atoms.Length - 1)
