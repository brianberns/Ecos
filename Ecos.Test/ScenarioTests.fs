namespace Ecos.Test

open System
open Xunit
open Ecos
open Ecos.Experiment

/// Tests of scenarios, schedules and runs, which the experiments and
/// the desktop app share.
module ScenarioTests =

    let private fingerprint = RegressionTests.fingerprint

    /// The given schedule's temperature after the given number of
    /// sweeps (at the origin, since these schedules are uniform), and
    /// the number of sweeps until it next changes.
    let private kTAt numSweeps schedule =
        let getTemperature, untilChangeOpt =
            Schedule.getTemperature numSweeps schedule
        getTemperature Point.zero, untilChangeOpt

    /// Scenario B's world and parameters, with a short cycle, so that a
    /// test can cross several changes of temperature quickly.
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

    /// A small vent for tests.
    let private vent =
        {
            X = -10
            HotTemperature = 2.0
            ColdTemperature = 0.5
            FloorTemperature = 0.5
            Period = 100
            Width = 20
            ColdSweeps = 70
            HotSweeps = 30
        }

    [<Fact>]
    let ``A vent is hottest at the vent during a pulse, and cold otherwise`` () =
        let at numSweeps x =
            let getTemperature, untilChangeOpt =
                Schedule.getTemperature numSweeps (Vent vent)
            getTemperature (Point.create (x, 7)), untilChangeOpt

            // between pulses: cold everywhere
        for x in [ -40; -10; 0; 30 ] do
            Assert.Equal((0.5, Some 70), at 0 x)
            Assert.Equal((0.5, Some 1), at 69 x)

            // during a pulse: falling linearly with distance either side
            // of the vent, and cold beyond its width
        let expected =
            [
                -10, 2.0
                -5, 1.625
                0, 1.25
                -20, 1.25
                5, 0.875
                10, 0.5
                40, 0.5
                -60, 0.5
            ]
        for x, kT in expected do
            let actual, untilChangeOpt = at 75 x
            Assert.Equal(kT, actual, 10)
            Assert.Equal(Some 25, untilChangeOpt)

            // and again, a cycle later
        Assert.Equal((2.0, Some 30), at 170 -10)
        Assert.Equal((0.5, Some 70), at 200 -10)

            // with a floor above the cold temperature, a pulse falls to the
            // floor beyond the vent's reach, while it is still cold between
            // pulses
        let floored = { vent with FloorTemperature = 0.8 }
        let flooredAt numSweeps x =
            let getTemperature, _ = Schedule.getTemperature numSweeps (Vent floored)
            getTemperature (Point.create (x, 7))
        Assert.Equal(2.0, flooredAt 75 -10, 10)
        Assert.Equal(1.4, flooredAt 75 0, 10)
        Assert.Equal(0.8, flooredAt 75 40, 10)
        Assert.Equal(0.5, flooredAt 0 40, 10)

    [<Fact>]
    let ``Advancing a vent run in pieces matches running each phase in turn`` () =
        let scenario = { shortCycle with Schedule = Vent vent }

            // oracle: each phase with its own temperatures
        let expected =
            let run = Run.create 2 scenario
            let mutable world = run.World
            for _ in 1 .. 2 do
                for getTemperature, numSweeps in
                    [
                        Parameters.getUniformTemperature vent.ColdTemperature, vent.ColdSweeps
                        Vent.getHotTemperature vent, vent.HotSweeps
                    ] do
                    let parms =
                        { scenario.Parameters with
                            GetTemperature = getTemperature }
                    world <- World.sweep parms run.Random numSweeps world
            fingerprint world

            // pieces that straddle the changes of temperature
        let run =
            (Run.create 2 scenario, [ 45; 45; 1; 59; 50 ])
                ||> List.fold (fun run n -> Run.advance n run)
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
                    | Vent vent ->
                        Assert.True(vent.Width > 0)
                        Assert.True(vent.ColdSweeps > 0 && vent.HotSweeps > 0)
                    | Constant _ -> ()

                // building is slow, so build one scenario per group
            let run = Run.create 0 group.Scenarios[group.Scenarios.Length - 1]
            Assert.NotEmpty(run.World.Atoms)

                // every atom is inside the walls
            let walls = run.World.Walls
            if walls.Length > 0 then
                let minX = walls |> Array.map _.X |> Array.min
                let maxX = walls |> Array.map _.X |> Array.max
                let minY = walls |> Array.map _.Y |> Array.min
                let maxY = walls |> Array.map _.Y |> Array.max
                for atom in run.World.Atoms do
                    let loc = atom.Location
                    Assert.True(
                        loc.X > minX && loc.X < maxX && loc.Y > minY && loc.Y < maxY,
                        $"{group.Name}, {group.Scenarios[group.Scenarios.Length - 1].Name}: atom {atom.Id} at {loc} is outside the walls")
            for id in run.Template do
                Assert.InRange(id, 0, run.World.Atoms.Length - 1)

    [<Fact>]
    let ``The longest hairpin stem needs complementary arms around a loop of two`` () =
        let parse (text : string) =
            text
                |> Seq.map (function
                    | 'W' -> AtomType.W | 'X' -> AtomType.X
                    | 'Y' -> AtomType.Y | 'Z' -> AtomType.Z
                    | 'P' -> AtomType.P | 'Q' -> AtomType.Q
                    | c -> failwith $"Unknown atom type: {c}")
                |> Seq.toArray
        let expected =
            [
                "PWWYYXXQ", 3   // P-Q, W-X, W-X around the Y Y loop
                "PWWYXXQ", 2    // a loop of one is too small for the inner W-X
                "PWYXQ", 1      // only P-Q, around a loop of three
                "PWXQ", 1       // P-Q around a loop of two; W-X would have none
                "WWWW", 0       // nothing complementary
                "PWWYYZXQ", 2   // P-Q and W-X, then W-Z doesn't pair
            ]
        for text, stem in expected do
            Assert.True((Experiment16.getMaxStem (parse text) = stem), $"{text}: expected {stem}, got {Experiment16.getMaxStem (parse text)}")

