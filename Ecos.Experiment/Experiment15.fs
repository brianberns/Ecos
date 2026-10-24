namespace Ecos.Experiment

open System
open System.IO
open System.Threading

open Ecos
open Ecos.Experiment.Experiment3

/// Experiment 15: hairpins. A single self-complementary strand, capped
/// P W...W Y Y X...X Q, sits alone in a small world at a fixed cold
/// temperature, with no free atoms. Its two arms (all W, all X) can pair
/// with each other only if the strand folds back on itself, through the
/// Y Y loop, which needs two sharp bends. How often is it folded, for
/// each limit on bending and each cost of bending?
///
/// The strand starts straight. Any weak bond in the world is between
/// the strand's own atoms, so the number of weak bonds measures how far
/// it is zipped up.
module Experiment15 =

    /// Settings for the experiment.
    type Settings =
        {
            /// Temperatures to run at.
            Temperatures : float[]

            /// Bend limits and stiffnesses to compare.
            Bends : (Rational * float)[]

            /// Number of coding atoms in each arm.
            ArmLength : int

            /// Number of independent runs (seeds) of each combination.
            NumRuns : int

            /// Sweeps before sampling starts.
            BurnIn : int

            /// Sweeps between samples.
            SampleInterval : int

            /// Number of samples per run.
            NumSamples : int
        }

    /// Default settings.
    let defaultSettings =
        {
            Temperatures = [| 0.3; 0.5 |]
            Bends =
                [|
                    Bend.straight, 0.0
                    for limit in [ Rational.create -1 2; Bend.rightAngle ] do
                        for kappa in [ 0.0; 0.5; 1.0; 1.25; 1.5; 1.75; 2.0; 3.0 ] do
                            limit, kappa
                |]
            ArmLength = 6
            NumRuns = 8
            BurnIn = 50_000
            SampleInterval = 1_000
            NumSamples = 150
        }

    /// Scenario C's parameters, with the given bend limit and stiffness
    /// for strong bonds, at the given temperature.
    let private getParameters (limit, kappa) kT =
        { (Scenario.createScenarioC false |> Scenario.withBend Strong limit kappa).Parameters with
            GetTemperature = Parameters.getUniformTemperature kT }

    /// A small world holding the strand, straight.
    let private createWorld settings parms =
        let types =
            [|
                AtomType.P
                yield! Array.replicate settings.ArmLength AtomType.W
                AtomType.Y
                AtomType.Y
                yield! Array.replicate settings.ArmLength AtomType.X
                AtomType.Q
            |]
        World.createSquare 60
            |> Build.tryAddStrand parms (-types.Length) 0 types
            |> Option.get

    /// Number of weak bonds in the given world.
    let private countWeakBonds (world : World) =
        let weak = world.BondTypeMap[Weak]
        (world.Atoms |> Array.sumBy (fun atom -> (BondMap.getBonds atom.Id weak).Count)) / 2

    /// Runs one combination with one seed. Answers the number of weak
    /// bonds in each sample.
    let private runOne settings bend kT seed =
        let parms = getParameters bend kT
        let rng = Random(seed)
        let mutable world =
            createWorld settings parms
                |> World.sweep parms rng settings.BurnIn
        [|
            for _ in 1 .. settings.NumSamples do
                world <- World.sweep parms rng settings.SampleInterval world
                countWeakBonds world
        |]

    /// Runs the experiment, prints a table of results, and writes them
    /// to the given CSV file.
    let run (settings : Settings) (csvPath : string) =

        let combos =
            [|
                for bend in settings.Bends do
                    for kT in settings.Temperatures do
                        for seed in 0 .. settings.NumRuns - 1 do
                            bend, kT, seed
            |]

            // run every combination in parallel
        let mutable nDone = 0
        let results =
            combos
                |> Array.Parallel.map (fun (bend, kT, seed) ->
                    let samples = runOne settings bend kT seed
                    let n = Interlocked.Increment(&nDone)
                    if n % 20 = 0 then
                        eprintfn "Finished run %d of %d" n combos.Length
                    (bend, kT), samples)
                |> Array.groupBy fst
                |> Array.map (fun (key, group) ->
                    key, group |> Array.collect snd)
                |> Map

        let half = settings.ArmLength / 2
        printfn "Experiment 15: hairpins of a P W%d Y Y X%d Q strand" settings.ArmLength settings.ArmLength
        printfn
            "%d runs of each combination, alone in a small world; %d samples per run, %d sweeps apart, after %d sweeps"
            settings.NumRuns settings.NumSamples settings.SampleInterval settings.BurnIn
        printParameters (getParameters (Bend.straight, 0.0) nan)
        printfn ""
        printfn "  limit (cos <=)  kappa   kT   mean weak   folded   zipped"

        use writer = new StreamWriter(csvPath)
        writer.WriteLine("limit,kappa,kT,mean_weak,folded,zipped")
        let invariant (x : float) = x.ToString(Globalization.CultureInfo.InvariantCulture)
        for (limit, kappa as bend) in settings.Bends do
            let n, d = limit.Numerator, limit.Denominator
            for kT in settings.Temperatures do
                let samples = results[bend, kT]
                let mean = samples |> Array.averageBy float
                let folded = samples |> Array.averageBy (fun k -> if k >= half then 1.0 else 0.0)
                let zipped = samples |> Array.averageBy (fun k -> if k >= settings.ArmLength then 1.0 else 0.0)
                printfn "  %6s %7.2f  %5.2f %5.2f   %9.2f   %6.2f   %6.2f"
                    $"{n}/{d}" (float n / float d) kappa kT mean folded zipped
                writer.WriteLine(
                    String.Join(",", [ $"{n}/{d}"; invariant kappa; invariant kT; invariant mean; invariant folded; invariant zipped ]))

        printfn ""
        printfn "mean weak: mean number of weak bonds (all within the strand)."
        printfn "folded: fraction of samples with at least %d; zipped: with at least %d (every arm atom paired)." half settings.ArmLength
        printfn "Results written to %s" (Path.GetFullPath csvPath)
