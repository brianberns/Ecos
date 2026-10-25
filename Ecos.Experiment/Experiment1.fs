namespace Ecos.Experiment

open System
open System.IO

open Ecos

/// Experiment 1: binding. A template strand sits in a box with free
/// atoms that can pair with it. How much of the template is paired,
/// as a function of temperature?
module Experiment1 =

    /// Settings for the experiment.
    type Settings =
        {
            /// Temperatures to run at.
            Temperatures : float[]

            /// Bond-type parameters to run with.
            BondTypeParameterMap : BondTypeParameterMap

            /// Number of independent runs (seeds) per temperature.
            NumRuns : int

            /// Sweeps to run before measuring, so that the world
            /// settles at the temperature. (One sweep is one update
            /// per atom.)
            EquilibrationSweeps : int

            /// Number of measurements per run.
            NumSamples : int

            /// Sweeps between measurements.
            SampleInterval : int
        }

    /// Default settings.
    let defaultSettings =
        {
            Temperatures = [| for i in 3 .. 20 -> float i / 10.0 |]   // 0.3 to 2.0
            BondTypeParameterMap =
                Map [
                    Strong, { Energy = 12; PairEnergyMap = Map.empty; Barrier = 6; BarrierStackReduction = 0; StartBarrier = 0; Entropy = 0; HydrolysisRate = 0; PairedHydrolysisRate = 0; BendLimit = Bend.straight; BendStiffness = 0 }
                    Weak, { Energy = 1; PairEnergyMap = Map.empty; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 0; Entropy = 0; HydrolysisRate = 0; PairedHydrolysisRate = 0; BendLimit = Bend.any; BendStiffness = 0 }
                ] |> BondTypeParameterMap.ofMap
            NumRuns = 16
            EquilibrationSweeps = 20000
            NumSamples = 2000
            SampleInterval = 20
        }

    /// Fraction of the given template's atoms that have a weak bond.
    let getOccupancy (template : AtomId[]) (world : World) =
        let weakBondMap = world.BondTypeMap[Weak]
        let nPaired =
            template
                |> Seq.where weakBondMap.ContainsKey
                |> Seq.length
        float nPaired / float template.Length

    /// Result of a single run.
    type private RunResult =
        {
            /// Average occupancy over all measurements.
            Occupancy : float

            /// Average occupancy over the second half of the
            /// measurements minus that over the first half. This is
            /// near zero if the world had settled before measuring.
            Drift : float

            /// Strong bonds formed during the run (including
            /// equilibration).
            StrongBondsFormed : int

            /// Strong bonds broken during the run (including
            /// equilibration).
            StrongBondsBroken : int
        }

    /// Scenario A's world, with the settings' bond parameters, no
    /// peeling, and the given fixed temperature.
    let getScenario settings kT =
        { Scenario.createScenarioA true with
            Schedule = Constant kT }
            |> Scenario.withName $"kT {kT}"
            |> Scenario.mapParameters (fun parms ->
                { parms with
                    BondTypeParameterMap = settings.BondTypeParameterMap
                    PeelBarrier = Double.PositiveInfinity })   // no peeling

    /// The experiment's scenarios: one for each temperature.
    let getScenarios settings =
        settings.Temperatures |> Array.map (getScenario settings)

    /// Runs one world with the given parameters.
    let private runOne settings scenario seed =
        let mutable run = Run.create seed scenario
        let mutable nFormed = 0
        let mutable nBroken = 0

        let onEvent event _ =
            match event with
                | CreateBond (Strong, _, _) -> nFormed <- nFormed + 1
                | BreakBond (Strong, _, _) -> nBroken <- nBroken + 1
                | _ -> ()

        let sweep nSweeps =
            run <- Run.advanceWith onEvent nSweeps run

        sweep settings.EquilibrationSweeps
        let samples =
            Array.init settings.NumSamples (fun _ ->
                sweep settings.SampleInterval
                getOccupancy run.Template run.World)

        let firstHalf, secondHalf =
            Array.splitAt (samples.Length / 2) samples
        {
            Occupancy = Array.average samples
            Drift = Array.average secondHalf - Array.average firstHalf
            StrongBondsFormed = nFormed
            StrongBondsBroken = nBroken
        }

    /// Mean of the given values, and the standard error of that mean.
    let private meanAndStdErr (values : float[]) =
        let mean = Array.average values
        let stdErr =
            if values.Length < 2 then nan
            else
                let variance =
                    (values |> Array.sumBy (fun x -> (x - mean) ** 2.0))
                        / float (values.Length - 1)
                sqrt (variance / float values.Length)
        mean, stdErr

    /// Runs the experiment, prints a table of results, and writes them
    /// to the given CSV file.
    let run (settings : Settings) (csvPath : string) =

            // run every (temperature, seed) combination in parallel
        let results =
            [|
                for kT in settings.Temperatures do
                    for seed in 0 .. settings.NumRuns - 1 do
                        kT, seed
            |]
                |> Array.Parallel.map (fun (kT, seed) ->
                    kT, runOne settings (getScenario settings kT) seed)
                |> Array.groupBy fst
                |> Array.map (fun (kT, group) -> kT, Array.map snd group)
                |> Array.sortBy fst

        printfn "Experiment 1: fraction of template paired, by temperature"
        printfn
            "%d runs per temperature; %d sweeps to settle, then %d measurements %d sweeps apart"
            settings.NumRuns
            settings.EquilibrationSweeps
            settings.NumSamples
            settings.SampleInterval
        printfn ""
        for KeyValue(bondType, bondTypeParms) in BondTypeParameterMap.toMap settings.BondTypeParameterMap do
            printfn
                "%A bonds: energy %g, barrier %g"
                bondType bondTypeParms.Energy bondTypeParms.Barrier
        printfn ""
        printfn "   kT   occupancy     +/-      drift     +/-   strong formed   strong broken"

        use writer = new StreamWriter(csvPath)
        writer.WriteLine(
            "kT,occupancy,occupancy_stderr,drift,drift_stderr,strong_formed,strong_broken")

        for kT, runs in results do
            let occupancy, occupancyErr =
                runs |> Array.map _.Occupancy |> meanAndStdErr
            let drift, driftErr =
                runs |> Array.map _.Drift |> meanAndStdErr
            let formed =
                runs |> Array.averageBy (fun run -> float run.StrongBondsFormed)
            let broken =
                runs |> Array.averageBy (fun run -> float run.StrongBondsBroken)

                // drift well outside its own noise means the world hadn't settled
            let flag = if abs drift > 3.0 * driftErr then " *" else ""

            printfn
                "%5.2f     %5.3f    %5.3f     %6.3f   %5.3f       %7.1f         %7.1f%s"
                kT occupancy occupancyErr drift driftErr formed broken flag
            writer.WriteLine(
                String.Join(
                    ",",
                    [ kT; occupancy; occupancyErr; drift; driftErr; formed; broken ]
                        |> Seq.map (fun (x : float) ->
                            x.ToString(Globalization.CultureInfo.InvariantCulture))))

        printfn ""
        printfn "+/- is the standard error across runs."
        printfn "drift is late occupancy minus early occupancy; * means it hadn't settled."
        printfn "Results written to %s" (Path.GetFullPath csvPath)
