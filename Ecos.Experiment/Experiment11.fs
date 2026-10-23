namespace Ecos.Experiment

open System
open System.IO
open System.Threading

open Ecos
open Ecos.Experiment.Experiment3

/// Experiment 11: melting. A ten-letter template and its copy, already
/// paired, sit alone in a small world at a fixed temperature, with no free atoms.
/// How soon do they come apart? Templates of all W (paired with all X)
/// and of all Y (paired with all Z) are compared, with the Y-Z bond
/// stronger than the W-X one. If their melting temperatures differ
/// enough, a vent could favor each kind at its own distance (see
/// experiment 10).
///
/// The strands count as separated the first time they have no weak
/// bonds between them and no atom of one is within reach of a weak bond
/// of any atom of the other.
module Experiment11 =

    /// Settings for the experiment.
    type Settings =
        {
            /// Temperatures to run at.
            Temperatures : float[]

            /// Energy of the weak Y-Z bond. W-X keeps the default weak
            /// bond energy.
            YZEnergy : float

            /// Number of independent runs (seeds) of each duplex at each
            /// temperature.
            NumRuns : int

            /// A run stops after this many sweeps, if the strands haven't
            /// separated by then.
            MaxSweeps : int

            /// Sweeps between checks for separation.
            CheckInterval : int

            /// Shorter time limit to report as well: the length of a
            /// short hot phase.
            ShortSweeps : int
        }

    /// Default settings. The two time limits are the hot phases of
    /// scenarios B and C.
    let defaultSettings =
        {
            Temperatures = [| for i in 2 .. 12 -> float i / 4.0 |]   // 0.5 to 3.0
            YZEnergy = 3.0
            NumRuns = 24
            MaxSweeps = 100_000
            CheckInterval = 100
            ShortSweeps = 10_000
        }

    /// Number of coding atoms in each strand.
    let private nCoding = 10

    /// Scenario C's parameters (nothing joins without a template, and
    /// caps never come off), with the given Y-Z energy, at the given
    /// temperature.
    let private getParameters settings kT =
        let scenario =
            Scenario.createScenarioC false
                |> Scenario.withPairEnergy
                    Weak AtomType.Y AtomType.Z settings.YZEnergy
        { scenario.Parameters with
            GetTemperature = Parameters.getUniformTemperature kT }

    /// A small world holding a template of the given coding type, capped P...Q,
    /// with its complement paired along one side. Answers the world and
    /// the atom IDs of the two strands.
    let private createDuplex codingType =
        let templateTypes =
            [|
                AtomType.P
                yield! Array.replicate nCoding codingType
                AtomType.Q
            |]
        let addRow y (types : AtomType[]) world =
            (world, Array.indexed types)
                ||> Array.fold (fun world (i, atomType) ->
                    let loc = Point.create (2 * i - nCoding - 2, y)
                    let world, atom =
                        World.tryAddAtom atomType loc world |> Option.get
                    if i > 0 then
                        World.addStrongBond (atom.Id - 1) atom.Id world
                    else world)
        let world =
            World.createSquare 60
                |> addRow 0 templateTypes
                |> addRow 2 (Array.map complement templateTypes)
        let n = templateTypes.Length
        let world =
            (world, [ 0 .. n - 1 ])
                ||> Seq.fold (fun world i ->
                    World.addWeakBond i (i + n) world)
        world, [| 0 .. n - 1 |], [| n .. 2 * n - 1 |]

    /// Whether the given strands have come apart: no weak bonds between
    /// them, and no atom of one within weak bond reach of the other.
    let private isSeparated (world : World) (idsA : AtomId[]) (idsB : AtomId[]) =
        let weak = world.BondTypeMap[Weak]
        let setB = set idsB
        let noBonds =
            idsA
                |> Array.forall (fun id ->
                    BondMap.getBonds id weak
                        |> Set.intersect setB
                        |> Set.isEmpty)
        let apart =
            idsA
                |> Array.forall (fun idA ->
                    let a = world.Atoms[idA].Location
                    idsB
                        |> Array.forall (fun idB ->
                            let b = world.Atoms[idB].Location
                            let dx, dy = float (a.X - b.X), float (a.Y - b.Y)
                            dx * dx + dy * dy > 9.0))   // weak bonds reach at most sqrt 8
        noBonds && apart

    /// Runs one duplex at the given temperature. Answers the number of
    /// sweeps it took to separate, if it did.
    let private runOne settings codingType kT seed =
        let parms = getParameters settings kT
        let rng = Random(seed)
        let world, idsA, idsB = createDuplex codingType
        let rec loop nSweeps world =
            if nSweeps >= settings.MaxSweeps then None
            else
                let world = World.sweep parms rng settings.CheckInterval world
                let nSweeps = nSweeps + settings.CheckInterval
                if isSeparated world idsA idsB then Some nSweeps
                else loop nSweeps world
        loop 0 world

    /// Runs the experiment, prints a table of results, and writes them
    /// to the given CSV file.
    let run (settings : Settings) (csvPath : string) =

        let kinds = [| "W/X", AtomType.W; "Y/Z", AtomType.Y |]
        let combos =
            [|
                for _, codingType in kinds do
                    for kT in settings.Temperatures do
                        for seed in 0 .. settings.NumRuns - 1 do
                            codingType, kT, seed
            |]

            // run every combination in parallel
        let mutable nDone = 0
        let results =
            combos
                |> Array.Parallel.map (fun (codingType, kT, seed) ->
                    let result = runOne settings codingType kT seed
                    let n = Interlocked.Increment(&nDone)
                    if n % 50 = 0 then
                        eprintfn "Finished run %d of %d" n combos.Length
                    (codingType, kT), result)
                |> Array.groupBy fst
                |> Array.map (fun (key, group) -> key, Array.map snd group)
                |> Map

        let parms = getParameters settings nan
        printfn "Experiment 11: melting of ten-letter duplexes"
        printfn
            "%d runs of each duplex at each temperature, alone in a small world; each run stops when the strands separate, or after %d sweeps"
            settings.NumRuns settings.MaxSweeps
        printParameters parms
        printfn "Weak bond energies: W-X %g, Y-Z %g" parms.BondParameterMap[Weak].Energy settings.YZEnergy
        printfn ""
        printfn "                |----------- W/X -----------|   |----------- Y/Z -----------|"
        printfn "   kT    within %6d %7d   median     within %6d %7d   median" settings.ShortSweeps settings.MaxSweeps settings.ShortSweeps settings.MaxSweeps

        use writer = new StreamWriter(csvPath)
        writer.WriteLine("kind,kT,separated_short,separated_max,median_sweeps")

        /// Fraction separated within the given time, and the median time
        /// to separate (if at least half did).
        let summarize (runs : int option[]) =
            let fraction limit =
                runs
                    |> Array.averageBy (function
                        | Some n when n <= limit -> 1.0
                        | _ -> 0.0)
            let times =
                runs
                    |> Array.map (Option.defaultValue Int32.MaxValue)
                    |> Array.sort
            let median = times[times.Length / 2]
            fraction settings.ShortSweeps, fraction settings.MaxSweeps, median

        for kT in settings.Temperatures do
            let cells =
                [
                    for name, codingType in kinds do
                        let short, max, median = summarize results[codingType, kT]
                        let medianText =
                            if median = Int32.MaxValue then "      -"
                            else sprintf "%7d" median
                        writer.WriteLine(
                            String.Join(
                                ",",
                                [
                                    name
                                    kT.ToString(Globalization.CultureInfo.InvariantCulture)
                                    short.ToString(Globalization.CultureInfo.InvariantCulture)
                                    max.ToString(Globalization.CultureInfo.InvariantCulture)
                                    (if median = Int32.MaxValue then "" else string median)
                                ]))
                        sprintf "%13.2f %7.2f  %s" short max medianText
                ]
            printfn "%5.2f  %s     %s" kT cells[0] cells[1]

        printfn ""
        printfn "within: fraction of runs whose strands separated within that many sweeps."
        printfn "median: median sweeps to separate; - if fewer than half separated within %d." settings.MaxSweeps
        printfn "Results written to %s" (Path.GetFullPath csvPath)
