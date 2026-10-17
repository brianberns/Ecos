namespace Ecos.Experiment

open System
open System.IO
open System.Threading

open Ecos
open Ecos.Experiment.Experiment3
open Ecos.Experiment.Experiment7

/// Experiment 8: head to head. As in experiment 7, W/X strands compete
/// with Y/Z strands in a living world, with the weak Y-Z bond at
/// various energies. Here, though, every world starts with equal
/// numbers of each kind (three-atom strands, P-W-Q and P-Y-Q), so the
/// result doesn't depend on which kind happened to arise first.
///
/// A stronger Y-Z bond holds a free atom on its template for longer in
/// the cold phase, which should help copying. In the hot phase, though,
/// breaking a weak bond only costs anything once its energy is more
/// than kT times its entropy (3 x 3 = 9 by default). Beyond that, a
/// stronger bond should slow the separation of copies from their
/// templates, so the advantage should shrink or reverse.
module Experiment8 =

    /// Settings for the experiment.
    type Settings =
        {
            /// Energies of the weak Y-Z bond to run with. W-X keeps the
            /// default weak bond energy.
            YZEnergies : float[]

            /// Number of strands of each kind to start with.
            NumSeeds : int

            /// Number of independent runs (seeds) with each energy.
            NumRuns : int

            /// Number of times to run through the scenario's phases.
            NumCycles : int
        }

    /// Default settings.
    let defaultSettings =
        {
            YZEnergies = [| 2.0; 3.0; 6.0; 9.0; 12.0; 18.0 |]
            NumSeeds = 8
            NumRuns = 12
            NumCycles = 30
        }

    /// The given scenario's parameters at the given temperature, with
    /// the given energy for the weak Y-Z bond.
    let private getParameters scenario yzEnergy kT =
        let parms = Experiment3.getParameters scenario None kT
        let weakParms =
            BondTypeParameters.addPairEnergy
                AtomType.Y AtomType.Z yzEnergy parms.BondParameterMap[Weak]
        { parms with
            BondParameterMap =
                Map.add Weak weakParms parms.BondParameterMap }

    /// Adds a three-atom strand, P-x-Q with the given coding atom type,
    /// at a random empty place in the given world.
    let private addStrand (rng : Random) codingType (world : World) =
        let rec loop () =
            let x = 2 * rng.Next(-35, 33)
            let y = 2 * rng.Next(-35, 36)
            let tryAdd atomType dx (worldOpt : Option<World * AtomId list>) =
                worldOpt
                    |> Option.bind (fun (world, ids) ->
                        World.tryAddAtom atomType (Point.create (x + dx, y)) world
                            |> Option.map (fun (world, atom) ->
                                world, atom.Id :: ids))
            let result =
                Some (world, [])
                    |> tryAdd AtomType.P 0
                    |> tryAdd codingType 2
                    |> tryAdd AtomType.Q 4
            match result with
                | Some (world, [ idQ; idCoding; idP ]) ->
                    world
                        |> World.addStrongBond idP idCoding
                        |> World.addStrongBond idCoding idQ
                | _ -> loop ()
        loop ()

    /// Runs one world through the cycles. Answers a result for each
    /// cycle, starting with the world as seeded.
    let private runOne settings yzEnergy seed =
        let scenario = Scenario.createScenarioB false seed
        let world =
            let rng = Random(seed + 1000)   // keep the scenario's own generator for the dynamics
            (scenario.World, [ 1 .. settings.NumSeeds ])
                ||> Seq.fold (fun world _ ->
                    world
                        |> addStrand rng AtomType.W
                        |> addStrand rng AtomType.Y)
        let phases = getPhases scenario
        let rng = scenario.Random
        let mutable world = world
        [|
            getCensus world
            for iCycle in 1 .. settings.NumCycles do
                for phase in phases do
                    let parms = getParameters scenario yzEnergy phase.Temperature
                    world <- World.sweep parms rng phase.NumSweeps world
                eprintfn "Y-Z energy %g, seed %d: finished cycle %d of %d" yzEnergy seed iCycle settings.NumCycles
                getCensus world
        |]

    /// Fraction of W/X and Y/Z chains together that are Y/Z.
    let private getShare (result : CycleResult) =
        let n = result.WX + result.YZ
        if n > 0 then float result.YZ / float n else nan

    /// Runs the experiment, prints tables of results, and writes them
    /// to the given CSV file.
    let run (settings : Settings) (csvPath : string) =

        let combos =
            [|
                for yzEnergy in settings.YZEnergies do
                    for seed in 0 .. settings.NumRuns - 1 do
                        yzEnergy, seed
            |]

            // run every combination in parallel
        let mutable nDone = 0
        let results =
            combos
                |> Array.Parallel.map (fun (yzEnergy, seed) ->
                    let cycles = runOne settings yzEnergy seed
                    let n = Interlocked.Increment(&nDone)
                    eprintfn "Finished run %d of %d" n combos.Length
                    yzEnergy, cycles)
                |> Array.groupBy fst
                |> Array.map (fun (yzEnergy, group) ->
                    yzEnergy, Array.map snd group)
                |> Map

        let scenario = Scenario.createScenarioB false 0
        printfn "Experiment 8: head to head between W/X and Y/Z strands"
        printfn
            "%d runs of %d cycles with each Y-Z energy; %d free atoms; %d strands of each kind to start (P-W-Q and P-Y-Q)"
            settings.NumRuns settings.NumCycles
            scenario.World.Atoms.Length settings.NumSeeds
        for phase in getPhases scenario do
            printfn "Phase: kT %g for %d sweeps" phase.Temperature phase.NumSweeps
        printParameters (Experiment3.getParameters scenario None nan)

        use writer = new StreamWriter(csvPath)
        writer.WriteLine(
            "yz_energy,cycle,wx,yz,mixed,free_wx,free_yz,"
                + "yz_share,runs_yz_ahead,runs_wx_ahead")

        let summaries = ResizeArray()
        for yzEnergy in settings.YZEnergies do
            let runs = results[yzEnergy]
            printfn ""
            printfn "Weak Y-Z bond energy %g" yzEnergy
            printfn "        |--- capped chains per run ---|  |-- free per run --|            |-- fraction of runs --|"
            printfn "cycle      W/X     Y/Z   mixed           W+X     Y+Z      Y/Z share     Y/Z ahead   W/X ahead"
            for iCycle in 0 .. settings.NumCycles do
                let cycle = runs |> Array.map (fun cycles -> cycles[iCycle])
                let average f = cycle |> Array.averageBy (f >> float)
                let fraction test =
                    cycle |> Array.averageBy (fun c -> if test c then 1.0 else 0.0)
                let nWX = average _.WX
                let nYZ = average _.YZ
                let share = if nWX + nYZ > 0.0 then nYZ / (nWX + nYZ) else nan
                let values =
                    [|
                        yzEnergy
                        float iCycle
                        nWX
                        nYZ
                        average _.Mixed
                        average _.FreeWX
                        average _.FreeYZ
                        share
                        fraction (fun c -> c.YZ > c.WX)
                        fraction (fun c -> c.WX > c.YZ)
                    |]
                printfn
                    "%5d   %6.2f  %6.2f  %6.2f        %6.1f  %6.1f         %6.2f        %6.2f      %6.2f"
                    iCycle values[2] values[3] values[4] values[5]
                    values[6] values[7] values[8] values[9]
                writer.WriteLine(
                    String.Join(
                        ",",
                        values
                            |> Seq.map (fun (x : float) ->
                                x.ToString(Globalization.CultureInfo.InvariantCulture))))

                // each run's final share, and how runs ended
            let final = runs |> Array.map (fun cycles -> cycles[settings.NumCycles])
            let shares = final |> Array.map getShare
            printfn
                "Final Y/Z share in each run: %s"
                (shares
                    |> Array.sort
                    |> Array.map (sprintf "%.2f")
                    |> String.concat " ")
            let count test = final |> Array.where test |> Array.length
            let nYZAhead = count (fun c -> c.YZ > c.WX)
            let nWXAhead = count (fun c -> c.WX > c.YZ)
            let nWXGone = count (fun c -> c.WX = 0 && c.YZ > 0)
            let nYZGone = count (fun c -> c.YZ = 0 && c.WX > 0)
            printfn
                "At the end: Y/Z ahead in %d runs, W/X ahead in %d, tied in %d; W/X gone in %d, Y/Z gone in %d."
                nYZAhead nWXAhead (runs.Length - nYZAhead - nWXAhead)
                nWXGone nYZGone
            let valid = shares |> Array.where (Double.IsNaN >> not)
            let mean = Array.average valid
            let sd =
                sqrt (
                    (valid |> Array.sumBy (fun s -> (s - mean) ** 2.0))
                        / float (valid.Length - 1))
            summaries.Add(yzEnergy, mean, sd / sqrt (float valid.Length), nYZAhead, nWXAhead)

        printfn ""
        printfn "Summary: final Y/Z share by Y-Z energy (W-X energy is %g)" scenario.Parameters.BondParameterMap[Weak].Energy
        printfn "energy   mean share   std error   Y/Z ahead   W/X ahead"
        for (yzEnergy, mean, se, nYZAhead, nWXAhead) in summaries do
            printfn "%6g       %6.2f      %6.2f        %4d        %4d" yzEnergy mean se nYZAhead nWXAhead

        printfn ""
        printfn "W/X, Y/Z: capped chains whose coding atoms are all of that kind; mixed: both kinds."
        printfn "Y/Z share: Y/Z chains as a fraction of W/X and Y/Z chains together (averaged over runs first, except in the summary)."
        printfn "Results written to %s" (Path.GetFullPath csvPath)
