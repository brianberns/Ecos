namespace Ecos.Experiment

open System
open System.IO
open System.Threading

open Ecos
open Ecos.Experiment.Experiment3

/// Experiment 7: selection. A living world with no template, as in
/// experiment 5: strands arise from free atoms by chance, are copied,
/// and die when their caps come off. There are equal numbers of free
/// W, X, Y and Z atoms, so strands that pair W with X and strands that
/// pair Y with Z start out on an equal footing. The only difference is
/// that the weak bond between Y and Z is made stronger than the one
/// between W and X, by an amount that the experiment varies. Does the
/// stronger pairing take over the population, and how much stronger
/// does it have to be?
///
/// Capped chains are sorted by their coding atoms (those between the
/// caps):
/// * W/X chains: every coding atom is a W or an X
/// * Y/Z chains: every coding atom is a Y or a Z
/// * mixed chains: some of each
module Experiment7 =

    /// Settings for the experiment.
    type Settings =
        {
            /// Amounts by which the weak Y-Z bond is made stronger than
            /// the default weak bond (which W-X keeps).
            YZBonuses : float[]

            /// Strong bond barrier. The barrier reduction per stack is
            /// set to the same value.
            StrongBarrier : float

            /// Number of independent runs (seeds) with each bonus.
            NumRuns : int

            /// Number of times to run through the scenario's phases.
            NumCycles : int
        }

    /// Default settings. A barrier of 15 lets strands arise in every
    /// run within a few cycles (see experiment 5).
    let defaultSettings =
        {
            YZBonuses = [| 0.0; 0.2; 0.5; 1.0 |]
            StrongBarrier = 15.0
            NumRuns = 12
            NumCycles = 30
        }

    /// What a world contains at the end of a cycle.
    type CycleResult =
        {
            /// Capped chains whose coding atoms are all W or X.
            WX : int

            /// Capped chains whose coding atoms are all Y or Z.
            YZ : int

            /// Capped chains with coding atoms of both kinds.
            Mixed : int

            /// Free W and X atoms.
            FreeWX : int

            /// Free Y and Z atoms.
            FreeYZ : int
        }

    /// Scenario B with no template, the settings' strong bond barrier
    /// (and reduction per stack), and the given bonus to the weak Y-Z
    /// bond.
    let getScenario settings bonus =
        let scenario = Scenario.createScenarioB false
        let energy = scenario.Parameters.BondParameterMap[Weak].Energy + bonus
        scenario
            |> Scenario.withStrongBarrier settings.StrongBarrier
            |> Scenario.withPairEnergy Weak AtomType.Y AtomType.Z energy
            |> Scenario.withName $"Y-Z bonus {bonus}"

    /// The experiment's scenarios: one for each Y-Z bonus.
    let getScenarios settings =
        settings.YZBonuses |> Array.map (getScenario settings)

    let isWX = function
        | AtomType.W | AtomType.X -> true
        | _ -> false

    let isYZ = function
        | AtomType.Y | AtomType.Z -> true
        | _ -> false

    /// Sorts the capped chains in the given world, and counts its free
    /// coding atoms.
    let getCensus (world : World) =
        let strands = getStrands world
        let chains =
            strands
                |> Array.where (isCapped world)
                |> Array.map (fun strand ->
                    strand.AtomIds
                        |> Array.map (fun id -> world.Atoms[id].Type)
                        |> Array.where (fun typ -> isWX typ || isYZ typ))
        let count test =
            chains |> Array.where test |> Array.length
        let inStrands =
            strands
                |> Seq.collect _.AtomIds
                |> set
        let free test =
            world.Atoms
                |> Seq.where (fun atom ->
                    test atom.Type && not (inStrands.Contains atom.Id))
                |> Seq.length
        {
            WX = count (Array.forall isWX)
            YZ = count (Array.forall isYZ)
            Mixed =
                count (fun types ->
                    Array.exists isWX types && Array.exists isYZ types)
            FreeWX = free isWX
            FreeYZ = free isYZ
        }

    /// Runs one world through the cycles. Answers a result for each
    /// cycle.
    let private runOne settings bonus seed =
        let scenario = getScenario settings bonus
        let mutable run = Run.create seed scenario
        [|
            for iCycle in 1 .. settings.NumCycles do
                for phase in getPhases scenario do
                    run <- Run.advance phase.NumSweeps run
                eprintfn "Bonus %g, seed %d: finished cycle %d of %d" bonus seed iCycle settings.NumCycles
                getCensus run.World
        |]

    /// Runs the experiment, prints tables of results, and writes them
    /// to the given CSV file.
    let run (settings : Settings) (csvPath : string) =

        let combos =
            [|
                for bonus in settings.YZBonuses do
                    for seed in 0 .. settings.NumRuns - 1 do
                        bonus, seed
            |]

            // run every combination in parallel
        let mutable nDone = 0
        let results =
            combos
                |> Array.Parallel.map (fun (bonus, seed) ->
                    let cycles = runOne settings bonus seed
                    let n = Interlocked.Increment(&nDone)
                    eprintfn "Finished run %d of %d" n combos.Length
                    bonus, cycles)
                |> Array.groupBy fst
                |> Array.map (fun (bonus, group) ->
                    bonus, Array.map snd group)
                |> Map

        let scenario = Scenario.createScenarioB false
        let example = Run.create 0 scenario
        printfn "Experiment 7: selection between W/X and Y/Z pairing"
        printfn
            "%d runs of %d cycles with each Y-Z bonus; %d atoms; no template; strong bond barrier %g"
            settings.NumRuns settings.NumCycles
            example.World.Atoms.Length settings.StrongBarrier
        for phase in getPhases scenario do
            printfn "Phase: kT %g for %d sweeps" phase.Temperature phase.NumSweeps
        printParameters scenario.Parameters
        printfn "(The strong bond barrier and its reduction per stack are replaced by the value above.)"

        use writer = new StreamWriter(csvPath)
        writer.WriteLine(
            "yz_bonus,cycle,wx,yz,mixed,free_wx,free_yz,"
                + "yz_share,runs_yz_ahead,runs_wx_ahead")

        for bonus in settings.YZBonuses do
            let runs = results[bonus]
            printfn ""
            printfn "Weak Y-Z bond stronger by %g (energy %g)" bonus (scenario.Parameters.BondParameterMap[Weak].Energy + bonus)
            printfn "        |--- capped chains per run ---|  |-- free per run --|            |-- fraction of runs --|"
            printfn "cycle      W/X     Y/Z   mixed           W+X     Y+Z      Y/Z share     Y/Z ahead   W/X ahead"
            for iCycle in 0 .. settings.NumCycles - 1 do
                let cycle = runs |> Array.map (fun cycles -> cycles[iCycle])
                let average f = cycle |> Array.averageBy (f >> float)
                let fraction test =
                    cycle |> Array.averageBy (fun c -> if test c then 1.0 else 0.0)
                let nWX = average _.WX
                let nYZ = average _.YZ
                let share = if nWX + nYZ > 0.0 then nYZ / (nWX + nYZ) else nan
                let values =
                    [|
                        bonus
                        float (iCycle + 1)
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
                    (iCycle + 1) values[2] values[3] values[4] values[5]
                    values[6] values[7] values[8] values[9]
                writer.WriteLine(
                    String.Join(
                        ",",
                        values
                            |> Seq.map (fun (x : float) ->
                                x.ToString(Globalization.CultureInfo.InvariantCulture))))

            let final = runs |> Array.map (fun cycles -> cycles[settings.NumCycles - 1])
            let count test = final |> Array.where test |> Array.length
            printfn
                "At the end: Y/Z ahead in %d runs, W/X ahead in %d, tied in %d."
                (count (fun c -> c.YZ > c.WX))
                (count (fun c -> c.WX > c.YZ))
                (count (fun c -> c.WX = c.YZ))

        printfn ""
        printfn "W/X, Y/Z: capped chains whose coding atoms are all of that kind; mixed: both kinds."
        printfn "Y/Z share: Y/Z chains as a fraction of W/X and Y/Z chains together (averaged over runs first)."
        printfn "Results written to %s" (Path.GetFullPath csvPath)
