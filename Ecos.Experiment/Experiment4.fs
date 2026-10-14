namespace Ecos.Experiment

open System
open System.IO
open System.Threading

open Ecos
open Ecos.Experiment.Experiment3

/// Experiment 4: emergence. The same world, bond parameters and
/// temperature cycle as experiment 3, but with no template: only free
/// atoms. Do strands that can be copied arise by chance, and once one
/// has, does it multiply?
///
/// The only strands that survive a hot phase are those with a cap at
/// each end, and the smallest of those has three atoms: a cap, a coding
/// atom, and a cap. Making one from free atoms takes two strong bonds
/// that no template helps along, so it depends on the strong bond
/// barrier, which this experiment varies.
module Experiment4 =

    /// Settings for the experiment.
    type Settings =
        {
            /// Strong bond barriers to run with. The barrier reduction
            /// per stack is set to the same value, so that a templated
            /// join has no barrier (other than the start barrier).
            StrongBarriers : float[]

            /// Number of independent runs (seeds) with each barrier.
            NumRuns : int

            /// Number of times to run through the scenario's phases.
            NumCycles : int

            /// Barrier to peeling a strong bond from an exposed end of a
            /// strand. (The desktop app uses the same value.)
            PeelBarrier : float

            /// Barrier to a cap coming off the strand it ends, after which
            /// the strand peels. Infinity means caps never come off.
            CapPeelBarrier : float
        }

    /// Default settings.
    let defaultSettings =
        {
            StrongBarriers = [| 20.0; 15.0; 12.0; 10.0 |]
            NumRuns = 8
            NumCycles = 10
            PeelBarrier = 6.0
            CapPeelBarrier = Double.PositiveInfinity
        }

    /// Settings for a living world: strands arise from free atoms and
    /// also die, since their caps can come off, so the population turns
    /// over. Run for many cycles, to see what a world settles into.
    let livingWorldSettings =
        {
            StrongBarriers = [| 20.0; 15.0 |]
            NumRuns = 8
            NumCycles = 30
            PeelBarrier = 6.0
            CapPeelBarrier = 27.0
        }

    /// What a world contains at the end of a cycle.
    type private CycleResult =
        {
            /// Family of each capped chain at the end of the cycle.
            /// (See Experiment3.getFamily.)
            Families : List<AtomType>[]

            /// Number of strands of any kind at the end of the cycle's
            /// first phase, before the heat has peeled any away.
            StrandsBeforeHeat : int
        }

    /// Families of the capped chains in the given world.
    let private getCappedFamilies (world : World) =
        getStrands world
            |> Array.where (isCapped world)
            |> Array.map (fun strand ->
                strand.AtomIds
                    |> Array.map (fun id -> world.Atoms[id].Type)
                    |> getFamily)

    /// Runs one world through the cycles. Answers a result for each
    /// cycle.
    let private runOne settings barrier seed =

            // free atoms only, with experiment 3's parameters and cycle
        let scenario = Scenario.createScenarioA false seed
        let cycled = Scenario.createScenarioB true seed
        let phases = getPhases cycled
        let bondParameterMap =
            let strongParms =
                { cycled.BondParameterMap[Strong] with
                    Barrier = barrier
                    BarrierStackReduction = barrier }
            Map.add Strong strongParms cycled.BondParameterMap

        let rng = scenario.Random
        let mutable world = scenario.World
        [|
            for iCycle in 1 .. settings.NumCycles do
                let mutable nStrandsBeforeHeat = 0
                for iPhase, phase in Array.indexed phases do
                    let parms =
                        {
                            Temperature = phase.Temperature
                            BondParameterMap = bondParameterMap
                            PeelBarrier = settings.PeelBarrier
                            CapPeelBarrier = settings.CapPeelBarrier
                        }
                    for _ in 1 .. phase.NumSweeps do
                        world <- World.sweep parms rng world
                    if iPhase = 0 then
                        nStrandsBeforeHeat <- (getStrands world).Length
                eprintfn "Barrier %g, seed %d: finished cycle %d of %d" barrier seed iCycle settings.NumCycles
                {
                    Families = getCappedFamilies world
                    StrandsBeforeHeat = nStrandsBeforeHeat
                }
        |]

    /// Writes the given family as a sequence, e.g. P-X-Q.
    let private formatFamily (family : List<AtomType>) =
        family
            |> List.map string
            |> String.concat "-"

    /// Families with more than one chain, and the number of chains in
    /// each, largest first.
    let private getCopiedFamilies (result : CycleResult) =
        result.Families
            |> Array.countBy id
            |> Array.where (fun (_, count) -> count > 1)
            |> Array.sortByDescending snd

    /// Runs the experiment, prints tables of results, and writes them
    /// to the given CSV file.
    let run (settings : Settings) (csvPath : string) =

        let combos =
            [|
                for barrier in settings.StrongBarriers do
                    for seed in 0 .. settings.NumRuns - 1 do
                        barrier, seed
            |]

            // run every combination in parallel
        let mutable nDone = 0
        let results =
            combos
                |> Array.Parallel.map (fun (barrier, seed) ->
                    let cycles = runOne settings barrier seed
                    let n = Interlocked.Increment(&nDone)
                    eprintfn "Finished run %d of %d" n combos.Length
                    barrier, cycles)
                |> Array.groupBy fst
                |> Array.map (fun (barrier, group) ->
                    barrier, Array.map snd group)
                |> Map

        let cycled = Scenario.createScenarioB true 0
        let phases = getPhases cycled
        printfn "Experiment 4: emergence of strands from free atoms, with no template"
        printfn
            "%d runs of %d cycles with each barrier; %d atoms; peel barrier %g; cap peel barrier %g"
            settings.NumRuns settings.NumCycles
            (Scenario.createScenarioA false 0).World.Atoms.Count
            settings.PeelBarrier settings.CapPeelBarrier
        for phase in phases do
            printfn "Phase: kT %g for %d sweeps" phase.Temperature phase.NumSweeps
        for bondType in BondType.all do
            let bondParms = cycled.BondParameterMap[bondType]
            printfn
                "%A bonds: energy %g, entropy %g, start barrier %g%s"
                bondType
                bondParms.Energy
                bondParms.Entropy
                bondParms.StartBarrier
                (if bondType = Strong then "; barrier and reduction per stack as below"
                 else $"; barrier {bondParms.Barrier}")

        use writer = new StreamWriter(csvPath)
        writer.WriteLine(
            "barrier,cycle,runs_with_capped,runs_with_copies,"
                + "capped,copied_families,largest_family,average_atoms,longest,"
                + "strands_before_heat")

        for barrier in settings.StrongBarriers do
            let runs = results[barrier]
            printfn ""
            printfn "Strong bond barrier %g" barrier
            printfn "              |-- fraction of runs --|   |------------ capped chains at the end of the cycle ------------|   strands"
            printfn "cycle          with any   with copies      number   copied families   largest family   average atoms   longest   before heat"
            for iCycle in 0 .. settings.NumCycles - 1 do
                let cycleResults = runs |> Array.map (fun cycles -> cycles[iCycle])
                let fraction test =
                    cycleResults
                        |> Array.averageBy (fun result ->
                            if test result then 1.0 else 0.0)
                let lengths =
                    cycleResults
                        |> Array.collect (fun result ->
                            result.Families |> Array.map List.length)
                let values =
                    [|
                        barrier
                        float (iCycle + 1)
                        fraction (fun result -> result.Families.Length > 0)
                        fraction (fun result -> (getCopiedFamilies result).Length > 0)
                        cycleResults
                            |> Array.averageBy (fun result -> float result.Families.Length)
                        cycleResults
                            |> Array.averageBy (fun result ->
                                float (getCopiedFamilies result).Length)
                        cycleResults
                            |> Array.averageBy (fun result ->
                                getCopiedFamilies result
                                    |> Array.tryHead
                                    |> Option.map (snd >> float)
                                    |> Option.defaultValue 0.0)
                        (if lengths.Length = 0 then nan
                         else lengths |> Array.averageBy float)
                        float (Array.fold max 0 lengths)
                        cycleResults
                            |> Array.averageBy (fun result ->
                                float result.StrandsBeforeHeat)
                    |]
                printfn
                    "%5d            %6.2f        %6.2f      %6.2f            %6.2f           %6.2f          %6.2f    %6.0f        %6.2f"
                    (iCycle + 1)
                    values[2] values[3] values[4] values[5] values[6]
                    values[7] values[8] values[9]
                writer.WriteLine(
                    String.Join(
                        ",",
                        values
                            |> Seq.map (fun (x : float) ->
                                x.ToString(Globalization.CultureInfo.InvariantCulture))))


                // turnover: families present, and those gained and lost
                // since the previous cycle
            printfn ""
            printfn "Turnover (averages per run)"
            printfn "cycle      families   gained   lost   share of the largest"
            for iCycle in 0 .. settings.NumCycles - 1 do
                let familySets =
                    runs
                        |> Array.map (fun cycles ->
                            let now = set cycles[iCycle].Families
                            let before =
                                if iCycle = 0 then Set.empty
                                else set cycles[iCycle - 1].Families
                            now, before, cycles[iCycle])
                let average f = Array.averageBy f familySets
                let share =
                    familySets
                        |> Array.choose (fun (_, _, result) ->
                            if result.Families.Length = 0 then None
                            else
                                let largest =
                                    result.Families
                                        |> Array.countBy id
                                        |> Array.map snd
                                        |> Array.max
                                Some (float largest / float result.Families.Length))
                printfn
                    "%5d       %6.2f   %6.2f %6.2f        %s"
                    (iCycle + 1)
                    (average (fun (now, _, _) -> float now.Count))
                    (average (fun (now, before, _) -> float (Set.difference now before).Count))
                    (average (fun (now, before, _) -> float (Set.difference before now).Count))
                    (if share.Length = 0 then "   -"
                     else sprintf "%6.2f" (Array.average share))

                // when things first appeared
            let firstCycle test =
                let firsts =
                    runs
                        |> Array.choose (fun cycles ->
                            cycles
                                |> Array.tryFindIndex test
                                |> Option.map ((+) 1))
                if firsts.Length = 0 then "never"
                else
                    let sorted = Array.sort firsts
                    $"cycle {sorted[0]} at the earliest, in {firsts.Length} of {runs.Length} runs"
            printfn
                "First capped chain: %s"
                (firstCycle (fun result -> result.Families.Length > 0))
            printfn
                "First family with copies: %s"
                (firstCycle (fun result -> (getCopiedFamilies result).Length > 0))

                // what there is at the end
            let finalFamilies =
                runs
                    |> Array.collect (fun cycles ->
                        cycles[settings.NumCycles - 1].Families)
                    |> Array.countBy id
                    |> Array.sortByDescending snd
            if finalFamilies.Length > 0 then
                printfn "Most common families at the end (chains in all runs together):"
                for family, count in Array.truncate 6 finalFamilies do
                    printfn "   %-40s %d" (formatFamily family) count

        printfn ""
        printfn "capped chain: a strand with a cap at each end. Only these survive a hot phase."
        printfn "family: chains that could be copies of each other, directly or through further copies."
        printfn "   They have the same sequence, or its complement, read from either end. A family is"
        printfn "   shown as one of those sequences."
        printfn "copied families: families with more than one chain, which suggests copying. (Two chains"
        printfn "   of one family can also arise separately by chance, especially short ones.)"
        printfn "largest family: number of chains in the largest such family (0 if there is none)."
        printfn "strands before heat: strands of any kind at the end of the first phase of the cycle."
        printfn "Results written to %s" (Path.GetFullPath csvPath)
