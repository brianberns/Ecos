namespace Ecos.Experiment

open System
open System.IO
open System.Threading

open Ecos
open Ecos.Experiment.Experiment3

/// Experiment 17: hairpins head to head. A living world (that of
/// experiment 16) is seeded with equal numbers of two kinds of strand,
/// made of the same atoms: one that can fold into a hairpin, and one
/// that can't. Both compete with each other, and with the trimers that
/// arise in the world by chance. Which kind multiplies more?
///
/// There are two contests. In the short one, the hairpin-former,
/// P W W Y Y X X Q, is the shortest strand with a three-pair stem (P-Q,
/// W-X and W-X, around a Y Y loop). It hardly ever folds in a crowded
/// world, so the long one uses P W W W Y Y X X X Q, with a four-pair
/// stem. In each, the non-former has the same atoms, in an order that
/// can't fold, and the copy of each strand can fold exactly as well as
/// the strand itself.
///
/// Chains are counted at the end of each cycle, after the hot phase, and
/// folding at the end of each cold phase, while strands are paired.
///
/// Without protection from hydrolysis, folding should only cost: a
/// folded strand can't template. With protection, a strong bond whose
/// atoms are both paired hydrolyzes more slowly, whether its strand is
/// folded or paired with another, so each variant of the experiment
/// sets both the strong-bond stiffness and the protected rate.
module Experiment17 =

    /// The short hairpin-former: a three-pair stem around a Y Y loop.
    let shortHairpinFormer =
        [|
            AtomType.P
            AtomType.W; AtomType.W
            AtomType.Y; AtomType.Y
            AtomType.X; AtomType.X
            AtomType.Q
        |]

    /// The short hairpin-former's atoms, in an order that can't fold:
    /// its caps pair, as every strand's can, but no two consecutive
    /// pairs form around any loop.
    let shortNonFormer =
        [|
            AtomType.P
            AtomType.W; AtomType.W
            AtomType.Y
            AtomType.X; AtomType.X
            AtomType.Y
            AtomType.Q
        |]

    /// The long hairpin-former: a four-pair stem around a Y Y loop.
    let longHairpinFormer =
        [|
            AtomType.P
            AtomType.W; AtomType.W; AtomType.W
            AtomType.Y; AtomType.Y
            AtomType.X; AtomType.X; AtomType.X
            AtomType.Q
        |]

    /// The long hairpin-former's atoms, in an order that can't fold.
    let longNonFormer =
        [|
            AtomType.P
            AtomType.W; AtomType.W; AtomType.W
            AtomType.X; AtomType.X
            AtomType.Y
            AtomType.X
            AtomType.Y
            AtomType.Q
        |]

    /// A variant of the experiment.
    type Variant =
        {
            /// Strong-bond bend stiffness, with the standard bend limit
            /// (right angles).
            BendStiffness : float

            /// Rate of hydrolysis of protected strong bonds: those whose
            /// atoms are both weakly paired.
            ProtectedHydrolysisRate : float
        }

    /// Settings for the experiment.
    type Settings =
        {
            /// The strand that can fold into a hairpin.
            HairpinFormer : AtomType[]

            /// A strand with the same atoms that can't fold.
            NonFormer : AtomType[]

            /// Rate of hydrolysis of unprotected strong bonds.
            HydrolysisRate : float

            /// Variants to run.
            Variants : Variant[]

            /// Number of strands of each kind to start with.
            NumSeeds : int

            /// Number of independent runs (seeds) of each variant.
            NumRuns : int

            /// Seed of the first run. The others follow in order.
            FirstSeed : int

            /// Number of times to run through the scenario's phases.
            NumCycles : int
        }

    /// Default settings: the long contest, at the vent window's rate of
    /// hydrolysis, which ten-letter strands survive (at the living
    /// world's rate, 1e-4, even the short strands die out within 20
    /// cycles), and the standard stiffness, with no protection, protected
    /// bonds hydrolyzing five times more slowly, and full protection.
    let defaultSettings =
        {
            HairpinFormer = longHairpinFormer
            NonFormer = longNonFormer
            HydrolysisRate = 2e-5
            Variants =
                [|
                    for rate in [ 2e-5; 4e-6; 0.0 ] do
                        { BendStiffness = 1.5; ProtectedHydrolysisRate = rate }
                |]
            NumSeeds = 8
            NumRuns = 16
            FirstSeed = 0
            NumCycles = 20
        }

    /// Settings for the short contest, at the standard stiffness and
    /// with no protection.
    let shortSettings =
        { defaultSettings with
            HairpinFormer = shortHairpinFormer
            NonFormer = shortNonFormer
            Variants = [| defaultSettings.Variants[0] |] }

    /// What a world contains at the end of a cycle.
    type private CycleResult =
        {
            /// Capped chains in the hairpin-former's family (it, its
            /// copies, and theirs).
            Hairpins : int

            /// Capped chains in the hairpin-former's family at the end
            /// of the cycle's cold phase.
            ColdHairpins : int

            /// Those of them that were folded then: at least two weak
            /// bonds within the chain.
            Folded : int

            /// Capped chains in the non-former's family.
            NonFormers : int

            /// Other capped chains.
            Others : int
        }

    /// Sorts the capped chains in the given world.
    let private getCensus settings (world : World) =
        let hairpinFamily = getFamily settings.HairpinFormer
        let nonFormerFamily = getFamily settings.NonFormer
        let weak = world.BondTypeMap[Weak]
        let chains =
            getStrands world
                |> Array.where (isCapped world)
        let family (strand : Strand) =
            strand.AtomIds
                |> Array.map (fun id -> world.Atoms[id].Type)
                |> getFamily
        let isFolded (strand : Strand) =
            let atoms = set strand.AtomIds
            let inner =
                strand.AtomIds
                    |> Array.sumBy (fun id ->
                        BondMap.getBonds id weak
                            |> Set.intersect atoms
                            |> Set.count)
            inner / 2 >= 2   // each bond counted from both ends
        let hairpins = chains |> Array.where (fun s -> family s = hairpinFamily)
        let nonFormers = chains |> Array.where (fun s -> family s = nonFormerFamily)
        {
            Hairpins = hairpins.Length
            ColdHairpins = hairpins.Length
            Folded = (hairpins |> Array.where isFolded).Length
            NonFormers = nonFormers.Length
            Others = chains.Length - hairpins.Length - nonFormers.Length
        }

    /// The living world at the settings' rate of hydrolysis, with the
    /// given variant's stiffness and protection, seeded with the
    /// settings' number of strands of each kind at random places.
    let getScenario settings variant =
        let addStrands parms rng world =
            (world, [ 1 .. settings.NumSeeds ])
                ||> Seq.fold (fun world _ ->
                    world
                        |> Build.addStrandAtRandom parms rng 70 settings.HairpinFormer
                        |> Build.addStrandAtRandom parms rng 70 settings.NonFormer)
        Experiment4.getScenario Experiment4.livingWorldSettings 15.0
            |> Scenario.withHydrolysisRate settings.HydrolysisRate
            |> Scenario.withProtectedHydrolysisRate variant.ProtectedHydrolysisRate
            |> Scenario.withBend Strong Bend.rightAngle variant.BendStiffness
            |> Scenario.withBuildStep addStrands
            |> Scenario.withName $"Hairpins head to head, length {settings.HairpinFormer.Length}, kappa {variant.BendStiffness}, protected rate {variant.ProtectedHydrolysisRate}"

    /// The experiment's scenarios: one for each variant.
    let getScenarios settings =
        settings.Variants |> Array.map (getScenario settings)

    /// Runs one world through the cycles. Answers a result for each
    /// cycle, starting with the world as seeded.
    let private runOne settings variant seed =
        let scenario = getScenario settings variant
        let mutable run = Run.create seed scenario
        [|
            getCensus settings run.World
            let phases = Schedule.getPhases scenario.Schedule
            for iCycle in 1 .. settings.NumCycles do
                run <- Run.advance phases[0].NumSweeps run   // cold
                let cold = getCensus settings run.World
                for phase in phases[1..] do
                    run <- Run.advance phase.NumSweeps run
                eprintfn "%s, seed %d: finished cycle %d of %d" scenario.Name seed iCycle settings.NumCycles
                { getCensus settings run.World with
                    ColdHairpins = cold.Hairpins
                    Folded = cold.Folded }
        |]

    /// Chance of at least the given number of successes in the given
    /// number of trials, each with probability one half: the one-sided p
    /// value of a sign test.
    let getSignTestP nAhead nBehind =
        let n = nAhead + nBehind
        let choose n k =
            Seq.fold (fun acc i -> acc * float (n - k + i) / float i) 1.0 (seq { 1 .. k })
        seq { nAhead .. n }
            |> Seq.sumBy (fun k -> choose n k / 2.0 ** float n)

    /// Runs the experiment, prints tables of results, and writes them
    /// to the given CSV file.
    let run (settings : Settings) (csvPath : string) =

        let combos =
            [|
                for variant in settings.Variants do
                    for seed in settings.FirstSeed .. settings.FirstSeed + settings.NumRuns - 1 do
                        variant, seed
            |]

            // run every combination in parallel
        let mutable nDone = 0
        let results =
            combos
                |> Array.Parallel.map (fun (variant, seed) ->
                    let cycles = runOne settings variant seed
                    let n = Interlocked.Increment(&nDone)
                    eprintfn "Finished run %d of %d" n combos.Length
                    variant, cycles)
                |> Array.groupBy fst
                |> Array.map (fun (variant, group) -> variant, Array.map snd group)
                |> Map

        let toText = Array.map string >> String.concat " "
        let scenario = getScenario settings settings.Variants[0]
        printfn "Experiment 17: hairpins head to head"
        printfn "Hairpin-former: %s (longest stem %d)" (toText settings.HairpinFormer) (Experiment16.getMaxStem settings.HairpinFormer)
        printfn "Non-former:     %s (longest stem %d)" (toText settings.NonFormer) (Experiment16.getMaxStem settings.NonFormer)
        printfn "%d runs of %d cycles of each variant (seeds %d to %d); %d strands of each kind to start"
            settings.NumRuns settings.NumCycles
            settings.FirstSeed (settings.FirstSeed + settings.NumRuns - 1)
            settings.NumSeeds
        for phase in Schedule.getPhases scenario.Schedule do
            printfn "Phase: kT %g for %d sweeps" phase.Temperature phase.NumSweeps
        printParameters scenario.Parameters
        let strong = scenario.Parameters.BondTypeParameterMap[Strong]
        printfn "Strong bend limit %d/%d" strong.BendLimit.Numerator strong.BendLimit.Denominator

        use writer = new StreamWriter(csvPath)
        writer.WriteLine("kappa,protected_rate,cycle,hairpins,non_formers,others,hairpin_share,runs_hairpins_ahead,runs_non_formers_ahead,cold_hairpins,folded")
        let invariant (x : float) = x.ToString(Globalization.CultureInfo.InvariantCulture)
        for variant in settings.Variants do
            let runs = results[variant]
            printfn ""
            printfn "Strong bend stiffness %g, protected hydrolysis rate %g"
                variant.BendStiffness variant.ProtectedHydrolysisRate
            printfn "        |------ capped chains per run ------|              |-- fraction of runs --|       |- end of cold -|"
            printfn "cycle   hairpin  non-former   other   hairpin share   hairpins ahead   non-formers ahead   hairpin  folded"
            for iCycle in 0 .. settings.NumCycles do
                let cycle = runs |> Array.map (fun cycles -> cycles[iCycle])
                let average f = cycle |> Array.averageBy (f >> float)
                let fraction test =
                    cycle |> Array.averageBy (fun c -> if test c then 1.0 else 0.0)
                let hairpins = average _.Hairpins
                let nonFormers = average _.NonFormers
                let share =
                    let n = hairpins + nonFormers
                    if n > 0.0 then hairpins / n else nan
                let ahead = fraction (fun c -> c.Hairpins > c.NonFormers)
                let behind = fraction (fun c -> c.Hairpins < c.NonFormers)
                printfn "%5d   %7.2f  %10.2f  %6.1f   %13.2f   %14.2f   %17.2f   %7.2f  %6.2f"
                    iCycle hairpins nonFormers (average _.Others) share ahead behind
                    (average _.ColdHairpins) (average _.Folded)
                writer.WriteLine(
                    String.Join(",",
                        [
                            invariant variant.BendStiffness; invariant variant.ProtectedHydrolysisRate; string iCycle
                            invariant hairpins; invariant nonFormers
                            invariant (average _.Others); invariant share; invariant ahead; invariant behind
                            invariant (average _.ColdHairpins); invariant (average _.Folded)
                        ]))

                // spread between runs at the end
            let final = runs |> Array.map (fun cycles -> cycles[settings.NumCycles])
            let shares =
                final
                    |> Array.choose (fun c ->
                        let n = c.Hairpins + c.NonFormers
                        if n > 0 then Some (float c.Hairpins / float n) else None)
            let mean = Array.average shares
            let sd =
                sqrt ((shares |> Array.sumBy (fun x -> (x - mean) ** 2.0)) / float (shares.Length - 1))
            let nAhead = final |> Array.where (fun c -> c.Hairpins > c.NonFormers) |> Array.length
            let nBehind = final |> Array.where (fun c -> c.Hairpins < c.NonFormers) |> Array.length
            printfn "At the end: hairpin share per run %.3f +/- %.3f (standard error, %d runs with either kind left)"
                mean (sd / sqrt (float shares.Length)) shares.Length
            printfn "   hairpins ahead in %d runs, behind in %d, tied in %d; one-sided sign test p = %.3g"
                nAhead nBehind (final.Length - nAhead - nBehind) (getSignTestP nAhead nBehind)

        printfn ""
        printfn "hairpin, non-former: capped chains in each kind's family (it, its copies, and theirs), per run."
        printfn "other: all other capped chains, at the end of the cycle."
        printfn "end of cold: hairpin-family chains at the end of the cold phase, and those of them folded"
        printfn "   (at least two weak bonds within the chain)."
        printfn "hairpin share: hairpins / (hairpins + non-formers), from the averages."
        printfn "Results written to %s" (Path.GetFullPath csvPath)
