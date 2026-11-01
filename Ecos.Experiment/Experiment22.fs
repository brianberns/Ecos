namespace Ecos.Experiment

open System
open System.IO
open System.Threading

open Ecos
open Ecos.Experiment.Experiment3

/// Experiment 22: feast and famine. Under one-sided pairing, a folded
/// hairpin can't be copied (experiment 21), but in a famine, when
/// nothing can be copied and a single strand is exposed to hydrolysis,
/// folding protects it: after a long melt has scattered each strand's
/// copy, hairpin-formers survived a harsh adverse phase better than
/// non-formers (experiment 19, rerun under one-sided pairing). Does
/// that pay for the copies they lose by being folded the rest of the
/// time?
///
/// A world of free atoms, seeded with equal numbers of experiment 19's
/// 18-atom hairpin-formers and non-formers, goes through cycles of:
///
/// - a feast: cold, for copying;
/// - a long melt: hot, so that duplexes come apart and each strand's
///   copy drifts away;
/// - a famine: at kT 0.5, at which free atoms can hardly copy a single
///   strand (experiment 20) but a folded hairpin holds, while unprotected
///   strong bonds hydrolyze quickly and protected ones not at all;
/// - a short melt, so that hairpins come apart before the next feast.
///
/// Nothing hydrolyzes outside the famine. A control has no hydrolysis
/// in the famine either, so that it shows what folding costs in copies
/// alone.
///
/// Copies slip, so chains are counted by whether they can fold, as in
/// experiment 21, rather than by family.
module Experiment22 =

    /// A variant of the experiment.
    type Variant =
        {
            /// Rate of hydrolysis of unprotected strong bonds during the
            /// famine.
            FamineHydrolysisRate : float
        }

    /// Settings for the experiment.
    type Settings =
        {
            /// The strand that can fold into a hairpin.
            HairpinFormer : AtomType[]

            /// A strand with the same atoms that can't fold.
            NonFormer : AtomType[]

            /// Number of strands of each kind to start with.
            NumSeeds : int

            /// Shortest capped chain counted as either kind. Shorter
            /// ones are counted as other.
            MinLength : int

            /// Shortest hairpin stem that makes a chain a hairpin-former.
            MinStem : int

            /// Shortest stem, in pairs, of a chain counted as folded: the
            /// longest run of its atoms that are each paired within it
            /// must be at least twice this. (Two weak bonds within a chain
            /// are too few: as free atoms run short, strands of both kinds
            /// pair with themselves here and there.)
            MinFoldedStem : int

            /// Strong-bond bend stiffness, with the standard bend limit
            /// (right angles).
            BendStiffness : float

            /// The feast, for copying.
            Feast : Phase

            /// The long melt after the feast.
            Melt : Phase

            /// The famine.
            Famine : Phase

            /// The short melt after the famine.
            Unfold : Phase

            /// Variants to run.
            Variants : Variant[]

            /// Number of independent runs (seeds) of each variant.
            NumRuns : int

            /// Number of cycles to run.
            NumCycles : int
        }

    /// Default settings: experiment 19's hairpin-formers and non-formers,
    /// at the softer stiffness, at which they fold soonest; scenario C's
    /// feast; the melt and famine of the best condition of experiment
    /// 19's rerun (a 100,000-sweep melt, then 50,000 sweeps at kT 0.5),
    /// but with milder hydrolysis than its 2.5e-4, which cut two strands
    /// in three each cycle, faster than feasts could replace them; and
    /// four strands of each kind, half experiment 21's density, so that
    /// strands bump into each other less.
    let defaultSettings =
        {
            HairpinFormer = Experiment19.windowHairpinFormer
            NonFormer = Experiment19.windowNonFormer
            NumSeeds = 4
            MinLength = 10
            MinStem = 6
            MinFoldedStem = 4
            BendStiffness = 0.5
            Feast = { Temperature = 0.3; NumSweeps = 200_000 }
            Melt = { Temperature = 3.0; NumSweeps = 100_000 }
            Famine = { Temperature = 0.5; NumSweeps = 50_000 }
            Unfold = { Temperature = 3.0; NumSweeps = 10_000 }
            Variants =
                [|
                    { FamineHydrolysisRate = 0.0 }   // control
                    { FamineHydrolysisRate = 5e-5 }
                    { FamineHydrolysisRate = 1e-4 }   // 2.5e-4 killed both kinds within 10 cycles
                |]
            NumRuns = 16
            NumCycles = 10   // about 90 minutes on 24 cores
        }

    /// The given variant's world: scenario C's bonds (nothing arises by
    /// chance), at the settings' stiffness, seeded with the settings'
    /// number of strands of each kind at random places, and then the
    /// standard world's free atoms around them. Its schedule is the
    /// cycle's temperatures; hydrolysis, which the schedule can't
    /// change, is left out, so in the desktop app the famine is only a
    /// warm spell.
    let getScenario settings variant =
        let scenario = Scenario.createScenarioC false
        let build parms rng =
            let world =
                (World.createSquare 160, [ 1 .. settings.NumSeeds ])
                    ||> Seq.fold (fun world _ ->
                        world
                            |> Build.addStrandAtRandom parms rng 74 settings.HairpinFormer
                            |> Build.addStrandAtRandom parms rng 74 settings.NonFormer)
            let nSeeded = world.Atoms.Length
            let world = Build.addFreeAtomsAnywhere rng 120 world   // as many as in the standard world
            let hairpinFormers =   // seeded in alternation, so every other strand
                [|
                    for i in 0 .. 2 .. 2 * settings.NumSeeds - 1 do
                        let n = settings.HairpinFormer.Length
                        yield! [| i * n .. i * n + n - 1 |]
                |]
            assert(nSeeded = 2 * settings.NumSeeds * settings.HairpinFormer.Length)
            world, hairpinFormers
        { scenario with
            Name = $"Feast and famine, famine hydrolysis {variant.FamineHydrolysisRate} (not shown)"
            Build = build
            Schedule = Cycle [| settings.Feast; settings.Melt; settings.Famine; settings.Unfold |] }
            |> Scenario.withBend Strong Bend.rightAngle settings.BendStiffness

    /// The experiment's scenarios: one for each variant.
    let getScenarios settings =
        settings.Variants |> Array.map (getScenario settings)

    /// A hairpin-former with the given stem: P, then that many W, a
    /// loop of two Y, then as many X, then Q.
    let getHairpinFormer stem =
        [|
            AtomType.P
            yield! Array.replicate stem AtomType.W
            AtomType.Y; AtomType.Y
            yield! Array.replicate stem AtomType.X
            AtomType.Q
        |]

    /// Stems of the hairpin-formers in the folding scenario.
    let foldingStems = [| 3; 4; 6; 8; 10 |]

    /// Folding alone, to watch: hairpin-formers of several lengths (see
    /// foldingStems), far apart, with no free atoms, at the given
    /// stiffness. Each cycle melts them briefly into random coils, then
    /// holds them at kT 0.4, at which a lone strand folds soonest.
    let getFoldingScenario bendStiffness =
        let scenario = Scenario.createScenarioC false
        let build parms (_ : Random) =
            let world =
                (World.createSquare 160, Array.indexed foldingStems)
                    ||> Array.fold (fun world (i, stem) ->
                        let strand = getHairpinFormer stem
                        let y = 32 * (i - foldingStems.Length / 2)
                        world
                            |> Build.tryAddStrand parms -strand.Length y strand
                            |> Option.get)
            world, Array.empty
        let stems = foldingStems |> Array.map string |> String.concat ", "
        { scenario with
            Name = $"Folding alone, stems {stems}, stiffness {bendStiffness}"
            Build = build
            Schedule =
                Cycle [|
                    { Temperature = 3.0; NumSweeps = 10_000 }
                    { Temperature = 0.4; NumSweeps = 100_000 }
                |] }
            |> Scenario.withBend Strong Bend.rightAngle bendStiffness

    /// Folding scenarios at the experiment's stiffness and at the
    /// stiffer standard one.
    let getFoldingScenarios () =
        [| 0.5; 1.5 |] |> Array.map getFoldingScenario

    /// Kind of a capped chain.
    type private Kind =
        | HairpinFormer
        | NonFormer
        | Short

    /// Capped chains of each kind in a world.
    type private Count =
        {
            /// Chains that can fold into a hairpin.
            Hairpins : int

            /// Chains that can't.
            NonFormers : int

            /// Capped chains too short to count as either.
            Others : int

            /// Chains that can fold and are folded (see MinFoldedStem).
            Folded : int
        }

    /// Counts the capped chains in the given world by kind.
    let private getCount settings (world : World) =
        let weak = world.BondTypeMap[Weak]
        let kinds =
            getStrands world
                |> Array.where (isCapped world)
                |> Array.map (fun strand ->
                    let types = strand.AtomIds |> Array.map (fun id -> world.Atoms[id].Type)
                    let kind =
                        if types.Length < settings.MinLength then Short
                        elif Experiment16.getMaxStem types >= settings.MinStem then HairpinFormer
                        else NonFormer
                    let isFolded =
                        let atoms = set strand.AtomIds
                        let longestRun =
                            ((0, 0), strand.AtomIds)
                                ||> Array.fold (fun (longest, run) id ->
                                    let isInner =
                                        BondMap.getBonds id weak
                                            |> Set.exists atoms.Contains
                                    if isInner then max longest (run + 1), run + 1
                                    else longest, 0)
                                |> fst
                        longestRun >= 2 * settings.MinFoldedStem
                    kind, isFolded)
        let count kind = kinds |> Array.where (fst >> (=) kind) |> Array.length
        {
            Hairpins = count HairpinFormer
            NonFormers = count NonFormer
            Others = count Short
            Folded = kinds |> Array.where (fun (kind, isFolded) -> kind = HairpinFormer && isFolded) |> Array.length
        }

    /// Counts in one cycle.
    type private CycleResult =
        {
            /// At the end of the feast.
            Fed : Count

            /// At the end of the famine (and so of the cycle, since the
            /// short melt that follows breaks nothing).
            Starved : Count
        }

    /// Runs one world through the cycles, calling the given function
    /// with each cycle's number when it ends. Answers a result for each
    /// cycle, starting with the world as seeded.
    let private runOne settings variant seed onCycle =
        let scenario = getScenario settings variant
        let rng = Random(seed)
        let parmsOf (phase : Phase) rate =
            let parms =
                (scenario
                    |> Scenario.withHydrolysisRate rate
                    |> Scenario.withProtectedHydrolysisRate 0.0).Parameters
            { parms with GetTemperature = Parameters.getUniformTemperature phase.Temperature }
        let feast = parmsOf settings.Feast 0.0
        let melt = parmsOf settings.Melt 0.0
        let famine = parmsOf settings.Famine variant.FamineHydrolysisRate
        let unfold = parmsOf settings.Unfold 0.0
        let mutable world = scenario.Build feast rng |> fst
        let initial = getCount settings world
        [|
            { Fed = initial; Starved = initial }
            for iCycle in 1 .. settings.NumCycles do
                world <- World.sweep feast rng settings.Feast.NumSweeps world
                let fed = getCount settings world
                world <- World.sweep melt rng settings.Melt.NumSweeps world
                world <- World.sweep famine rng settings.Famine.NumSweeps world
                let starved = getCount settings world
                world <- World.sweep unfold rng settings.Unfold.NumSweeps world
                onCycle iCycle
                { Fed = fed; Starved = starved }
        |]

    /// Runs the experiment, prints tables of results, and writes them
    /// to the given CSV file.
    let run (settings : Settings) (csvPath : string) =

        let combos =
            [|
                for variant in settings.Variants do
                    for seed in 0 .. settings.NumRuns - 1 do
                        variant, seed
            |]

        let toText = Array.map string >> String.concat " "
        let phaseText (phase : Phase) = sprintf "kT %g for %d sweeps" phase.Temperature phase.NumSweeps
        printfn "Experiment 22: feast and famine"
        printfn "Hairpin-former: %s (longest stem %d)" (toText settings.HairpinFormer) (Experiment16.getMaxStem settings.HairpinFormer)
        printfn "Non-former:     %s (longest stem %d)" (toText settings.NonFormer) (Experiment16.getMaxStem settings.NonFormer)
        printfn "%d runs of %d cycles of each variant; %d strands of each kind to start, at random places"
            settings.NumRuns settings.NumCycles settings.NumSeeds
        printfn "Capped chains of %d atoms or more count as hairpin-formers if they could fold with a stem of %d pairs or more, otherwise as non-formers"
            settings.MinLength settings.MinStem
        printfn "Each cycle: feast %s; melt %s; famine %s; unfold %s"
            (phaseText settings.Feast) (phaseText settings.Melt) (phaseText settings.Famine) (phaseText settings.Unfold)
        printfn "Hydrolysis only in the famine, of unprotected strong bonds; protected ones never hydrolyze"
        printParameters (getScenario settings settings.Variants[0]).Parameters
        printfn ""

            // run every combination in parallel, reporting each cycle
            // once every world has finished it
        let nFinished : int[] = Array.zeroCreate (settings.NumCycles + 1)
        let stopwatch = Diagnostics.Stopwatch.StartNew()
        let onCycle iCycle =
            if Interlocked.Increment(&nFinished[iCycle]) = combos.Length then
                printfn "Finished cycle %d of %d (%.0f min)"
                    iCycle settings.NumCycles stopwatch.Elapsed.TotalMinutes
        let results =
            combos
                |> Array.Parallel.map (fun (variant, seed) ->
                    variant, runOne settings variant seed onCycle)
                |> Array.groupBy fst
                |> Array.map (fun (variant, group) -> variant, Array.map snd group)
                |> Map

        use writer = new StreamWriter(csvPath)
        writer.WriteLine("famine_hydrolysis,cycle,fed_hairpins,fed_non_formers,fed_folded,starved_hairpins,starved_non_formers,others,hairpin_share")
        let invariant (x : float) = x.ToString(Globalization.CultureInfo.InvariantCulture)
        let share (hairpins : float) nonFormers =
            let n = hairpins + nonFormers
            if n > 0.0 then hairpins / n else nan
        for variant in settings.Variants do
            let runs = results[variant]
            printfn ""
            printfn "Famine hydrolysis %g" variant.FamineHydrolysisRate
            printfn "        |-- after the feast --|   |- after the famine -|"
            printfn "cycle   hairpin / non   folded   hairpin / non   share   other"
            for iCycle in 0 .. settings.NumCycles do
                let cycle = runs |> Array.map (fun cycles -> cycles[iCycle])
                let average f = cycle |> Array.averageBy (f >> float)
                let fh = average _.Fed.Hairpins
                let fn = average _.Fed.NonFormers
                let ff = average _.Fed.Folded
                let sh = average _.Starved.Hairpins
                let sn = average _.Starved.NonFormers
                let o = average _.Starved.Others
                writer.WriteLine(
                    String.Join(",",
                        [ invariant variant.FamineHydrolysisRate; string iCycle
                          invariant fh; invariant fn; invariant ff
                          invariant sh; invariant sn; invariant o; invariant (share sh sn) ]))
                printfn "%5d   %6.2f / %5.2f   %5.2f   %6.2f / %5.2f   %5.2f  %6.1f"
                    iCycle fh fn ff sh sn (share sh sn) o

                // at the end, per run
            let final = runs |> Array.map (fun cycles -> cycles[settings.NumCycles].Starved)
            let shares =
                final
                    |> Array.choose (fun c ->
                        let n = c.Hairpins + c.NonFormers
                        if n > 0 then Some (float c.Hairpins / float n) else None)
            let nAhead = final |> Array.where (fun c -> c.Hairpins > c.NonFormers) |> Array.length
            let nBehind = final |> Array.where (fun c -> c.Hairpins < c.NonFormers) |> Array.length
            let meanText =
                if shares.Length < 2 then "-"
                else
                    let mean = Array.average shares
                    let sd = sqrt ((shares |> Array.sumBy (fun x -> (x - mean) ** 2.0)) / float (shares.Length - 1))
                    sprintf "%.3f +/- %.3f" mean (sd / sqrt (float shares.Length))
            printfn "At the end: hairpin share per run %s (%d runs with either kind); hairpins ahead in %d runs, behind in %d; sign test p = %.3g (ahead), %.3g (behind)"
                meanText shares.Length nAhead nBehind
                (Experiment17.getSignTestP nAhead nBehind)
                (Experiment17.getSignTestP nBehind nAhead)

        printfn ""
        printfn "hairpin, non: capped chains that could fold, and that couldn't, per run. folded: those that"
        printfn "   could fold and were folded (a run of at least %d atoms paired within the chain), after the feast."
            (2 * settings.MinFoldedStem)
        printfn "share: hairpins / (hairpins + non-formers) after the famine, from the averages."
        printfn "other: shorter capped chains, after the famine."
        printfn "Results written to %s" (Path.GetFullPath csvPath)
