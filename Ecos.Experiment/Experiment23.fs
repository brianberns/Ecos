namespace Ecos.Experiment

open System
open System.IO
open System.Text.RegularExpressions
open System.Threading

open Ecos
open Ecos.Experiment.Experiment3

/// Experiment 23: folding in a famine. Under one-sided pairing, a strand
/// whose face is taken by food can't fold, but one that has no food
/// does (the food-cue test). A closed world fills up until free atoms
/// run short, after which most strands that come out of each melt can't
/// be copied: a famine, with no atoms added or removed. Then stress,
/// cold hydrolysis of unprotected strong bonds at the end of each feast,
/// spares strands that were copied and those that folded, and cuts
/// starving single strands. Does it favor strands that fold when they
/// starve?
///
/// A world of free atoms, seeded with equal numbers of 24-atom
/// hairpin-formers (P, ten W, Y Y, ten X, Q) and non-formers (the same
/// atoms in an order that can't fold), goes through cycles of:
///
/// - a feast: cold, for copying, with no hydrolysis;
/// - stress: just as cold, with hydrolysis of unprotected strong bonds;
///   protected ones never hydrolyze;
/// - a melt: hot, so that duplexes and hairpins come apart.
///
/// A control has no hydrolysis in the stress phase either. Copies slip,
/// and the two sequences don't slip alike, so the two kinds are judged
/// against the control, run for run: a run and its control use the same
/// seed, so they are the same until the first bond hydrolyzes.
///
/// Chains are counted by family: a chain is in the hairpin family if it
/// is caps around a run of W, a run of Y or Z (perhaps empty), and a run
/// of X, in either order (Z
/// is Y's complement, so copies have Z Z), and otherwise in the
/// non-former family.
module Experiment23 =

    /// A variant of the experiment.
    type Variant =
        {
            /// Rate of hydrolysis of unprotected strong bonds during the
            /// stress phase.
            StressHydrolysisRate : float
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

            /// Shortest capped chain counted in either family. Shorter
            /// ones are counted as other.
            MinLength : int

            /// Shortest hairpin stem that makes a chain foldable.
            MinStem : int

            /// Shortest stem, in pairs, of a chain counted as folded: the
            /// longest run of its atoms that are each paired within it
            /// must be at least twice this.
            MinFoldedStem : int

            /// Strong-bond bend stiffness, with the standard bend limit
            /// (right angles).
            BendStiffness : float

            /// The feast, for copying.
            Feast : Phase

            /// The stress phase, cold, after the feast.
            Stress : Phase

            /// The melt after the stress phase.
            Melt : Phase

            /// Variants to run.
            Variants : Variant[]

            /// Number of independent runs (seeds) of each variant.
            NumRuns : int

            /// Number of cycles to run.
            NumCycles : int

            /// Number of the commonest sequences of each family to list
            /// at the end.
            NumCommonSequences : int
        }

    /// The non-former: experiment 22's stem-10 hairpin-former's atoms,
    /// shuffled until its longest stem is 2.
    let nonFormer =
        [|
            AtomType.P
            AtomType.W; AtomType.X; AtomType.X; AtomType.W; AtomType.X; AtomType.X
            AtomType.W; AtomType.X; AtomType.X; AtomType.W; AtomType.Y; AtomType.W
            AtomType.W; AtomType.W; AtomType.W; AtomType.W; AtomType.Y; AtomType.X
            AtomType.X; AtomType.W; AtomType.X; AtomType.X
            AtomType.Q
        |]

    /// Default settings: a stem-10 hairpin-former, which folds most fully
    /// when it starves (the food-cue test), at the softer stiffness; and
    /// the stress rate at which, each kind alone, folded strands survived
    /// stress about as well as copied ones (93% against 92%), while
    /// single strands lost about four in ten and both kinds persisted.
    let defaultSettings =
        {
            HairpinFormer = Experiment22.getHairpinFormer 10
            NonFormer = nonFormer
            NumSeeds = 4
            MinLength = 10
            MinStem = 6
            MinFoldedStem = 4
            BendStiffness = 0.5
            Feast = { Name = Some "feast"; Temperature = 0.3; NumSweeps = 200_000; HydrolysisRate = None }
            Stress = { Name = Some "stress"; Temperature = 0.3; NumSweeps = 50_000; HydrolysisRate = None }
            Melt = { Name = Some "melt"; Temperature = 3.0; NumSweeps = 100_000; HydrolysisRate = None }
            Variants =
                [|
                    { StressHydrolysisRate = 0.0 }   // control
                    { StressHydrolysisRate = 1.4e-4 }   // 2.8e-4 cut half of folded strands too
                |]
            NumRuns = 16
            NumCycles = 12
            NumCommonSequences = 8
        }

    /// A non-former made of runs, like the hairpin-former, so that it
    /// should copy about as fast: P, ten W, Y Y, ten W, Q. W pairs only
    /// with X, so its halves can't pair with each other.
    let runner =
        [|
            AtomType.P
            yield! Array.replicate 10 AtomType.W
            AtomType.Y; AtomType.Y
            yield! Array.replicate 10 AtomType.W
            AtomType.Q
        |]

    /// Settings that pit the hairpin-former against the runner, at
    /// stiffness 0: does the hairpin-former still gain under stress
    /// when the non-former copies as fast?
    let runnerSettings =
        { defaultSettings with
            NonFormer = runner
            BendStiffness = 0.0 }

    /// The given variant's world: scenario C's bonds (nothing arises by
    /// chance), at the settings' stiffness, seeded with the settings'
    /// number of strands of each kind at random places, and then the
    /// standard world's free atoms around them. Its schedule is the
    /// cycle, with the variant's hydrolysis in the stress phase;
    /// protected bonds never hydrolyze.
    let getScenario settings variant =
        let scenario =
            Scenario.createScenarioC false
                |> Scenario.withProtectedHydrolysisRate 0.0
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
                let n = settings.HairpinFormer.Length
                assert(settings.NonFormer.Length = n)
                [|
                    for i in 0 .. 2 .. 2 * settings.NumSeeds - 1 do
                        yield! [| i * n .. i * n + n - 1 |]
                |]
            assert(nSeeded = 2 * settings.NumSeeds * settings.HairpinFormer.Length)
            world, hairpinFormers
        { scenario with
            Name =
                let nonFormer = settings.NonFormer |> Array.map string |> String.concat ""
                $"Folding in a famine against {nonFormer}, stiffness {settings.BendStiffness}, stress hydrolysis {variant.StressHydrolysisRate}"
            Build = build
            Schedule =
                Cycle [|
                    settings.Feast
                    { settings.Stress with HydrolysisRate = Some variant.StressHydrolysisRate }
                    settings.Melt
                |] }
            |> Scenario.withBend Strong Bend.rightAngle settings.BendStiffness

    /// The experiment's scenarios: one for each variant.
    let getScenarios settings =
        settings.Variants |> Array.map (getScenario settings)

    /// Family of a chain.
    type private Family =
        | HairpinFamily
        | NonFormerFamily

    /// Chains of the hairpin family, as text.
    let private hairpinPattern = Regex("^[PQ](W+[YZ]*X+|X+[YZ]*W+)[PQ]$")

    /// Family of the chain with the given sequence, as text.
    let private getFamily (text : string) =
        if hairpinPattern.IsMatch text then HairpinFamily
        else NonFormerFamily

    /// Sequence of the given chain, as text.
    let private getText (world : World) (atomIds : AtomId[]) =
        atomIds
            |> Array.map (fun id -> string world.Atoms[id].Type)
            |> String.concat ""

    /// State of a chain.
    type private State =
        /// Half or more of its atoms are paired with another chain.
        | Copied
        /// Folded (see MinFoldedStem), but not copied.
        | Folded
        /// Neither.
        | Single

    /// All states, in order.
    let private states = [| Copied; Folded; Single |]

    /// Census of one capped chain.
    type private ChainCensus =
        {
            /// The chain's atoms, in order.
            AtomIds : AtomId[]

            /// Its family.
            Family : Family

            /// Can it fold? (See MinStem.)
            IsFoldable : bool

            /// Its state.
            State : State

            /// Number of its strong bonds that are not protected from
            /// hydrolysis.
            NumExposed : int
        }

    /// Census of the capped chains of a world.
    type private Census =
        {
            /// Chains counted in either family.
            Chains : ChainCensus[]

            /// Number of shorter capped chains.
            Others : int
        }

    /// Takes a census of the capped chains in the given world.
    let private getCensus settings (world : World) =
        let strong = world.BondTypeMap[Strong]
        let weak = world.BondTypeMap[Weak]
        let capped = getStrands world |> Array.where (isCapped world)
        let chains =
            capped
                |> Array.where (fun strand -> strand.AtomIds.Length >= settings.MinLength)
                |> Array.map (fun strand ->
                    let ids = strand.AtomIds
                    let atoms = set ids
                    let partners =
                        ids |> Array.map (fun id ->
                            BondMap.getBonds id weak |> Seq.tryHead)
                    let nCopied =
                        partners
                            |> Array.where (function
                                | Some p ->
                                    not (atoms.Contains p)
                                        && not (Set.isEmpty (BondMap.getBonds p strong))
                                | None -> false)
                            |> Array.length
                    let longestInnerRun =
                        ((0, 0), partners)
                            ||> Array.fold (fun (longest, run) p ->
                                match p with
                                    | Some p when atoms.Contains p -> max longest (run + 1), run + 1
                                    | _ -> longest, 0)
                            |> fst
                    let state =
                        if 2 * nCopied >= ids.Length then Copied
                        elif longestInnerRun >= 2 * settings.MinFoldedStem then Folded
                        else Single
                    let numExposed =
                        ids
                            |> Seq.pairwise
                            |> Seq.where (fun (a, b) ->
                                not (World.isProtected world.Atoms[a] world.Atoms[b] world))
                            |> Seq.length
                    let types = ids |> Array.map (fun id -> world.Atoms[id].Type)
                    {
                        AtomIds = ids
                        Family = getFamily (getText world ids)
                        IsFoldable = Experiment16.getMaxStem types >= settings.MinStem
                        State = state
                        NumExposed = numExposed
                    })
        { Chains = chains; Others = capped.Length - chains.Length }

    /// Results of one cycle.
    type private CycleResult =
        {
            /// At the end of the feast.
            Fed : Census

            /// Whether each chain counted at the end of the feast was
            /// still intact at the end of the stress phase.
            Survived : bool[]

            /// At the end of the stress phase.
            Stressed : Census
        }

    /// Results of one run.
    type private RunResult =
        {
            /// Each cycle's results.
            Cycles : CycleResult[]

            /// Sequences of all capped chains at the end, as text.
            FinalSequences : string[]
        }

    /// Runs one world through the cycles, calling the given function
    /// with each cycle's number when it ends.
    let private runOne settings variant seed onCycle =
        let mutable run = Run.create seed (getScenario settings variant)
        let cycles =
            [|
                for iCycle in 1 .. settings.NumCycles do
                    run <- Run.advance settings.Feast.NumSweeps run
                    let fed = getCensus settings run.World
                    run <- Run.advance settings.Stress.NumSweeps run
                    let stressed = getCensus settings run.World
                    let intact =
                        getStrands run.World
                            |> Array.where (isCapped run.World)
                            |> Array.map _.AtomIds
                            |> set
                    run <- Run.advance settings.Melt.NumSweeps run
                    onCycle iCycle
                    {
                        Fed = fed
                        Survived = fed.Chains |> Array.map (fun c -> intact.Contains c.AtomIds)
                        Stressed = stressed
                    }
            |]
        let finalSequences =
            getStrands run.World
                |> Array.where (isCapped run.World)
                |> Array.map (fun strand -> getText run.World strand.AtomIds)
        { Cycles = cycles; FinalSequences = finalSequences }

    /// Number of chains of the given family in the given census.
    let private count family (census : Census) =
        census.Chains |> Array.where (fun c -> c.Family = family) |> Array.length

    /// Share of the hairpin family in the given census, if either family
    /// has any chains.
    let private tryGetShare census =
        let h = count HairpinFamily census
        let n = count NonFormerFamily census
        if h + n > 0 then Some (float h / float (h + n)) else None

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
        printfn "Experiment 23: folding in a famine"
        printfn "Hairpin-former: %s (longest stem %d)" (toText settings.HairpinFormer) (Experiment16.getMaxStem settings.HairpinFormer)
        printfn "Non-former:     %s (longest stem %d)" (toText settings.NonFormer) (Experiment16.getMaxStem settings.NonFormer)
        printfn "%d runs of %d cycles of each variant; %d strands of each kind to start, at random places, among the standard world's free atoms"
            settings.NumRuns settings.NumCycles settings.NumSeeds
        printfn "Capped chains of %d atoms or more are counted by family: the hairpin family is caps around one run each of W, Y or Z, and X"
            settings.MinLength
        printfn "Each cycle: feast %s; stress %s; melt %s"
            (phaseText settings.Feast) (phaseText settings.Stress) (phaseText settings.Melt)
        printfn "Hydrolysis only under stress, of unprotected strong bonds; protected ones never hydrolyze"
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
                    (variant, seed), runOne settings variant seed onCycle)
                |> Map

        use writer = new StreamWriter(csvPath)
        writer.WriteLine("stress_hydrolysis,cycle,fed_hairpins,fed_non_formers,fed_foldable_hairpins,hairpins_copied,hairpins_folded,hairpins_single,non_formers_copied,non_formers_single,stressed_hairpins,stressed_non_formers,others,hairpin_share")
        let invariant (x : float) = x.ToString(Globalization.CultureInfo.InvariantCulture)
        let familyName = function HairpinFamily -> "hairpin" | NonFormerFamily -> "non-former"
        let stateName = function Copied -> "copied" | Folded -> "folded" | Single -> "single"
        for variant in settings.Variants do
            let runs = [| for seed in 0 .. settings.NumRuns - 1 -> results[variant, seed] |]
            printfn ""
            printfn "Stress hydrolysis %g" variant.StressHydrolysisRate
            printfn "        |------------- after the feast -------------|   |-- after the stress --|"
            printfn "cycle   hairpin / non   foldable   copied folded single   hairpin / non   share   other"
            printfn "                                   (hairpin family)"
            for iCycle in 0 .. settings.NumCycles - 1 do
                let cycle = runs |> Array.map (fun r -> r.Cycles[iCycle])
                let average f = cycle |> Array.averageBy (f >> float)
                let fh = average (fun c -> count HairpinFamily c.Fed)
                let fn = average (fun c -> count NonFormerFamily c.Fed)
                let foldable = average (fun c -> c.Fed.Chains |> Array.where (fun ch -> ch.Family = HairpinFamily && ch.IsFoldable) |> Array.length)
                let inState family state =
                    average (fun c -> c.Fed.Chains |> Array.where (fun ch -> ch.Family = family && ch.State = state) |> Array.length)
                let sh = average (fun c -> count HairpinFamily c.Stressed)
                let sn = average (fun c -> count NonFormerFamily c.Stressed)
                let o = average _.Stressed.Others
                let share = if sh + sn > 0.0 then sh / (sh + sn) else nan
                writer.WriteLine(
                    String.Join(",",
                        [ invariant variant.StressHydrolysisRate; string (iCycle + 1)
                          invariant fh; invariant fn; invariant foldable
                          invariant (inState HairpinFamily Copied); invariant (inState HairpinFamily Folded); invariant (inState HairpinFamily Single)
                          invariant (inState NonFormerFamily Copied); invariant (inState NonFormerFamily Single)
                          invariant sh; invariant sn; invariant o; invariant share ]))
                printfn "%5d   %6.2f / %5.2f   %6.2f    %5.2f  %5.2f  %5.2f    %6.2f / %5.2f   %5.2f  %6.1f"
                    (iCycle + 1) fh fn foldable
                    (inState HairpinFamily Copied) (inState HairpinFamily Folded) (inState HairpinFamily Single)
                    sh sn share o

                // survival through the stress, by family and state, pooled
            printfn "Survival through the stress, all cycles (unprotected strong bonds per chain at the end of the feast):"
            for family in [ HairpinFamily; NonFormerFamily ] do
                let cells =
                    states |> Array.map (fun state ->
                        let pairs =
                            runs
                                |> Array.collect (fun r ->
                                    r.Cycles |> Array.collect (fun c ->
                                        Array.zip c.Fed.Chains c.Survived))
                                |> Array.where (fun (ch, _) -> ch.Family = family && ch.State = state)
                        let nSurvived = pairs |> Array.where snd |> Array.length
                        if pairs.Length = 0 then sprintf "%s -" (stateName state)
                        else
                            sprintf "%s %d/%d = %.2f (%.1f exposed)"
                                (stateName state) nSurvived pairs.Length
                                (float nSurvived / float pairs.Length)
                                (pairs |> Array.averageBy (fun (ch, _) -> float ch.NumExposed)))
                printfn "   %-11s %s" (familyName family) (String.Join("; ", cells))

                // at the end, per run
            let final = runs |> Array.map (fun r -> r.Cycles[settings.NumCycles - 1].Stressed)
            let shares = final |> Array.choose tryGetShare
            let nAhead = final |> Array.where (fun c -> count HairpinFamily c > count NonFormerFamily c) |> Array.length
            let nBehind = final |> Array.where (fun c -> count HairpinFamily c < count NonFormerFamily c) |> Array.length
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

                // against the control, run for run
            let control = settings.Variants[0]
            if variant <> control then
                let pairs =
                    [| for seed in 0 .. settings.NumRuns - 1 do
                        let stressed = results[variant, seed].Cycles[settings.NumCycles - 1].Stressed |> tryGetShare
                        let controlled = results[control, seed].Cycles[settings.NumCycles - 1].Stressed |> tryGetShare
                        match stressed, controlled with
                            | Some s, Some c -> yield s - c
                            | _ -> () |]
                let nUp = pairs |> Array.where (fun d -> d > 0.0) |> Array.length
                let nDown = pairs |> Array.where (fun d -> d < 0.0) |> Array.length
                printfn "Against the control, run for run: hairpin share %+.3f on average; higher in %d runs, lower in %d; sign test p = %.3g (higher), %.3g (lower)"
                    (if pairs.Length > 0 then Array.average pairs else nan) nUp nDown
                    (Experiment17.getSignTestP nUp nDown)
                    (Experiment17.getSignTestP nDown nUp)

                // commonest sequences at the end
            printfn "Commonest capped chains at the end (all runs):"
            let sequences = runs |> Array.collect _.FinalSequences
            for family in [ HairpinFamily; NonFormerFamily ] do
                let common =
                    sequences
                        |> Array.where (fun text -> getFamily text = family)
                        |> Array.countBy id
                        |> Array.sortByDescending snd
                        |> Array.truncate settings.NumCommonSequences
                printfn "   %s family:" (familyName family)
                for text, n in common do
                    printfn "      %4d  %2d atoms  %s" n text.Length text

        printfn ""
        printfn "hairpin, non: capped chains of each family, per run. foldable: hairpin-family chains that"
        printfn "   could fold with a stem of %d pairs or more. copied: half or more of a chain's atoms paired with"
            settings.MinStem
        printfn "   another chain; folded: not copied, but a run of at least %d atoms paired within it; single: neither."
            (2 * settings.MinFoldedStem)
        printfn "share: hairpins / (hairpins + non-formers) after the stress, from the averages."
        printfn "other: shorter capped chains, after the stress."
        printfn "Results written to %s" (Path.GetFullPath csvPath)
