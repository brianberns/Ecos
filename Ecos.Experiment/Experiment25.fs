namespace Ecos.Experiment

open System
open System.IO
open System.Threading

open Ecos
open Ecos.Experiment.Experiment3

/// Experiment 25: stress after plenty and want. Does folding in a famine
/// protect a strand through a stress that follows it?
///
/// A single 160x160 world, like one of experiment 24's chambers, is
/// seeded with two hairpin-formers and either the standard world's
/// density of free atoms (a feast, which the strands copy until it runs
/// out) or none (a famine from the start). It runs through experiment
/// 24's cold/hot cycles, with no hydrolysis, until the end of a cold
/// phase. Then, at the same cold temperature, comes a stress:
/// unprotected strong bonds hydrolyze, while protected ones (one side of
/// a stack, in a paired stretch) don't. Each counted chain at the start
/// of the stress is followed, to see whether it stays intact, by its
/// state then (copied, folded or single).
///
/// Controls: the same with protection off, so that every bond hydrolyzes
/// at the same rate, whatever its state; and the hingeless mixed
/// hairpin-former of experiment 24m (at its best stiffness, 0.5), which
/// copies about as well but seldom folds in a famine.
module Experiment25 =

    /// A variant of the experiment.
    type Variant =
        {
            /// Short name.
            Name : string

            /// Experiment 24's settings for the strand, its free atom
            /// types, its bends and its cycles.
            Base : Experiment24.Settings

            /// Strong-bond bend stiffness (at atoms other than hinges).
            BendStiffness : float

            /// Whether protected bonds are spared during the stress.
            IsProtected : bool

            /// Number of free atoms of each type to start with.
            NumFoodEach : int

            /// Number of the cycle at the end of whose cold phase the
            /// stress begins. Earlier cycles run in full.
            StressCycle : int

            /// Number of independent runs (seeds).
            NumRuns : int
        }

    /// Settings for the experiment.
    type Settings =
        {
            /// Variants to run.
            Variants : Variant[]

            /// Temperature of the stress.
            StressTemperature : float

            /// Rate of hydrolysis of unprotected strong bonds during the
            /// stress.
            StressHydrolysisRate : float

            /// Times during the stress, in sweeps from its start, at
            /// which the chains are checked. The last is its end.
            SampleTimes : int[]
        }

    /// The hinged hairpin-former and the hingeless one, with and without
    /// protection, after a feast that has run out (at the twelfth cycle,
    /// when experiment 24h's best-fed chamber had about 5 free atoms of
    /// each type left) and in a famine from the start (at the third
    /// cycle, by when strands have settled). Famine worlds, with only
    /// two strands, are cheap, so they get many more runs: 8 runs left
    /// too few folded strands to compare the formers.
    let private defaultVariants =
        [|
            for numFood, stressCycle, numRuns, foodName in [ 120, 12, 8, "after a feast"; 0, 3, 96, "famine" ] do
                for isProtected in [ true; false ] do
                    let suffix = if isProtected then "" else ", unprotected"
                    {
                        Name = $"hinged, {foodName}{suffix}"
                        Base = Experiment24.standardSettings
                        BendStiffness = Scenario.defaultBendStiffness
                        IsProtected = isProtected
                        NumFoodEach = numFood
                        StressCycle = stressCycle
                        NumRuns = numRuns
                    }
                    {
                        Name = $"hingeless, {foodName}{suffix}"
                        Base = Experiment24.mixedSettings
                        BendStiffness = 0.5
                        IsProtected = isProtected
                        NumFoodEach = numFood
                        StressCycle = stressCycle
                        NumRuns = numRuns
                    }
        |]

    /// Default settings. At the stress's rate, each unprotected bond has
    /// about a 5% chance of breaking over the stress, so an unpaired
    /// 24-atom strand (23 bonds) about a 70% chance of being cut, while
    /// a folded one, with only its loop exposed, is cut far less often.
    let defaultSettings =
        {
            Variants = defaultVariants
            StressTemperature = 0.3
            StressHydrolysisRate = 2e-4
            SampleTimes = [| 0; 5_000; 10_000; 20_000; 50_000 |]
        }

    /// Width of the world.
    let private worldSize = 160

    /// The scenario of the given variant: experiment 24's (scenario C's
    /// bonds, with the variant's bends and cycles), in a single chamber
    /// with no walls.
    let getScenario variant =
        let settings = variant.Base
        let build parms (rng : Random) =
            let world = World.createSquare worldSize
            let n = settings.HairpinFormer.Length
            let world =
                (world, [ 1 .. settings.NumSeeds ])
                    ||> Seq.fold (fun world _ ->
                        let rec loop () =
                            let x = 2 * rng.Next(-worldSize / 4, worldSize / 4 - n)
                            let y = 2 * rng.Next(-worldSize / 4, worldSize / 4)
                            Build.tryAddStrand parms x y settings.HairpinFormer world
                                |> Option.defaultWith loop
                        loop ())
            let template = Array.map _.Id world.Atoms
            let world =
                (world, settings.FoodTypes)
                    ||> Array.fold (fun world atomType ->
                        (world, [ 1 .. variant.NumFoodEach ])
                            ||> Seq.fold (fun world _ ->
                                let rec loop () =
                                    let loc = Point.create (2 * rng.Next(worldSize / 2), 2 * rng.Next(worldSize / 2))
                                    match World.tryAddAtom atomType loc world with
                                        | Some (world, _) -> world
                                        | None -> loop ()
                                loop ()))
            world, template
        let scenario = Experiment24.getScenario settings variant.BendStiffness
        { scenario with
            Name = $"Stress after plenty and want, {variant.Name}"
            Build = build }

    /// The scenarios of the given settings' variants, before the stress.
    let getScenarios settings =
        settings.Variants
            |> Array.where _.IsProtected   // protection matters only during the stress
            |> Array.map getScenario

    /// The given variant's scenario during the stress.
    let private getStressScenario settings variant =
        let rate = settings.StressHydrolysisRate
        getScenario variant
            |> Scenario.withHydrolysisRate rate
            |> Scenario.withProtectedHydrolysisRate
                (if variant.IsProtected then 0.0 else rate)

    /// Whether every strong bond along the given chain is still present.
    let private isIntact (world : World) (ids : AtomId[]) =
        let strong = world.BondTypeMap[Strong]
        ids
            |> Array.pairwise
            |> Array.forall (fun (a, b) ->
                (BondMap.getBonds a strong).Contains b)

    /// One chain followed through the stress.
    type private Followed =
        {
            /// State of the chain at the start of the stress: 0 if
            /// copied, 1 if folded, 2 if single.
            State : int

            /// Whether it was intact at each sample time.
            Intact : bool[]
        }

    /// Result of one run.
    type private Outcome =
        {
            /// Chains followed through the stress.
            Followed : Followed[]

            /// Free W and X atoms, per type, at the start of the stress.
            Food : float

            /// Counted chains at the end of the stress, new copies
            /// included.
            NumAtEnd : int
        }

    /// Runs one world up to the stress, and then through it.
    let private runOne settings variant seed =
        let baseSettings = variant.Base
        let period = baseSettings.Cold.NumSweeps + baseSettings.Hot.NumSweeps
        let run =
            Run.create seed (getScenario variant)
                |> Run.advance ((variant.StressCycle - 1) * period + baseSettings.Cold.NumSweeps)
        let chains = Experiment24.getChains baseSettings run.World
        let food =
            let strong = run.World.BondTypeMap[Strong]
            let n =
                run.World.Atoms
                    |> Array.where (fun atom ->
                        (atom.Type = AtomType.W || atom.Type = AtomType.X)
                            && Set.isEmpty (BondMap.getBonds atom.Id strong))
                    |> Array.length
            float n / 2.0
        let mutable run =
            { run with
                Scenario = getStressScenario settings variant
                Schedule = Constant settings.StressTemperature }
        let intact =
            [|
                let mutable elapsed = 0
                for t in settings.SampleTimes do
                    run <- Run.advance (t - elapsed) run
                    elapsed <- t
                    chains |> Array.map (fun c -> isIntact run.World c.AtomIds)
            |]
        {
            Followed =
                chains |> Array.mapi (fun i c ->
                    {
                        State = c.State
                        Intact = intact |> Array.map (fun sample -> sample[i])
                    })
            Food = food
            NumAtEnd = (Experiment24.getChains baseSettings run.World).Length
        }

    /// Names of the chain states.
    let private stateNames = [| "copied"; "folded"; "single" |]

    /// Runs the experiment, prints tables of results, and writes them
    /// to the given CSV file.
    let run (settings : Settings) (csvPath : string) =
        let combos =
            [|
                for iVariant in 0 .. settings.Variants.Length - 1 do
                    for seed in 0 .. settings.Variants[iVariant].NumRuns - 1 do
                        iVariant, seed
            |]

        let toText = Array.map string >> String.concat " "
        printfn "Experiment 25: stress after plenty and want"
        printfn "A %dx%d world with two hairpin-formers at random places" worldSize worldSize
        for variant in settings.Variants do
            let b = variant.Base
            printfn "Variant %s: %s, stiffness %g%s; %d free atoms of each type (%s); stress at the end of cold phase %d; %d runs"
                variant.Name (toText b.HairpinFormer) variant.BendStiffness
                (b.HingeStiffness |> Option.map (sprintf ", hinges (U, V) %g") |> Option.defaultValue "")
                variant.NumFoodEach (toText b.FoodTypes) variant.StressCycle variant.NumRuns
        let b = settings.Variants[0].Base
        printfn "Cycles of cold (kT %g, %d sweeps) and hot (kT %g, %d sweeps) with no hydrolysis"
            b.Cold.Temperature b.Cold.NumSweeps b.Hot.Temperature b.Hot.NumSweeps
        printfn "Stress: kT %g, unprotected strong bonds hydrolyze at %g; protected ones at 0 (or the same, if unprotected)"
            settings.StressTemperature settings.StressHydrolysisRate
        printfn "Chains counted at the start of the stress are checked at %s sweeps into it"
            (settings.SampleTimes |> Array.map string |> String.concat ", ")
        printParameters (getStressScenario settings settings.Variants[0]).Parameters
        printfn ""

        let nFinished = ref 0
        let stopwatch = Diagnostics.Stopwatch.StartNew()
        let results =
            combos
                |> Array.Parallel.map (fun (iVariant, seed) ->
                    let outcome = runOne settings settings.Variants[iVariant] seed
                    let n = Interlocked.Increment(&nFinished.contents)
                    printfn "Finished %d of %d worlds (%.0f min)" n combos.Length stopwatch.Elapsed.TotalMinutes
                    (iVariant, seed), outcome)
                |> Map

        use writer = new StreamWriter(csvPath)
        writer.WriteLine(
            "variant,seed,state,"
                + (settings.SampleTimes |> Array.map (sprintf "intact_%d") |> String.concat ","))
        for (iVariant, seed), outcome in Map.toSeq results do
            for f in outcome.Followed do
                writer.WriteLine(
                    String.Join(",",
                        [
                            yield "\"" + settings.Variants[iVariant].Name + "\""
                            yield string seed
                            yield stateNames[f.State]
                            for b in f.Intact -> if b then "1" else "0"
                        ]))

        let row label (followed : Followed[]) =
            let n = followed.Length
            let fracs =
                [| for iSample in 0 .. settings.SampleTimes.Length - 1 ->
                    if n = 0 then "      -"
                    else
                        let k = followed |> Array.where (fun f -> f.Intact[iSample]) |> Array.length
                        sprintf "%7.2f" (float k / float n) |]
                    |> String.concat ""
            printfn "  %-8s %5d   %s" label n fracs

        for iVariant in 0 .. settings.Variants.Length - 1 do
            let variant = settings.Variants[iVariant]
            let outcomes = [| for seed in 0 .. variant.NumRuns - 1 -> results[iVariant, seed] |]
            let followed = outcomes |> Array.collect _.Followed
            printfn ""
            printfn "Variant %s: at the start of the stress, %.1f free atoms of each type (W and X averaged) and %.2f chains per run; %.2f chains at its end"
                variant.Name (outcomes |> Array.averageBy _.Food)
                (float followed.Length / float outcomes.Length)
                (outcomes |> Array.averageBy (_.NumAtEnd >> float))
            printfn "  state        n   intact at %s"
                (settings.SampleTimes |> Array.map (fun t -> sprintf "%6dk" (t / 1000)) |> String.concat "")
            row "all" followed
            for state in 0 .. 2 do
                row stateNames[state] (followed |> Array.where (fun f -> f.State = state))

        printfn ""
        printfn "copied: half or more of a chain's atoms paired with another chain; folded: not copied, but a run of"
        printfn "   at least %d atoms paired within it; single: neither. All as of the start of the stress." (2 * b.MinFoldedStem)
        printfn "intact: every strong bond along the chain still present."
        printfn "Results written to %s" (Path.GetFullPath csvPath)
