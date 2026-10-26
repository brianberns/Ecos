namespace Ecos.Experiment

open System
open System.IO
open System.Threading

open Ecos
open Ecos.Experiment.Experiment3

/// Experiment 19: does folding protect a strand through an adverse
/// phase? A living world (that of experiment 16) is seeded with
/// duplexes, each a strand paired with its copy: some of experiment 17's
/// hairpin-formers, and as many of its non-formers. A hot phase melts
/// the duplexes, and their strands drift apart. Then, at a warm
/// temperature, comes an adverse phase, in which unprotected strong
/// bonds hydrolyze quickly, while protected ones (each one side of a
/// stack, in a double-stranded stretch) hydrolyze slowly or not at all.
///
/// The idea: a hairpin-former's arms are tethered, so it can fold up in
/// place and protect itself, while a non-former, whose partner has
/// drifted away, is left single and exposed, until it re-anneals or is
/// copied.
///
/// Each strand of each seeded duplex is followed separately. At the
/// start of the adverse phase, and at times through it, each is
/// recorded as intact (all of its strong bonds present), folded (at
/// least two weak bonds within it), and with the fraction of its strong
/// bonds that are protected.
module Experiment19 =

    /// A variant of the experiment.
    type Variant =
        {
            /// Strong-bond bend stiffness, with the standard bend limit
            /// (right angles).
            BendStiffness : float

            /// Rate of hydrolysis of protected strong bonds during the
            /// adverse phase.
            ProtectedHydrolysisRate : float
        }

    /// Settings for the experiment.
    type Settings =
        {
            /// The strand that can fold into a hairpin.
            HairpinFormer : AtomType[]

            /// A strand with the same atoms that can't fold.
            NonFormer : AtomType[]

            /// Number of duplexes of each kind to start with.
            NumDuplexes : int

            /// Temperature of the hot phase.
            HotTemperature : float

            /// Length of the hot phase, in sweeps.
            HotSweeps : int

            /// Temperature of the adverse phase.
            AdverseTemperature : float

            /// Rate of hydrolysis of unprotected strong bonds during the
            /// adverse phase. (Before it, the living world's own rate.)
            AdverseHydrolysisRate : float

            /// Times during the adverse phase, in sweeps from its start,
            /// at which to record the strands. The last is its end.
            SampleTimes : int[]

            /// Variants to run.
            Variants : Variant[]

            /// Number of independent runs (seeds) of each variant.
            NumRuns : int

            /// Seed of the first run. The others follow in order.
            FirstSeed : int

            /// Whether the world has the living world's free atoms. Without
            /// them, nothing can copy a single strand.
            HasFreeAtoms : bool
        }

    /// Default settings: experiment 17's long contest, a hot phase as
    /// long as scenario B's, and an adverse phase at a temperature at
    /// which a lone hairpin is folded about 90% of the time (experiment
    /// 18), harsh enough to cut most unprotected ten-atom strands. With
    /// and without protection, at the standard stiffness and a softer
    /// one, at which folds start more readily.
    let defaultSettings =
        {
            HairpinFormer = Experiment17.longHairpinFormer
            NonFormer = Experiment17.longNonFormer
            NumDuplexes = 8
            HotTemperature = 3.0
            HotSweeps = 10_000
            AdverseTemperature = 0.4
            AdverseHydrolysisRate = 0.05
            SampleTimes = [| 0; 100; 250; 500; 1000; 2000 |]
            Variants =
                [|
                    for kappa in [ 1.5; 0.5 ] do
                        for protectedRate in [ 0.05; 0.0 ] do
                            { BendStiffness = kappa; ProtectedHydrolysisRate = protectedRate }
                |]
            NumRuns = 8
            FirstSeed = 0
            HasFreeAtoms = true
        }

    /// Settings for a gentler and longer adverse phase, in which strands
    /// last long enough to fold: at the default rate, an unprotected
    /// strand's half-life (about 250 sweeps) is far shorter than the time
    /// a hairpin-former takes to fold.
    let gentleSettings =
        { defaultSettings with
            AdverseHydrolysisRate = 0.005
            SampleTimes = [| 0; 500; 1000; 2000; 5000; 10000 |]
            Variants =
                defaultSettings.Variants
                    |> Array.map (fun variant ->
                        if variant.ProtectedHydrolysisRate > 0.0 then
                            { variant with ProtectedHydrolysisRate = 0.005 }
                        else variant) }

    /// Settings for a slow adverse phase, which cuts each strand once at
    /// most, so to speak: a non-former, with all nine of its bonds
    /// exposed, has about a 70% chance of being cut over the phase, while
    /// a hairpin-former that folds early has only its three loop bonds
    /// exposed for most of it. At the softer stiffness only, since at the
    /// standard one hairpins hardly fold at this temperature.
    let slowSettings =
        { defaultSettings with
            AdverseHydrolysisRate = 0.0005
            SampleTimes = [| 0; 2000; 5000; 10000; 20000; 50000 |]
            Variants =
                [|
                    for protectedRate in [ 0.0005; 0.0 ] do
                        { BendStiffness = 0.5; ProtectedHydrolysisRate = protectedRate }
                |] }

    /// Settings for the slow adverse phase in a starved world, with no
    /// free atoms, so that nothing can copy a single strand: a non-former
    /// is protected only if it finds its partner again, while a
    /// hairpin-former can fold on its own.
    let starvedSettings =
        { slowSettings with HasFreeAtoms = false }

    /// The starved settings again, on more runs and with seeds that they
    /// don't use, to confirm their result.
    let confirmationSettings =
        { starvedSettings with NumRuns = 32; FirstSeed = 8 }

    /// A hairpin-former with an 8-pair stem (counting the caps' pair),
    /// long enough to stay folded at kT 0.5, at which free atoms can
    /// hardly copy a single strand (experiment 20).
    let windowHairpinFormer =
        [|
            AtomType.P
            yield! Array.replicate 7 AtomType.W
            AtomType.Y; AtomType.Y
            yield! Array.replicate 7 AtomType.X
            AtomType.Q
        |]

    /// The window hairpin-former's atoms, in an order that pairs as
    /// little as possible with it and its copy (at most two pairs in a
    /// row), and can fold only into a 2-pair stem, too short to hold at
    /// kT 0.5. (No order of these atoms does better on both counts.)
    let windowNonFormer =
        [|
            AtomType.P
            AtomType.W; AtomType.Y; AtomType.W; AtomType.W; AtomType.X; AtomType.X
            AtomType.W; AtomType.X; AtomType.X; AtomType.W; AtomType.X; AtomType.W
            AtomType.Y; AtomType.X; AtomType.W; AtomType.X
            AtomType.Q
        |]

    /// Settings for an adverse phase in the window found by experiment
    /// 20: at kT 0.5, a folded 8-pair stem holds, but free atoms can't
    /// copy a single strand, so the living world, free atoms and all, is
    /// in effect starved. Hydrolysis is slow, since a long hairpin-former
    /// takes long to fold; a non-folder, with all 17 of its bonds
    /// exposed, has about a 30% chance of escaping it. At the standard
    /// stiffness.
    let windowSettings =
        { defaultSettings with
            HairpinFormer = windowHairpinFormer
            NonFormer = windowNonFormer
            AdverseTemperature = 0.5
            AdverseHydrolysisRate = 0.00025
            SampleTimes = [| 0; 5000; 10000; 20000; 50000 |]
            Variants =
                [|
                    for protectedRate in [ 0.00025; 0.0 ] do
                        { BendStiffness = 1.5; ProtectedHydrolysisRate = protectedRate }
                |]
            NumRuns = 16 }

    /// The given variant of the living world, at the given temperature,
    /// with the given rates of hydrolysis.
    let private getParameters variant kT rate protectedRate =
        let scenario =
            Experiment4.getScenario Experiment4.livingWorldSettings 15.0
                |> Scenario.withBend Strong Bend.rightAngle variant.BendStiffness
                |> Scenario.withHydrolysisRate rate
                |> Scenario.withProtectedHydrolysisRate protectedRate
        { scenario.Parameters with
            GetTemperature = Parameters.getUniformTemperature kT }

    /// The copy of the given strand, read from P to Q.
    let private copyOf (strand : AtomType[]) =
        strand |> Array.rev |> Array.map complement

    /// Adds a duplex of the given strand and its copy at a random empty
    /// place: the strand along a row, and the copy in the row above,
    /// each atom paired with the one below it. Answers the world and the
    /// atom IDs of the two strands.
    let private addDuplex parms (rng : Random) (strand : AtomType[]) (world : World) =
        let n = strand.Length
        let rec loop () =
            let x = 2 * rng.Next(-35, 36 - n)
            let y = 2 * rng.Next(-35, 35)
            let first = world.Atoms.Length
            let worldOpt =
                Build.tryAddStrand parms x y strand world
                    |> Option.bind (
                            // the copy, laid out the other way, so that each
                            // atom faces its complement
                        Build.tryAddStrand parms x (y + 2) (Array.map complement strand))
            match worldOpt with
                | Some world ->
                    let idsA = [| first .. first + n - 1 |]
                    let idsB = [| first + n .. first + 2 * n - 1 |]
                    let world =
                        (world, Array.zip idsA idsB)
                            ||> Array.fold (fun world (idA, idB) ->
                                World.addWeakBond parms idA idB world)
                    world, idsA, idsB
                | None -> loop ()
        loop ()

    /// A seeded strand: its kind, and its atoms in order.
    type private Seed =
        {
            /// Whether it can fold.
            IsHairpinFormer : bool

            /// Its atoms, in order along the strand.
            AtomIds : AtomId[]
        }

    /// The state of a seeded strand at some moment.
    type private SeedState =
        {
            /// All of its strong bonds are present.
            Intact : bool

            /// At least two weak bonds within it.
            Folded : bool

            /// Fraction of its strong bonds that are one side of a stack
            /// (their atoms are paired with atoms that are strongly bonded
            /// to each other), and so protected.
            Protected : float
        }

    /// The state of the given seeded strand in the given world.
    let private getState (world : World) seed =
        let strong = world.BondTypeMap[Strong]
        let weak = world.BondTypeMap[Weak]
        let bonds = Array.pairwise seed.AtomIds
        let isStacked (a, b) =
            BondMap.getBonds a weak
                |> Set.exists (fun a' ->
                    BondMap.getBonds b weak
                        |> Set.exists (fun b' -> BondMap.areBonded a' b' strong))
        let atoms = set seed.AtomIds
        let inner =
            seed.AtomIds
                |> Array.sumBy (fun id ->
                    BondMap.getBonds id weak |> Set.intersect atoms |> Set.count)
        {
            Intact = bonds |> Array.forall (fun (a, b) -> BondMap.areBonded a b strong)
            Folded = inner / 2 >= 2
            Protected =
                let present = bonds |> Array.where (fun (a, b) -> BondMap.areBonded a b strong)
                if present.Length = 0 then 0.0
                else
                    float (present |> Array.where isStacked).Length
                        / float present.Length
        }

    /// Builds the given variant's world, as it is at the start of the
    /// adverse phase: the living world's free atoms (or none), and the
    /// seeded duplexes, melted by the hot phase. Answers the world and
    /// the seeded strands.
    let private buildMelted settings variant (rng : Random) =
        let baseRate =
            (Experiment4.getScenario Experiment4.livingWorldSettings 15.0)
                .Parameters.BondTypeParameterMap[Strong].HydrolysisRate
        let hot = getParameters variant settings.HotTemperature baseRate baseRate
        let world =
            if settings.HasFreeAtoms then
                (Experiment4.getScenario Experiment4.livingWorldSettings 15.0).Build hot rng |> fst
            else
                World.createSquare 160
        let world, seeds =
            ((world, []), [ for _ in 1 .. settings.NumDuplexes do yield true; yield false ])
                ||> List.fold (fun (world, seeds) isHairpinFormer ->
                    let strand =
                        if isHairpinFormer then settings.HairpinFormer else settings.NonFormer
                    let world, idsA, idsB = addDuplex hot rng strand world
                    world,
                        { IsHairpinFormer = isHairpinFormer; AtomIds = idsA }
                            :: { IsHairpinFormer = isHairpinFormer; AtomIds = Array.rev idsB }   // in order along the copy
                            :: seeds)
        World.sweep hot rng settings.HotSweeps world, List.toArray seeds

    /// The adverse phase of the given variant, for the desktop app: its
    /// world is built as it is at the start of the phase (see
    /// buildMelted), with the seeded hairpin-formers (and their copies,
    /// which can fold too) as its template, so that they can be told
    /// apart from the non-formers.
    let getScenario settings variant =
        let scenario =
            Experiment4.getScenario Experiment4.livingWorldSettings 15.0
                |> Scenario.withBend Strong Bend.rightAngle variant.BendStiffness
                |> Scenario.withHydrolysisRate settings.AdverseHydrolysisRate
                |> Scenario.withProtectedHydrolysisRate variant.ProtectedHydrolysisRate
        let build _ rng =
            let world, seeds = buildMelted settings variant rng
            let hairpinFormers =
                seeds
                    |> Array.where _.IsHairpinFormer
                    |> Array.collect _.AtomIds
            world, hairpinFormers
        let where = if settings.HasFreeAtoms then "living world" else "starved"
        { scenario with
            Name = $"Adverse phase at kT {settings.AdverseTemperature}, {where}, protected rate {variant.ProtectedHydrolysisRate}"
            Build = build
            Schedule = Constant settings.AdverseTemperature }

    /// The experiment's scenarios: one for each variant.
    let getScenarios settings =
        settings.Variants |> Array.map (getScenario settings)

    /// Runs one world. Answers each seed's kind, and its state at each
    /// sample time of the adverse phase.
    let private runOne settings variant seed =
        let rng = Random(seed)
        let adverse =
            getParameters variant settings.AdverseTemperature
                settings.AdverseHydrolysisRate variant.ProtectedHydrolysisRate
        let world, seeds = buildMelted settings variant rng

            // each seeded strand's strong bonds, as (smaller ID, larger ID)
        let seedOfBond =
            dict [
                for iSeed, seed in Array.indexed seeds do
                    for a, b in Array.pairwise seed.AtomIds do
                        (min a b, max a b), iSeed
            ]

            // during the adverse phase, how often each seeded strand loses a
            // bond, and how often a bond it lost forms again (is repaired)
        let breaks = Array.zeroCreate<int> seeds.Length
        let repairs = Array.zeroCreate<int> seeds.Length
        let broken = Collections.Generic.HashSet<AtomId * AtomId>()
        let onEvent event _ =
            match event with
                | BreakBond (Strong, a, b) ->
                    let key = min a b, max a b
                    match seedOfBond.TryGetValue(key) with
                        | true, iSeed ->
                            breaks[iSeed] <- breaks[iSeed] + 1
                            broken.Add(key) |> ignore
                        | _ -> ()
                | CreateBond (Strong, a, b) ->
                    let key = min a b, max a b
                    if broken.Remove(key) then
                        let iSeed = seedOfBond[key]
                        repairs[iSeed] <- repairs[iSeed] + 1
                | _ -> ()

            // the adverse phase
        let mutable world = world
        let mutable elapsed = 0
        let states =
            [|
                for time in settings.SampleTimes do
                    world <- World.sweepWith adverse rng (time - elapsed) onEvent world
                    elapsed <- time
                    seeds |> Array.map (getState world)
            |]
        seeds |> Array.map _.IsHairpinFormer, states, breaks, repairs

    /// Runs the experiment, prints a table of results, and writes them
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
                    let result = runOne settings variant seed
                    let n = Interlocked.Increment(&nDone)
                    eprintfn "Finished run %d of %d" n combos.Length
                    variant, result)
                |> Array.groupBy fst
                |> Array.map (fun (variant, group) -> variant, Array.map snd group)
                |> Map

        let toText = Array.map string >> String.concat " "
        printfn "Experiment 19: folding through an adverse phase"
        printfn "Hairpin-former: %s; non-former: %s; %d duplexes of each (strand and copy) per run"
            (toText settings.HairpinFormer) (toText settings.NonFormer) settings.NumDuplexes
        printfn "Living world, strong barrier 15; hot phase kT %g for %d sweeps; then adverse phase kT %g, unprotected hydrolysis %g"
            settings.HotTemperature settings.HotSweeps settings.AdverseTemperature settings.AdverseHydrolysisRate
        printfn "%d runs of each variant (seeds %d to %d)%s" settings.NumRuns
            settings.FirstSeed (settings.FirstSeed + settings.NumRuns - 1)
            (if settings.HasFreeAtoms then "" else "; no free atoms")

        use writer = new StreamWriter(csvPath)
        writer.WriteLine("kappa,protected_rate,kind,time,intact,folded,protected")
        let invariant (x : float) = x.ToString(Globalization.CultureInfo.InvariantCulture)
        for variant in settings.Variants do
            let runs = results[variant]
            printfn ""
            printfn "Strong bend stiffness %g, protected hydrolysis rate %g in the adverse phase"
                variant.BendStiffness variant.ProtectedHydrolysisRate
            printfn "                 |------- hairpin-formers -------|   |-------- non-formers --------|"
            printfn "  sweeps         intact   folded   protected        intact   folded   protected"
            for iTime, time in Array.indexed settings.SampleTimes do
                let summarize isHairpinFormer =
                    let states =
                        runs
                            |> Array.collect (fun (kinds, states, _, _) ->
                                Array.zip kinds states[iTime]
                                    |> Array.where (fun (kind, _) -> kind = isHairpinFormer)
                                    |> Array.map snd)
                    let intact = states |> Array.averageBy (fun s -> if s.Intact then 1.0 else 0.0)
                    let folded = states |> Array.averageBy (fun s -> if s.Folded then 1.0 else 0.0)
                    let prot = states |> Array.averageBy _.Protected
                    writer.WriteLine(
                        String.Join(",",
                            [ invariant variant.BendStiffness; invariant variant.ProtectedHydrolysisRate
                              (if isHairpinFormer then "hairpin" else "non-former"); string time
                              invariant intact; invariant folded; invariant prot ]))
                    sprintf "%6.2f   %6.2f   %9.2f" intact folded prot
                printfn "  %6d      %s      %s" time (summarize true) (summarize false)

                // repairs of each kind's bonds over the adverse phase
            for isHairpinFormer in [ true; false ] do
                let counts =
                    runs
                        |> Array.collect (fun (kinds, _, breaks, repairs) ->
                            Array.zip3 kinds breaks repairs
                                |> Array.where (fun (kind, _, _) -> kind = isHairpinFormer))
                let nBreaks = counts |> Array.sumBy (fun (_, n, _) -> n)
                let nRepairs = counts |> Array.sumBy (fun (_, _, n) -> n)
                printfn "  %s: %d bonds broken in the adverse phase, %d of them formed again (%.0f%%)"
                    (if isHairpinFormer then "Hairpin-formers" else "Non-formers")
                    nBreaks nRepairs (100.0 * float nRepairs / float (max 1 nBreaks))

                // per run, the fraction of each kind's strands intact at the end
            let survival isHairpinFormer (kinds : bool[], states : SeedState[][], _, _) =
                Array.zip kinds states[states.Length - 1]
                    |> Array.where (fun (kind, _) -> kind = isHairpinFormer)
                    |> Array.averageBy (fun (_, state) -> if state.Intact then 1.0 else 0.0)
            let meanAndError (xs : float[]) =
                let mean = Array.average xs
                let sd = sqrt ((xs |> Array.sumBy (fun x -> (x - mean) ** 2.0)) / float (xs.Length - 1))
                mean, sd / sqrt (float xs.Length)
            let hairpins = runs |> Array.map (survival true)
            let nonFormers = runs |> Array.map (survival false)
            let differences = Array.map2 (-) hairpins nonFormers
            let mH, eH = meanAndError hairpins
            let mN, eN = meanAndError nonFormers
            let mD, eD = meanAndError differences
            let nAhead = differences |> Array.where (fun d -> d > 0.0) |> Array.length
            let nBehind = differences |> Array.where (fun d -> d < 0.0) |> Array.length
            printfn "  Intact at the end, per run: hairpin-formers %.3f +/- %.3f, non-formers %.3f +/- %.3f"
                mH eH mN eN
            printfn "  Difference per run %.3f +/- %.3f; hairpin-formers ahead in %d runs, behind in %d, tied in %d; one-sided sign test p = %.3g"
                mD eD nAhead nBehind (runs.Length - nAhead - nBehind)
                (Experiment17.getSignTestP nAhead nBehind)

        printfn ""
        printfn "Fractions of seeded strands (both strands of each duplex, followed separately)."
        printfn "intact: all of its strong bonds present. folded: at least two weak bonds within it."
        printfn "protected: mean fraction of its present strong bonds that are one side of a stack."
        printfn "formed again: a seeded strand's bond that broke and then formed again between the same atoms, as a repair."
        printfn "Per-run figures are means over runs, with standard errors between runs."
        printfn "Results written to %s" (Path.GetFullPath csvPath)
