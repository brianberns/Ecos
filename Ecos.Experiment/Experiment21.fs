namespace Ecos.Experiment

open System
open System.IO
open System.Threading

open Ecos
open Ecos.Experiment.Experiment3

/// Experiment 21: hairpins in a vent world. With one-sided pairing, a
/// folded hairpin can't be copied, since each atom of its stem has its
/// one partner within the strand, but its stem is stacked, and so
/// protected from hydrolysis. A strand that can fold therefore trades
/// copying for survival. Is there a place in a vent world where that
/// pays?
///
/// The living world (scenario B), seeded with equal numbers of
/// experiment 19's 18-atom strands that can fold into a hairpin with an
/// 8-pair stem, and of strands of the same atoms that can't, is heated
/// by a vent along its seam. Near the vent, each pulse melts duplexes,
/// and their strands come apart. Farther out, a pulse reaches only the
/// window of experiment 20, in which a folded hairpin holds but free
/// atoms can hardly copy a single strand; farther still, it hardly
/// warms anything. Between pulses, the whole world is cold, for
/// copying.
///
/// The idea: after a pulse near the vent, a strand that can fold
/// protects itself on its own, while one that can't is exposed until it
/// is copied or finds its copy again. Away from the vent, strands stay
/// paired, and a strand that can't fold may do better, since it is
/// never tied up in a hairpin.
///
/// Chains are counted, by zone, at the end of each pulse, and folding
/// at the end of each cold phase.
module Experiment21 =

    /// A variant of the experiment.
    type Variant =
        {
            /// Strong-bond bend stiffness, with the standard bend limit
            /// (right angles).
            BendStiffness : float

            /// Rate of hydrolysis of protected strong bonds: those that
            /// are one side of a stack.
            ProtectedHydrolysisRate : float
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
            /// Copies slip, so the seeded strands' families soon include
            /// mutants with deletions (experiment 21's first run); a
            /// mutant that can still fold is counted with the strands
            /// that can.
            MinStem : int

            /// The vent.
            Vent : Vent

            /// Strong bond barrier, and its reduction per stack, so that
            /// a fully templated join has none.
            StrongBarrier : float

            /// Rate of hydrolysis of unprotected strong bonds.
            HydrolysisRate : float

            /// Pulse temperatures (kT) that divide the world into zones:
            /// below the first, a pulse hardly warms anything; between
            /// them is the window of experiment 20; above the second, a
            /// pulse melts duplexes.
            ZoneTemperatures : float * float

            /// Variants to run.
            Variants : Variant[]

            /// Number of independent runs (seeds) of each variant.
            NumRuns : int

            /// Number of pulses (cycles) to run.
            NumCycles : int
        }

    /// Default settings.
    ///
    /// - The vent: a pulse of kT 1.2 at the vent, falling to the cold
    ///   temperature, 0.3, halfway around the world. Pulses reach 0.7, at
    ///   which long duplexes melt (experiment 11), within 44 sites of the
    ///   vent, and 0.4, below which nothing in experiment 20 melted or
    ///   was copied, within 71.
    /// - Scenario C's durations, which one-sided copying needs: 200,000
    ///   sweeps cold and 100,000 hot.
    /// - Scenario C's strong bond barrier, so that nothing arises by
    ///   chance: at scenario B's, chance strands would compete with the
    ///   seeded ones.
    /// - Hydrolysis slow enough for 18-atom strands to multiply: a strand
    ///   exposed for a whole cycle would be cut roughly one time in four.
    /// - The standard stiffness and a softer one, at which folds start
    ///   more readily, each with full protection, which gives a folded
    ///   hairpin its advantage. (The first run, with barrier 16 and
    ///   without protection too, found no difference between zones
    ///   either way.)
    let defaultSettings =
        {
            HairpinFormer = Experiment19.windowHairpinFormer
            NonFormer = Experiment19.windowNonFormer
            NumSeeds = 8
            MinLength = 10
            MinStem = 6
            Vent =
                {
                    Scenario.seamVent with
                        HotTemperature = 1.2
                        FloorTemperature = 0.3
                        ColdSweeps = 200_000
                        HotSweeps = 100_000
                }
            StrongBarrier = 40.0
            HydrolysisRate = 1e-5
            ZoneTemperatures = 0.4, 0.7
            Variants =
                [|
                    for kappa in [ 1.5; 0.5 ] do
                        { BendStiffness = kappa; ProtectedHydrolysisRate = 0.0 }
                |]
            NumRuns = 12
            NumCycles = 12   // about 75 minutes on 24 cores
        }

    /// The zones of the world, by how hot a pulse gets there.
    type Zone =
        | Near
        | Window
        | Far

    /// All zones, nearest the vent first.
    let private zones = [| Near; Window; Far |]

    /// Name of the given zone.
    let private getZoneName = function
        | Near -> "near"
        | Window -> "window"
        | Far -> "far"

    /// Zone of the given location.
    let private getZone settings (pt : Point) =
        let low, high = settings.ZoneTemperatures
        let kT = Vent.getHotTemperature settings.Vent pt
        if kT >= high then Near
        elif kT >= low then Window
        else Far

    /// Capped chains of each kind in one zone.
    type private Count =
        {
            /// Chains that can fold into a hairpin.
            Hairpins : int

            /// Chains that can't.
            NonFormers : int

            /// Capped chains too short to count as either.
            Others : int
        }

    /// What a world contains at the end of a cycle.
    type private CycleResult =
        {
            /// Capped chains of each kind, by zone.
            Counts : Map<Zone, Count>

            /// Chains that can fold at the end of the cycle's cold
            /// phase.
            ColdHairpins : int

            /// Those of them that were folded then: at least two weak
            /// bonds within the chain.
            Folded : int
        }

    /// Kind of a capped chain.
    type private Kind =
        | HairpinFormer
        | NonFormer
        | Short

    /// Sorts the capped chains in the given world by kind and zone,
    /// placing each chain by its middle atom.
    let private getCensus settings (world : World) =
        let getKind (types : AtomType[]) =
            if types.Length < settings.MinLength then Short
            elif Experiment16.getMaxStem types >= settings.MinStem then HairpinFormer
            else NonFormer
        let weak = world.BondTypeMap[Weak]
        let chains =
            getStrands world
                |> Array.where (isCapped world)
        let isFolded (strand : Strand) =
            let atoms = set strand.AtomIds
            let inner =
                strand.AtomIds
                    |> Array.sumBy (fun id ->
                        BondMap.getBonds id weak
                            |> Set.intersect atoms
                            |> Set.count)
            inner / 2 >= 2   // each bond counted from both ends
        let classified =
            chains
                |> Array.map (fun strand ->
                    let kind =
                        strand.AtomIds
                            |> Array.map (fun id -> world.Atoms[id].Type)
                            |> getKind
                    let middle = strand.AtomIds[strand.AtomIds.Length / 2]
                    let zone = getZone settings world.Atoms[middle].Location
                    zone, kind, strand)
        let counts =
            zones
                |> Array.map (fun zone ->
                    let inZone =
                        classified |> Array.where (fun (z, _, _) -> z = zone)
                    let count kind =
                        inZone
                            |> Array.where (fun (_, k, _) -> k = kind)
                            |> Array.length
                    zone,
                    {
                        Hairpins = count HairpinFormer
                        NonFormers = count NonFormer
                        Others = count Short
                    })
                |> Map
        let hairpins =
            classified
                |> Array.where (fun (_, k, _) -> k = HairpinFormer)
                |> Array.map (fun (_, _, strand) -> strand)
        {
            Counts = counts
            ColdHairpins = hairpins.Length
            Folded = (hairpins |> Array.where isFolded).Length
        }

    /// The living world, heated by the settings' vent, at the settings'
    /// rate of hydrolysis, with the given variant's stiffness and
    /// protection, seeded with the settings' number of strands of each
    /// kind at random places. The strands are seeded in pairs, one of
    /// each kind, starting at the same x and so at the same distance
    /// from the vent, since lineages don't travel far: otherwise each
    /// zone's share would start wherever chance put it.
    let getScenario settings variant =
        assert(settings.HairpinFormer.Length = settings.NonFormer.Length)
        let addStrands parms rng world =
            (world, [ 1 .. settings.NumSeeds ])
                ||> Seq.fold (fun world _ ->
                    let x = Build.getRandomStart rng 74 settings.HairpinFormer.Length
                    world
                        |> Build.addStrandAtRandomHeight parms rng 74 x settings.HairpinFormer
                        |> Build.addStrandAtRandomHeight parms rng 74 x settings.NonFormer)
        let scenario = Scenario.createScenarioB false
        { scenario with Schedule = Vent settings.Vent }
            |> Scenario.withStrongBarrier settings.StrongBarrier
            |> Scenario.withHydrolysisRate settings.HydrolysisRate
            |> Scenario.withProtectedHydrolysisRate variant.ProtectedHydrolysisRate
            |> Scenario.withBend Strong Bend.rightAngle variant.BendStiffness
            |> Scenario.withTemplateStep addStrands   // so that the seeded strands can be followed in the desktop app
            |> Scenario.withName $"Hairpins in a vent world, kappa {variant.BendStiffness}, protected rate {variant.ProtectedHydrolysisRate}"

    /// The experiment's scenarios: one for each variant.
    let getScenarios settings =
        settings.Variants |> Array.map (getScenario settings)

    /// Runs one world through the cycles, calling the given function
    /// with each cycle's number when it ends. Answers a result for each
    /// cycle, starting with the world as seeded.
    let private runOne settings variant seed onCycle =
        let scenario = getScenario settings variant
        let mutable run = Run.create seed scenario
        [|
            getCensus settings run.World
            for iCycle in 1 .. settings.NumCycles do
                run <- Run.advance settings.Vent.ColdSweeps run
                let cold = getCensus settings run.World
                run <- Run.advance settings.Vent.HotSweeps run
                onCycle iCycle
                { getCensus settings run.World with
                    ColdHairpins = cold.ColdHairpins
                    Folded = cold.Folded }
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
        let scenario = getScenario settings settings.Variants[0]
        let vent = settings.Vent
        let low, high = settings.ZoneTemperatures
        let reach kT =   // distance from the vent at which a pulse reaches the given temperature
            float vent.Width * (1.0 - (kT - vent.FloorTemperature) / (vent.HotTemperature - vent.FloorTemperature))
        printfn "Experiment 21: hairpins in a vent world"
        printfn "Hairpin-former: %s (longest stem %d)" (toText settings.HairpinFormer) (Experiment16.getMaxStem settings.HairpinFormer)
        printfn "Non-former:     %s (longest stem %d)" (toText settings.NonFormer) (Experiment16.getMaxStem settings.NonFormer)
        printfn "%d runs of %d cycles of each variant; %d strands of each kind to start, at random places"
            settings.NumRuns settings.NumCycles settings.NumSeeds
        printfn "Capped chains of %d atoms or more count as hairpin-formers if they could fold with a stem of %d pairs or more, otherwise as non-formers"
            settings.MinLength settings.MinStem
        printfn "Vent: pulses of kT %g at the vent, falling to %g at %d sites; kT %g everywhere between pulses; %d sweeps cold, %d hot"
            vent.HotTemperature vent.FloorTemperature vent.Width vent.ColdTemperature vent.ColdSweeps vent.HotSweeps
        printfn "Zones: near, where a pulse reaches kT %g or more (within %.0f sites); window, %g to %g (to %.0f sites); far, beyond"
            high (reach high) low high (reach low)
        printParameters scenario.Parameters
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
        writer.WriteLine("kappa,protected_rate,cycle,zone,hairpins,non_formers,others,hairpin_share,cold_hairpins,folded")
        let invariant (x : float) = x.ToString(Globalization.CultureInfo.InvariantCulture)
        let share (hairpins : float) nonFormers =
            let n = hairpins + nonFormers
            if n > 0.0 then hairpins / n else nan
        for variant in settings.Variants do
            let runs = results[variant]
            printfn ""
            printfn "Strong bend stiffness %g, protected hydrolysis rate %g"
                variant.BendStiffness variant.ProtectedHydrolysisRate
            printfn "        |--------- capped chains per run, by zone (hairpin / non-former) ---------|   |- end of cold -|"
            printfn "cycle         near          window             far           all   share   other   hairpin  folded"
            for iCycle in 0 .. settings.NumCycles do
                let cycle = runs |> Array.map (fun cycles -> cycles[iCycle])
                let average f = cycle |> Array.averageBy (f >> float)
                let inZone zone f = average (fun c -> f c.Counts[zone])
                let total f = average (fun c -> zones |> Array.sumBy (fun zone -> f c.Counts[zone]))
                let cells =
                    zones
                        |> Array.map (fun zone ->
                            let h = inZone zone _.Hairpins
                            let n = inZone zone _.NonFormers
                            let o = inZone zone _.Others
                            writer.WriteLine(
                                String.Join(",",
                                    [
                                        invariant variant.BendStiffness; invariant variant.ProtectedHydrolysisRate
                                        string iCycle; getZoneName zone
                                        invariant h; invariant n; invariant o; invariant (share h n)
                                        invariant (average _.ColdHairpins); invariant (average _.Folded)
                                    ]))
                            sprintf "%6.2f / %5.2f" h n)
                let h = total _.Hairpins
                let n = total _.NonFormers
                printfn "%5d  %s  %s  %s  %6.2f / %5.2f  %6.2f  %6.1f   %7.2f  %6.2f"
                    iCycle cells[0] cells[1] cells[2] h n (share h n) (total _.Others)
                    (average _.ColdHairpins) (average _.Folded)

                // at the end, per run: each zone's share, and runs ahead
            let final = runs |> Array.map (fun cycles -> cycles[settings.NumCycles])
            for zone in zones do
                let counts = final |> Array.map (fun c -> c.Counts[zone])
                let shares =
                    counts
                        |> Array.choose (fun c ->
                            let n = c.Hairpins + c.NonFormers
                            if n > 0 then Some (float c.Hairpins / float n) else None)
                let nAhead = counts |> Array.where (fun c -> c.Hairpins > c.NonFormers) |> Array.length
                let nBehind = counts |> Array.where (fun c -> c.Hairpins < c.NonFormers) |> Array.length
                let meanText =
                    if shares.Length = 0 then "-"
                    elif shares.Length = 1 then sprintf "%.3f" shares[0]
                    else
                        let mean = Array.average shares
                        let sd = sqrt ((shares |> Array.sumBy (fun x -> (x - mean) ** 2.0)) / float (shares.Length - 1))
                        sprintf "%.3f +/- %.3f" mean (sd / sqrt (float shares.Length))
                printfn "At the end, %-6s hairpin share per run %s (%d runs with either kind); hairpins ahead in %d runs, behind in %d; one-sided sign test p = %.3g"
                    (getZoneName zone + ":") meanText shares.Length nAhead nBehind
                    (Experiment17.getSignTestP nAhead nBehind)

        printfn ""
        printfn "hairpin, non-former: capped chains that could fold, and that couldn't, per run, at the end"
        printfn "   of each pulse, placed in a zone by the location of the chain's middle atom."
        printfn "share: hairpins / (hairpins + non-formers), from the averages. other: shorter capped chains."
        printfn "end of cold: chains that could fold at the end of the cold phase, and those of them folded"
        printfn "   (at least two weak bonds within the chain)."
        printfn "Results written to %s" (Path.GetFullPath csvPath)
