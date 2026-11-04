namespace Ecos.Experiment

open System
open System.IO
open System.Text.RegularExpressions
open System.Threading

open Ecos
open Ecos.Experiment.Experiment3

/// Experiment 24: chambers of plenty and want. Does a hairpin-former copy
/// well while food lasts, and fold once it runs out?
///
/// A 320x320 torus is divided by walls into four 160x160 chambers. Each
/// is seeded with two hairpin-formers (P, ten W, Y Y, ten X, Q), and the
/// first with the standard world's density of free atoms, the second
/// with half of it, the third with a quarter, and the fourth with none.
/// The world cycles between cold, for copying, and hot, so that duplexes
/// and hairpins come apart, with no hydrolysis, until every chamber
/// starves. Within each cold phase, each chamber's capped chains are
/// counted several times, as copied, folded or single, to see how fast
/// starving strands fold.
module Experiment24 =

    /// Settings for the experiment.
    type Settings =
        {
            /// The strand that can fold into a hairpin.
            HairpinFormer : AtomType[]

            /// Number of hairpin-formers to start with in each chamber.
            NumSeeds : int

            /// Number of free atoms of each type in each chamber.
            NumFoodEach : int[]

            /// Types of free atoms.
            FoodTypes : AtomType[]

            /// Shortest capped chain counted.
            MinLength : int

            /// Pattern that a capped chain's sequence, as text, must match
            /// to be counted.
            ChainPattern : string

            /// Shortest stem, in pairs, of a chain counted as folded: the
            /// longest run of its atoms that are each paired within it
            /// must be at least twice this.
            MinFoldedStem : int

            /// Strong-bond bend stiffnesses to run, with the standard
            /// bend limit (right angles).
            BendStiffnesses : float[]

            /// Bend stiffness of the hinge atoms, U and V, if they
            /// differ from the others.
            HingeStiffness : float option

            /// The cold phase, for copying.
            Cold : Phase

            /// The hot phase.
            Hot : Phase

            /// Sweeps into each cold phase at which chains are counted.
            SampleTimes : int[]

            /// Number of independent runs (seeds) of each stiffness.
            NumRuns : int

            /// Number of cycles to run.
            NumCycles : int
        }

    /// Default settings.
    let defaultSettings =
        {
            HairpinFormer = Experiment22.getHairpinFormer 10
            NumSeeds = 2
            NumFoodEach = [| 120; 60; 30; 0 |]   // the standard world has 120 of each type
            FoodTypes = [| AtomType.W; AtomType.X; AtomType.Y; AtomType.Z; AtomType.P; AtomType.Q |]
            MinLength = 10
            ChainPattern = "^[PQ](W+[YZ]*X+|X+[YZ]*W+)[PQ]$"   // the hairpin family: caps around runs of W, of Y or Z, and of X
            MinFoldedStem = 4
            BendStiffnesses = [| 0.0; 0.5 |]
            HingeStiffness = None
            Cold = { Name = Some "cold"; Temperature = 0.3; NumSweeps = 200_000; HydrolysisRate = None }
            Hot = { Name = Some "hot"; Temperature = 3.0; NumSweeps = 100_000; HydrolysisRate = None }
            SampleTimes = [| 10_000; 25_000; 50_000; 100_000; 200_000 |]
            NumRuns = 12
            NumCycles = 16
        }

    /// A hairpin-former whose stems mix all four coding types, so that
    /// it pairs strongly only in register: P, a stem, Y Y, the stem's
    /// reverse complement, Q. Chosen by search: it folds with 11 pairs
    /// (caps included), but its longest misaligned fold is 2 pairs, and
    /// its longest pairing with another copy of itself or with its
    /// complement, out of register, is 3 (against 9 or 10 for a stem of
    /// ten W). W-X pairs close its loop.
    let mixedHairpinFormer =
        let stem =
            [| AtomType.W; AtomType.W; AtomType.X; AtomType.W; AtomType.Y
               AtomType.X; AtomType.Z; AtomType.X; AtomType.W; AtomType.W |]
        [|
            AtomType.P
            yield! stem
            AtomType.Y; AtomType.Y
            yield! stem |> Array.rev |> Array.map complement
            AtomType.Q
        |]

    /// Settings for the mixed-sequence hairpin-former, at stiffer
    /// settings, since long strands are floppy. Only hairpin-formers
    /// are seeded, so every capped chain is counted.
    let mixedSettings =
        { defaultSettings with
            HairpinFormer = mixedHairpinFormer
            ChainPattern = ".*"
            BendStiffnesses = [| 0.5; 1.0; 1.5 |]
            NumRuns = 8 }

    /// The mixed-sequence hairpin-former with a hinge for a loop: U U,
    /// whose copy's loop is V V. In a loop of two, the two right-angle
    /// bends of the fold fall on the loop's atoms, and the stems run
    /// straight into it.
    let hingedHairpinFormer =
        mixedHairpinFormer
            |> Array.mapi (fun i atomType ->
                let n = mixedHairpinFormer.Length
                if i = n / 2 - 1 || i = n / 2 then AtomType.U
                else atomType)

    /// Settings for the hinged hairpin-former: stiff everywhere but at
    /// the hinge, which bends freely, so that a strand can fold at its
    /// loop but nowhere else. Free U and V atoms are added, so that the
    /// hinge can be copied.
    let hingedSettings =
        { mixedSettings with
            HairpinFormer = hingedHairpinFormer
            FoodTypes = Array.append defaultSettings.FoodTypes [| AtomType.U; AtomType.V |]
            BendStiffnesses = [| 1.0; 2.0; 3.0 |]
            HingeStiffness = Some 0.0 }

    /// The hinged hairpin-former at the default stiffness only. This is
    /// the baseline for later experiments.
    let standardSettings =
        { hingedSettings with
            BendStiffnesses = [| Scenario.defaultBendStiffness |]
            HingeStiffness = Some Scenario.defaultHingeStiffness }

    /// Width of the world.
    let private worldSize = 320

    /// Width of each chamber, walls included.
    let private chamberSize = worldSize / 2

    /// Center of the given chamber, relative to the center of the world.
    /// Chambers are numbered left to right, then top to bottom.
    let private getChamberCenter iChamber =
        let col = iChamber % 2
        let row = iChamber / 2
        Point.create (
            -chamberSize / 2 + chamberSize * col,
            -chamberSize / 2 + chamberSize * row)

    /// Chamber that holds the given location.
    let private getChamber (location : Point) =
        let col = if location.X < chamberSize then 0 else 1
        let row = if location.Y < chamberSize then 0 else 1
        2 * row + col

    /// Greatest distance from a chamber's center, along each axis, at
    /// which an atom can be placed inside it.
    let private maxInside = chamberSize / 2 - 2

    /// The world for the given stiffness: scenario C's bonds (nothing
    /// arises by chance), with walls between the chambers, each seeded
    /// with hairpin-formers at random places, and then its free atoms.
    let getScenario settings bendStiffness =
        assert(settings.NumFoodEach.Length = 4)
        let scenario = Scenario.createScenarioC false
        let build parms (rng : Random) =
            let half = worldSize / 2

                // walls along both seams of the torus and through its center
            let world =
                World.createSquare worldSize
                    |> Build.addVerticalWall -half -half (half - 2)
                    |> Build.addVerticalWall 0 -half (half - 2)
            let world =
                (world, [ -half; 0 ])
                    ||> Seq.fold (fun world y ->
                        world
                            |> Build.addHorizontalWall (-half + 2) -2 y
                            |> Build.addHorizontalWall 2 (half - 2) y)

                // hairpin-formers
            let n = settings.HairpinFormer.Length
            let world =
                (world, [| for iChamber in 0 .. 3 do for _ in 1 .. settings.NumSeeds -> iChamber |])
                    ||> Array.fold (fun world iChamber ->
                        let c = getChamberCenter iChamber
                        let rec loop () =
                            let x = c.X + 2 * rng.Next(-maxInside / 2, (maxInside - 2 * n) / 2 + 1)
                            let y = c.Y + 2 * rng.Next(-maxInside / 2, maxInside / 2 + 1)
                            Build.tryAddStrand parms x y settings.HairpinFormer world
                                |> Option.defaultWith loop
                        loop ())
            let template = Array.map _.Id world.Atoms

                // free atoms
            let types = settings.FoodTypes
            let world =
                (world, Array.indexed settings.NumFoodEach)
                    ||> Array.fold (fun world (iChamber, nEach) ->
                        let c = getChamberCenter iChamber
                        (world, types)
                            ||> Array.fold (fun world atomType ->
                                (world, [ 1 .. nEach ])
                                    ||> Seq.fold (fun world _ ->
                                        let rec loop () =
                                            let x = c.X + 2 * rng.Next(-maxInside / 2, maxInside / 2 + 1)
                                            let y = c.Y + 2 * rng.Next(-maxInside / 2, maxInside / 2 + 1)
                                            match World.tryAddAtom atomType (Build.center world + Point.create (x, y)) world with
                                                | Some (world, _) -> world
                                                | None -> loop ()
                                        loop ())))
            world, template
        let food = settings.NumFoodEach |> Array.map string |> String.concat "/"
        { scenario with
            Name =
                let strand = settings.HairpinFormer |> Array.map string |> String.concat ""
                let hinge =
                    settings.HingeStiffness
                        |> Option.map (fun kappa -> $", hinge {kappa}")
                        |> Option.defaultValue ""
                $"Chambers of plenty and want, {strand}, food {food}, stiffness {bendStiffness}{hinge}"
            Build = build
            Schedule = Cycle [| settings.Cold; settings.Hot |] }
            |> Scenario.withBend Strong Bend.rightAngle bendStiffness
            |> fun scenario ->
                match settings.HingeStiffness with
                    | Some kappa ->
                        scenario
                            |> Scenario.withBendStiffness Strong AtomType.U kappa
                            |> Scenario.withBendStiffness Strong AtomType.V kappa
                    | None -> scenario

    /// The experiment's scenarios: one for each stiffness.
    let getScenarios settings =
        settings.BendStiffnesses |> Array.map (getScenario settings)

    /// Census of one chamber.
    type private Census =
        {
            /// Number of counted chains, by state.
            NumCopied : int
            NumFolded : int
            NumSingle : int

            /// Number of uncopied chains with at least 2 * MinFoldedStem
            /// atoms paired in register: each with the atom at the mirror
            /// position of the chain, as in its designed fold.
            NumInRegister : int

            /// Sum, over uncopied chains, of the fraction of each chain's
            /// atoms that are paired within it, however aligned.
            InnerPaired : float

            /// Number of free W and X atoms (with no strong bonds).
            NumFreeW : int
            NumFreeX : int
        }

    module private Census =

        /// Total number of counted chains.
        let getNumChains census =
            census.NumCopied + census.NumFolded + census.NumSingle

        /// Number of uncopied chains.
        let getNumUncopied census =
            census.NumFolded + census.NumSingle

        /// Number of free W and X atoms, per type.
        let getFood census =
            float (census.NumFreeW + census.NumFreeX) / 2.0

    /// A counted chain's atoms, chamber and census.
    type ChainCensus =
        {
            /// Its atoms, in order.
            AtomIds : int[]

            Chamber : int

            /// 0 if copied, 1 if folded, 2 if single.
            State : int

            /// Number of its atoms paired in register.
            NumInRegister : int

            /// Fraction of its atoms paired within it.
            InnerPaired : float
        }

    /// Finds and classifies the counted chains of the given world. A
    /// chain is copied if half or more of its atoms are paired with
    /// another chain; folded if not, but a long enough run of its atoms
    /// are paired within it (see MinFoldedStem); and single otherwise.
    let getChains settings (world : World) =
        let strong = world.BondTypeMap[Strong]
        let weak = world.BondTypeMap[Weak]
        let pattern = Regex(settings.ChainPattern)
        getStrands world
            |> Array.where (fun strand ->
                strand.AtomIds.Length >= settings.MinLength
                    && isCapped world strand
                    && strand.AtomIds
                        |> Array.map (fun id -> string world.Atoms[id].Type)
                        |> String.concat ""
                        |> pattern.IsMatch)
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
                let nInner =
                    partners
                        |> Array.where (function Some p -> atoms.Contains p | None -> false)
                        |> Array.length
                let nInRegister =
                    partners
                        |> Array.indexed
                        |> Array.where (fun (i, p) -> p = Some ids[ids.Length - 1 - i])
                        |> Array.length
                {
                    AtomIds = ids
                    Chamber = getChamber world.Atoms[ids[0]].Location
                    State =
                        if 2 * nCopied >= ids.Length then 0
                        elif longestInnerRun >= 2 * settings.MinFoldedStem then 1
                        else 2
                    NumInRegister = nInRegister
                    InnerPaired = float nInner / float ids.Length
                })

    /// Takes a census of each chamber of the given world (see getChains).
    let private getCensuses settings (world : World) =
        let strong = world.BondTypeMap[Strong]
        let chains = getChains settings world
        let free =
            world.Atoms
                |> Array.where (fun atom ->
                    (atom.Type = AtomType.W || atom.Type = AtomType.X)
                        && Set.isEmpty (BondMap.getBonds atom.Id strong))
                |> Array.map (fun atom -> getChamber atom.Location, atom.Type)
        [|
            for iChamber in 0 .. 3 do
                let inChamber = chains |> Array.where (fun c -> c.Chamber = iChamber)
                let uncopied = inChamber |> Array.where (fun c -> c.State <> 0)
                let inState state =
                    inChamber |> Array.where (fun c -> c.State = state) |> Array.length
                let numFree atomType =
                    free |> Array.where (fun f -> f = (iChamber, atomType)) |> Array.length
                {
                    NumCopied = inState 0
                    NumFolded = inState 1
                    NumSingle = inState 2
                    NumInRegister =
                        uncopied
                            |> Array.where (fun c -> c.NumInRegister >= 2 * settings.MinFoldedStem)
                            |> Array.length
                    InnerPaired = uncopied |> Array.sumBy _.InnerPaired
                    NumFreeW = numFree AtomType.W
                    NumFreeX = numFree AtomType.X
                }
        |]

    /// Runs one world through the cycles, calling the given function
    /// with each cycle's number when it ends. Answers each chamber's
    /// census at each sample time of each cycle, indexed by cycle,
    /// sample and chamber.
    let private runOne settings bendStiffness seed onCycle =
        let mutable run = Run.create seed (getScenario settings bendStiffness)
        [|
            for iCycle in 1 .. settings.NumCycles do
                let samples =
                    [|
                        let mutable elapsed = 0
                        for t in settings.SampleTimes do
                            run <- Run.advance (t - elapsed) run
                            elapsed <- t
                            getCensuses settings run.World
                    |]
                run <- Run.advance (settings.Cold.NumSweeps + settings.Hot.NumSweeps - Array.last settings.SampleTimes) run
                onCycle iCycle
                samples
        |]

    /// Bins of free atoms of each type, by lower bound.
    let private foodBins = [| 0; 5; 10; 20; 40; 80 |]

    /// Runs the experiment, prints tables of results, and writes them
    /// to the given CSV file.
    let run (settings : Settings) (csvPath : string) =
        assert(Array.last settings.SampleTimes <= settings.Cold.NumSweeps)

        let combos =
            [|
                for stiffness in settings.BendStiffnesses do
                    for seed in 0 .. settings.NumRuns - 1 do
                        stiffness, seed
            |]

        let toText = Array.map string >> String.concat " "
        let phaseText (phase : Phase) = sprintf "kT %g for %d sweeps" phase.Temperature phase.NumSweeps
        printfn "Experiment 24: chambers of plenty and want"
        printfn "Hairpin-former: %s (longest stem %d)" (toText settings.HairpinFormer) (Experiment16.getMaxStem settings.HairpinFormer)
        printfn "A %dx%d world in four walled chambers, each with %d hairpin-formers at random places; free atoms of each type per chamber: %s"
            worldSize worldSize settings.NumSeeds (settings.NumFoodEach |> Array.map string |> String.concat ", ")
        printfn "Free atom types: %s" (toText settings.FoodTypes)
        settings.HingeStiffness
            |> Option.iter (printfn "Hinge atoms (U, V) bend with stiffness %g; all others with the stiffness of the run")
        printfn "%d runs of %d cycles at each stiffness (%s); each cycle: cold %s; hot %s; no hydrolysis"
            settings.NumRuns settings.NumCycles (settings.BendStiffnesses |> Array.map string |> String.concat ", ")
            (phaseText settings.Cold) (phaseText settings.Hot)
        printfn "Capped chains of %d atoms or more that match %s are counted at %s sweeps into each cold phase"
            settings.MinLength settings.ChainPattern (settings.SampleTimes |> Array.map string |> String.concat ", ")
        printParameters (getScenario settings settings.BendStiffnesses[0]).Parameters
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
                |> Array.Parallel.map (fun (stiffness, seed) ->
                    (stiffness, seed), runOne settings stiffness seed onCycle)
                |> Map

        use writer = new StreamWriter(csvPath)
        writer.WriteLine("stiffness,seed,cycle,sweeps_into_cold,chamber,food_each,free_w,free_x,copied,folded,single,in_register,inner_paired")
        for (stiffness, seed), cycles in Map.toSeq results do
            for iCycle in 0 .. settings.NumCycles - 1 do
                for iSample in 0 .. settings.SampleTimes.Length - 1 do
                    for iChamber in 0 .. 3 do
                        let c = cycles.[iCycle].[iSample].[iChamber]
                        writer.WriteLine(
                            String.Join(",",
                                [ string stiffness; string seed; string (iCycle + 1)
                                  string settings.SampleTimes[iSample]; string (iChamber + 1)
                                  string settings.NumFoodEach[iChamber]
                                  string c.NumFreeW; string c.NumFreeX
                                  string c.NumCopied; string c.NumFolded; string c.NumSingle
                                  string c.NumInRegister; c.InnerPaired.ToString(Globalization.CultureInfo.InvariantCulture) ]))

        let iEnd = settings.SampleTimes.Length - 1
        for stiffness in settings.BendStiffnesses do
            let runs = [| for seed in 0 .. settings.NumRuns - 1 -> results[stiffness, seed] |]
            printfn ""
            printfn "Stiffness %g" stiffness
            for iChamber in 0 .. 3 do
                printfn ""
                printfn "Chamber %d (%d free atoms of each type to start), at the end of each cold phase, and folded chains over it:"
                    (iChamber + 1) settings.NumFoodEach[iChamber]
                printfn "cycle   chains   food   copied folded single   fold%%  in-reg  inner   folded at %s"
                    (settings.SampleTimes |> Array.map (fun t -> sprintf "%5dk" (t / 1000)) |> String.concat "")
                for iCycle in 0 .. settings.NumCycles - 1 do
                    let average iSample f =
                        runs |> Array.averageBy (fun r -> f r.[iCycle].[iSample].[iChamber])
                    let copied = average iEnd (_.NumCopied >> float)
                    let folded = average iEnd (_.NumFolded >> float)
                    let single = average iEnd (_.NumSingle >> float)
                    let foldPct = if folded + single > 0.0 then 100.0 * folded / (folded + single) else nan
                    let inRegister = average iEnd (_.NumInRegister >> float)
                    let inner =
                        let n = runs |> Array.sumBy (fun r -> Census.getNumUncopied r.[iCycle].[iEnd].[iChamber])
                        if n > 0 then (runs |> Array.sumBy (fun r -> r.[iCycle].[iEnd].[iChamber].InnerPaired)) / float n
                        else nan
                    let overTime =
                        [| for iSample in 0 .. iEnd -> sprintf "%6.2f" (average iSample (_.NumFolded >> float)) |]
                            |> String.concat ""
                    printfn "%5d   %6.2f  %5.1f    %5.2f  %5.2f  %5.2f   %4.0f%%   %5.2f  %5.2f   %s"
                        (iCycle + 1) (average iEnd (Census.getNumChains >> float)) (average iEnd Census.getFood)
                        copied folded single foldPct inRegister inner overTime

                // pooled over chambers, cycles and sample times, by food
            printfn ""
            printfn "By free atoms of each type in the chamber (W and X averaged), pooled over chambers, cycles and sample times:"
            printfn "   food      samples   chains   copied   folded   single   in register of uncopied   inner   folded of uncopied"
            let censuses =
                runs |> Array.collect (fun r ->
                    r |> Array.collect (fun samples -> samples |> Array.collect id))
            for iBin in 0 .. foodBins.Length - 1 do
                let lo = float foodBins[iBin]
                let hi = if iBin + 1 < foodBins.Length then float foodBins[iBin + 1] else infinity
                let inBin =
                    censuses |> Array.where (fun c ->
                        let food = Census.getFood c
                        food >= lo && food < hi && Census.getNumChains c > 0)
                let sum f = inBin |> Array.sumBy f
                let nChains = sum Census.getNumChains
                let frac n = if nChains > 0 then float n / float nChains else nan
                let nFolded = sum _.NumFolded
                let nUncopied = nFolded + sum _.NumSingle
                let ofUncopied x = if nUncopied > 0 then x / float nUncopied else nan
                printfn "   %-8s  %7d   %6d    %5.2f    %5.2f    %5.2f    %22.2f   %5.2f   %s"
                    (if Double.IsInfinity hi then sprintf "%g+" lo else sprintf "%g-%g" lo (hi - 1.0))
                    inBin.Length nChains
                    (frac (sum _.NumCopied)) (frac nFolded) (frac (sum _.NumSingle))
                    (ofUncopied (float (sum _.NumInRegister))) (ofUncopied (inBin |> Array.sumBy _.InnerPaired))
                    (if nUncopied > 0 then sprintf "%.2f (%d/%d)" (float nFolded / float nUncopied) nFolded nUncopied else "-")

        printfn ""
        printfn "chains: counted chains in the chamber, per run. food: free W and X atoms (no strong bonds), per type."
        printfn "copied: half or more of a chain's atoms paired with another chain; folded: not copied, but a run of"
        printfn "   at least %d atoms paired within it; single: neither. fold%%: folded / (folded + single)."
            (2 * settings.MinFoldedStem)
        printfn "in-reg: uncopied chains with at least %d atoms paired in register (each with the atom at its mirror position)."
            (2 * settings.MinFoldedStem)
        printfn "inner: mean fraction of an uncopied chain's atoms paired within it, however aligned."
        printfn "Results written to %s" (Path.GetFullPath csvPath)
