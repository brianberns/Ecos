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

            /// Shortest capped chain counted.
            MinLength : int

            /// Shortest stem, in pairs, of a chain counted as folded: the
            /// longest run of its atoms that are each paired within it
            /// must be at least twice this.
            MinFoldedStem : int

            /// Strong-bond bend stiffnesses to run, with the standard
            /// bend limit (right angles).
            BendStiffnesses : float[]

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
            MinLength = 10
            MinFoldedStem = 4
            BendStiffnesses = [| 0.0; 0.5 |]
            Cold = { Name = Some "cold"; Temperature = 0.3; NumSweeps = 200_000; HydrolysisRate = None }
            Hot = { Name = Some "hot"; Temperature = 3.0; NumSweeps = 100_000; HydrolysisRate = None }
            SampleTimes = [| 10_000; 25_000; 50_000; 100_000; 200_000 |]
            NumRuns = 12
            NumCycles = 16
        }

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
            let types =
                [| AtomType.W; AtomType.X; AtomType.Y; AtomType.Z; AtomType.P; AtomType.Q |]
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
            Name = $"Chambers of plenty and want, food {food}, stiffness {bendStiffness}"
            Build = build
            Schedule = Cycle [| settings.Cold; settings.Hot |] }
            |> Scenario.withBend Strong Bend.rightAngle bendStiffness

    /// The experiment's scenarios: one for each stiffness.
    let getScenarios settings =
        settings.BendStiffnesses |> Array.map (getScenario settings)

    /// Chains of the hairpin family, as text: caps around a run of W, a
    /// run of Y or Z (perhaps empty), and a run of X, in either order.
    let private hairpinPattern = Regex("^[PQ](W+[YZ]*X+|X+[YZ]*W+)[PQ]$")

    /// Census of one chamber.
    type private Census =
        {
            /// Number of hairpin-family chains, by state.
            NumCopied : int
            NumFolded : int
            NumSingle : int

            /// Number of free W and X atoms (with no strong bonds).
            NumFreeW : int
            NumFreeX : int
        }

    module private Census =

        /// Total number of hairpin-family chains.
        let getNumChains census =
            census.NumCopied + census.NumFolded + census.NumSingle

        /// Number of free W and X atoms, per type.
        let getFood census =
            float (census.NumFreeW + census.NumFreeX) / 2.0

    /// Takes a census of each chamber of the given world. A chain is
    /// copied if half or more of its atoms are paired with another
    /// chain; folded if not, but a long enough run of its atoms are
    /// paired within it (see MinFoldedStem); and single otherwise.
    let private getCensuses settings (world : World) =
        let strong = world.BondTypeMap[Strong]
        let weak = world.BondTypeMap[Weak]
        let chains =
            getStrands world
                |> Array.where (fun strand ->
                    strand.AtomIds.Length >= settings.MinLength
                        && isCapped world strand
                        && strand.AtomIds
                            |> Array.map (fun id -> string world.Atoms[id].Type)
                            |> String.concat ""
                            |> hairpinPattern.IsMatch)
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
                        if 2 * nCopied >= ids.Length then 0
                        elif longestInnerRun >= 2 * settings.MinFoldedStem then 1
                        else 2
                    getChamber world.Atoms[ids[0]].Location, state)
        let free =
            world.Atoms
                |> Array.where (fun atom ->
                    (atom.Type = AtomType.W || atom.Type = AtomType.X)
                        && Set.isEmpty (BondMap.getBonds atom.Id strong))
                |> Array.map (fun atom -> getChamber atom.Location, atom.Type)
        [|
            for iChamber in 0 .. 3 do
                let inState state =
                    chains |> Array.where (fun c -> c = (iChamber, state)) |> Array.length
                let numFree atomType =
                    free |> Array.where (fun f -> f = (iChamber, atomType)) |> Array.length
                {
                    NumCopied = inState 0
                    NumFolded = inState 1
                    NumSingle = inState 2
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
        printfn "%d runs of %d cycles at each stiffness (%s); each cycle: cold %s; hot %s; no hydrolysis"
            settings.NumRuns settings.NumCycles (settings.BendStiffnesses |> Array.map string |> String.concat ", ")
            (phaseText settings.Cold) (phaseText settings.Hot)
        printfn "Hairpin-family chains (caps around a run of W, of Y or Z, and of X) of %d atoms or more are counted at %s sweeps into each cold phase"
            settings.MinLength (settings.SampleTimes |> Array.map string |> String.concat ", ")
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
        writer.WriteLine("stiffness,seed,cycle,sweeps_into_cold,chamber,food_each,free_w,free_x,copied,folded,single")
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
                                  string c.NumCopied; string c.NumFolded; string c.NumSingle ]))

        let iEnd = settings.SampleTimes.Length - 1
        for stiffness in settings.BendStiffnesses do
            let runs = [| for seed in 0 .. settings.NumRuns - 1 -> results[stiffness, seed] |]
            printfn ""
            printfn "Stiffness %g" stiffness
            for iChamber in 0 .. 3 do
                printfn ""
                printfn "Chamber %d (%d free atoms of each type to start), at the end of each cold phase, and folded chains over it:"
                    (iChamber + 1) settings.NumFoodEach[iChamber]
                printfn "cycle   chains   food   copied folded single   fold%%   folded at %s"
                    (settings.SampleTimes |> Array.map (fun t -> sprintf "%5dk" (t / 1000)) |> String.concat "")
                for iCycle in 0 .. settings.NumCycles - 1 do
                    let average iSample f =
                        runs |> Array.averageBy (fun r -> f r.[iCycle].[iSample].[iChamber])
                    let copied = average iEnd (_.NumCopied >> float)
                    let folded = average iEnd (_.NumFolded >> float)
                    let single = average iEnd (_.NumSingle >> float)
                    let foldPct = if folded + single > 0.0 then 100.0 * folded / (folded + single) else nan
                    let overTime =
                        [| for iSample in 0 .. iEnd -> sprintf "%6.2f" (average iSample (_.NumFolded >> float)) |]
                            |> String.concat ""
                    printfn "%5d   %6.2f  %5.1f    %5.2f  %5.2f  %5.2f   %4.0f%%   %s"
                        (iCycle + 1) (average iEnd (Census.getNumChains >> float)) (average iEnd Census.getFood)
                        copied folded single foldPct overTime

                // pooled over chambers, cycles and sample times, by food
            printfn ""
            printfn "By free atoms of each type in the chamber (W and X averaged), pooled over chambers, cycles and sample times:"
            printfn "   food      samples   chains   copied   folded   single   folded of uncopied"
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
                printfn "   %-8s  %7d   %6d    %5.2f    %5.2f    %5.2f    %s"
                    (if Double.IsInfinity hi then sprintf "%g+" lo else sprintf "%g-%g" lo (hi - 1.0))
                    inBin.Length nChains
                    (frac (sum _.NumCopied)) (frac nFolded) (frac (sum _.NumSingle))
                    (if nUncopied > 0 then sprintf "%.2f (%d/%d)" (float nFolded / float nUncopied) nFolded nUncopied else "-")

        printfn ""
        printfn "chains: hairpin-family chains in the chamber, per run. food: free W and X atoms (no strong bonds), per type."
        printfn "copied: half or more of a chain's atoms paired with another chain; folded: not copied, but a run of"
        printfn "   at least %d atoms paired within it; single: neither. fold%%: folded / (folded + single)."
            (2 * settings.MinFoldedStem)
        printfn "Results written to %s" (Path.GetFullPath csvPath)
