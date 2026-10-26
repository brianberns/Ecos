namespace Ecos.Experiment

open System
open System.IO
open System.Threading

open Ecos
open Ecos.Experiment.Experiment3

/// Experiment 20: is there a temperature at which hairpins stay folded
/// but single strands can't be copied? A hairpin's stem pairs are
/// stacked, which makes them stronger than the unstacked pairs that free
/// atoms form on a single strand while it is being copied. A long enough
/// stem might stay folded at a temperature at which free atoms no longer
/// stay paired long enough to join into a copy. At such a temperature, a
/// strand that can fold would be protected (its stem is stacked), while
/// one that can't would be left single and exposed, without any shortage
/// of free atoms.
///
/// Part A: lone hairpins, P W...W Y Y X...X Q, with stems of various
/// lengths (counting the caps' pair), at various temperatures, starting
/// either folded (do they stay folded?) or straight (do they fold?).
///
/// Part B: single strands among the living world's free atoms, at
/// various temperatures: how quickly do they become stacked, by being
/// copied or by pairing with another strand?
module Experiment20 =

    /// Settings for the experiment.
    type Settings =
        {
            /// Stem lengths of the hairpins in part A, in pairs.
            StemLengths : int[]

            /// Strong-bond bend stiffnesses for part A.
            BendStiffnesses : float[]

            /// Temperatures for part A.
            HairpinTemperatures : float[]

            /// Number of runs of each combination in part A.
            NumHairpinRuns : int

            /// Sweeps before sampling starts in part A.
            BurnIn : int

            /// Sweeps between samples in part A.
            SampleInterval : int

            /// Number of samples per run in part A.
            NumSamples : int

            /// Temperatures for part B.
            CopyingTemperatures : float[]

            /// Number of single strands seeded in each world in part B.
            NumStrands : int

            /// Times in part B, in sweeps, at which to measure the strands.
            CopyingTimes : int[]

            /// Number of runs at each temperature in part B.
            NumCopyingRuns : int
        }

    /// Default settings.
    let defaultSettings =
        {
            StemLengths = [| 4; 6; 8 |]
            BendStiffnesses = [| 1.5; 0.5 |]
            HairpinTemperatures = [| 0.4; 0.5; 0.6; 0.7; 0.8; 0.9 |]
            NumHairpinRuns = 8
            BurnIn = 10_000
            SampleInterval = 1_000
            NumSamples = 40
            CopyingTemperatures = [| 0.3; 0.4; 0.5; 0.6; 0.7; 0.8 |]
            NumStrands = 16
            CopyingTimes = [| 1000; 2000; 5000; 10000 |]
            NumCopyingRuns = 4
        }

    /// The living world's parameters, with the given strong-bond
    /// stiffness, at the given temperature, without hydrolysis.
    let private getParameters kappa kT =
        let scenario =
            Experiment4.getScenario Experiment4.livingWorldSettings 15.0
                |> Scenario.withBend Strong Bend.rightAngle kappa
                |> Scenario.withHydrolysisRate 0.0
        { scenario.Parameters with
            GetTemperature = Parameters.getUniformTemperature kT }

    /// A hairpin's atoms in order, with a stem of the given number of
    /// pairs (counting the caps' pair) and a Y Y loop.
    let private getHairpin stem =
        [|
            AtomType.P
            yield! Array.replicate (stem - 1) AtomType.W
            AtomType.Y; AtomType.Y
            yield! Array.replicate (stem - 1) AtomType.X
            AtomType.Q
        |]

    /// Adds the given atoms at the given locations, relative to the
    /// center of the given world, strongly bonded in order.
    let private addChain parms (atoms : (AtomType * int * int)[]) (world : World) =
        let first = world.Atoms.Length
        let world =
            (world, atoms)
                ||> Array.fold (fun world (atomType, x, y) ->
                    World.addAtom atomType (Build.center world + Point.create (x, y)) world)
        (world, [| first + 1 .. world.Atoms.Length - 1 |])
            ||> Array.fold (fun world id ->
                World.addStrongBond parms (id - 1) id world)

    /// A small world holding a hairpin with the given stem, either folded
    /// (the stem along the bottom, back along the top, each X above a W
    /// and Q above P, its pairs formed) or straight.
    let private createHairpinWorld parms stem isFolded =
        let world = World.createSquare 60
        let strand = getHairpin stem
        let n = strand.Length
        if isFolded then
            let half = n / 2
            let bottom = strand[.. half - 1] |> Array.mapi (fun i t -> t, 2 * i - half, 0)
            let top = strand[half ..] |> Array.mapi (fun i t -> t, half - 2 - 2 * i, 2)
            let world = addChain parms (Array.append bottom top) world
            (world, [ for i in 0 .. stem - 1 -> i, n - 1 - i ])
                ||> List.fold (fun world (idA, idB) -> World.addWeakBond parms idA idB world)
        else
            addChain parms (strand |> Array.mapi (fun i t -> t, 2 * i - n, 0)) world

    /// Number of weak bonds within the hairpin, which is alone in its world.
    let private countPairs (world : World) =
        let weak = world.BondTypeMap[Weak]
        (world.Atoms |> Array.sumBy (fun atom -> (BondMap.getBonds atom.Id weak).Count)) / 2

    /// Runs one lone hairpin. Answers the number of its pairs in each
    /// sample.
    let private runHairpin settings stem kappa kT isFolded seed =
        let parms = getParameters kappa kT
        let rng = Random(seed)
        let mutable world =
            createHairpinWorld parms stem isFolded
                |> World.sweep parms rng settings.BurnIn
        [|
            for _ in 1 .. settings.NumSamples do
                world <- World.sweep parms rng settings.SampleInterval world
                countPairs world
        |]

    /// Runs single strands among free atoms. Answers, at each time, the
    /// fraction of the seeded strands' bonds that are stacked.
    let private runCopying settings kT seed =
        let parms = getParameters 1.5 kT
        let rng = Random(seed)
        let strand = Experiment17.longNonFormer
        let world, _ = (Experiment4.getScenario Experiment4.livingWorldSettings 15.0).Build parms rng
        let world, seeds =
            ((world, []), [ 1 .. settings.NumStrands ])
                ||> List.fold (fun (world, seeds) _ ->
                    let first = world.Atoms.Length
                    let world = Build.addStrandAtRandom parms rng 70 strand world
                    world, [| first .. first + strand.Length - 1 |] :: seeds)
        let bonds = seeds |> List.toArray |> Array.collect Array.pairwise
        let stackedFraction (world : World) =
            let strong = world.BondTypeMap[Strong]
            let weak = world.BondTypeMap[Weak]
            let isStacked (a, b) =
                BondMap.getBonds a weak
                    |> Set.exists (fun a' ->
                        BondMap.getBonds b weak
                            |> Set.exists (fun b' -> BondMap.areBonded a' b' strong))
            float (bonds |> Array.where isStacked).Length / float bonds.Length
        let mutable world = world
        let mutable elapsed = 0
        [|
            for time in settings.CopyingTimes do
                world <- World.sweep parms rng (time - elapsed) world
                elapsed <- time
                stackedFraction world
        |]

    /// Runs the experiment, prints tables of results, and writes them to
    /// the given CSV file.
    let run (settings : Settings) (csvPath : string) =
        let invariant (x : float) = x.ToString(Globalization.CultureInfo.InvariantCulture)
        use writer = new StreamWriter(csvPath)
        writer.WriteLine("part,stem,kappa,kT,start,time,value")

            // part A: lone hairpins
        let combos =
            [|
                for stem in settings.StemLengths do
                    for kappa in settings.BendStiffnesses do
                        for kT in settings.HairpinTemperatures do
                            for isFolded in [ true; false ] do
                                for seed in 0 .. settings.NumHairpinRuns - 1 do
                                    stem, kappa, kT, isFolded, seed
            |]
        let mutable nDone = 0
        let hairpins =
            combos
                |> Array.Parallel.map (fun (stem, kappa, kT, isFolded, seed) ->
                    let samples = runHairpin settings stem kappa kT isFolded seed
                    let n = Interlocked.Increment(&nDone)
                    if n % 100 = 0 then
                        eprintfn "Part A: finished run %d of %d" n combos.Length
                    (stem, kappa, kT, isFolded), samples)
                |> Array.groupBy fst
                |> Array.map (fun (key, group) -> key, group |> Array.collect snd)
                |> Map

        printfn "Experiment 20: a window in which hairpins stay folded but single strands can't be copied"
        printfn ""
        printfn "Part A: lone hairpins P W...W Y Y X...X Q, by stem length (pairs, the caps' included)"
        printfn "%d runs of each, alone in a small world; %d samples, %d sweeps apart, after %d sweeps"
            settings.NumHairpinRuns settings.NumSamples settings.SampleInterval settings.BurnIn
        printfn "Fraction of samples folded (at least two pairs), starting folded / starting straight"
        for kappa in settings.BendStiffnesses do
            printfn ""
            printfn "Strong bend stiffness %g" kappa
            printfn "   kT   %s" (settings.StemLengths |> Array.map (sprintf "    stem %d    ") |> String.concat " ")
            for kT in settings.HairpinTemperatures do
                let cells =
                    settings.StemLengths
                        |> Array.map (fun stem ->
                            let folded isFolded =
                                let samples = hairpins[stem, kappa, kT, isFolded]
                                let value = samples |> Array.averageBy (fun n -> if n >= 2 then 1.0 else 0.0)
                                writer.WriteLine(
                                    String.Join(",",
                                        [ "A"; string stem; invariant kappa; invariant kT
                                          (if isFolded then "folded" else "straight"); ""; invariant value ]))
                                value
                            sprintf "  %4.2f / %4.2f  " (folded true) (folded false))
                        |> String.concat " "
                printfn "%5.2f   %s" kT cells

            // part B: copying single strands
        let combos =
            [|
                for kT in settings.CopyingTemperatures do
                    for seed in 0 .. settings.NumCopyingRuns - 1 do
                        kT, seed
            |]
        let copying =
            combos
                |> Array.Parallel.map (fun (kT, seed) -> kT, runCopying settings kT seed)
                |> Array.groupBy fst
                |> Array.map (fun (kT, group) -> kT, Array.map snd group)
                |> Map

        printfn ""
        printfn "Part B: %d single strands (%s) among the living world's free atoms, no hydrolysis"
            settings.NumStrands (Experiment17.longNonFormer |> Array.map string |> String.concat " ")
        printfn "%d runs at each temperature. Fraction of the strands' bonds stacked (copied, or paired with another strand)"
            settings.NumCopyingRuns
        printfn "   kT   %s" (settings.CopyingTimes |> Array.map (sprintf "%8d") |> String.concat "")
        for kT in settings.CopyingTemperatures do
            let runs = copying[kT]
            let cells =
                settings.CopyingTimes
                    |> Array.mapi (fun i time ->
                        let value = runs |> Array.averageBy (fun r -> r[i])
                        writer.WriteLine(
                            String.Join(",", [ "B"; ""; ""; invariant kT; ""; string time; invariant value ]))
                        sprintf "%8.2f" value)
                    |> String.concat ""
            printfn "%5.2f   %s" kT cells

        printfn ""
        printfn "Results written to %s" (Path.GetFullPath csvPath)
