namespace Ecos.Experiment

open System
open System.IO
open System.Threading

open Ecos
open Ecos.Experiment.Experiment3

/// Experiment 18: melting of hairpins and duplexes. Does a hairpin stay
/// paired at temperatures where duplexes come apart? A hairpin's arms
/// are tethered by its loop, so after melting partly it may zip up
/// again, while a duplex whose strands have separated must find its
/// partner again. If so, a warm phase would leave hairpins paired, and
/// so protected from hydrolysis, while duplexes are not.
///
/// Each structure starts fully paired, alone in a small world with
/// scenario C's parameters (strong bonds never break, and capped ends
/// never peel), at a fixed temperature. It is sampled every so often,
/// and counts as paired when it has at least two weak bonds within the
/// hairpin, or between the duplex's two strands.
module Experiment18 =

    /// A structure to melt.
    type Structure =
        | Hairpin
        | ShortDuplex
        | LongDuplex

    /// Settings for the experiment.
    type Settings =
        {
            /// Temperatures to run at.
            Temperatures : float[]

            /// Strong-bond bend stiffnesses to run with, all with the
            /// standard bend limit (right angles).
            BendStiffnesses : float[]

            /// Number of independent runs (seeds) of each combination.
            NumRuns : int

            /// Sweeps before sampling starts.
            BurnIn : int

            /// Sweeps between samples.
            SampleInterval : int

            /// Number of samples per run.
            NumSamples : int
        }

    /// Default settings.
    let defaultSettings =
        {
            Temperatures = [| for i in 3 .. 12 -> float i / 10.0 |]   // 0.3 to 1.2
            BendStiffnesses = [| 1.5; 0.5 |]
            NumRuns = 8
            BurnIn = 10_000
            SampleInterval = 1_000
            NumSamples = 90
        }

    /// Scenario C's parameters with the given strong-bond stiffness (and
    /// the standard bend limit), at the given temperature.
    let private getParameters kappa kT =
        { (Scenario.createScenarioC false
            |> Scenario.withBend Strong Bend.rightAngle kappa).Parameters with
                GetTemperature = Parameters.getUniformTemperature kT }

    /// Adds atoms of the given types at the given locations, relative to
    /// the center of the given world, joined in order. Answers the world
    /// and the new atoms' IDs.
    let private addStrand parms (atoms : (AtomType * int * int)[]) (world : World) =
        let first = world.Atoms.Length
        let world =
            (world, atoms)
                ||> Array.fold (fun world (atomType, x, y) ->
                    World.addAtom atomType (Build.center world + Point.create (x, y)) world)
        let ids = [| first .. world.Atoms.Length - 1 |]
        let world =
            (world, Array.pairwise ids)
                ||> Array.fold (fun world (idA, idB) ->
                    World.addStrongBond parms idA idB world)
        world, ids

    /// A small world holding the given structure, fully paired. Answers
    /// the world and the atom IDs of its strand or strands.
    let private createWorld parms structure =
        let world = World.createSquare 60
        let pairUp (pairs : (AtomId * AtomId) seq) world =
            (world, pairs)
                ||> Seq.fold (fun world (idA, idB) -> World.addWeakBond parms idA idB world)
        match structure with
            | Hairpin ->
                    // P W W W Y along the bottom, then up and back along
                    // the top as Y X X X Q, so each X sits above a W, and
                    // Q above P
                let bottom =
                    [| AtomType.P; AtomType.W; AtomType.W; AtomType.W; AtomType.Y |]
                        |> Array.mapi (fun i t -> t, 2 * i - 4, 0)
                let top =
                    [| AtomType.Y; AtomType.X; AtomType.X; AtomType.X; AtomType.Q |]
                        |> Array.mapi (fun i t -> t, 4 - 2 * i, 2)
                let world, ids = addStrand parms (Array.append bottom top) world
                let world =
                    pairUp [ for i in 0 .. 3 -> ids[i], ids[9 - i] ] world   // P-Q, then W-X
                world, [| ids |]
            | ShortDuplex | LongDuplex ->
                let types =
                    match structure with
                        | ShortDuplex ->
                            [| AtomType.P; AtomType.W; AtomType.W; AtomType.W; AtomType.Q |]
                        | _ ->
                            [| AtomType.P; AtomType.W; AtomType.W; AtomType.W; AtomType.X
                               AtomType.X; AtomType.Y; AtomType.X; AtomType.Y; AtomType.Q |]
                let n = types.Length
                let row y (types : AtomType[]) =
                    types |> Array.mapi (fun i t -> t, 2 * i - n, y)
                let world, idsA = addStrand parms (row 0 types) world
                let partner =
                    types |> Array.rev |> Array.map complement   // its copy, read from P to Q
                let world, idsB =
                        // laid out right to left, so each atom faces its complement
                    addStrand parms (row 2 partner |> Array.map (fun (t, x, y) -> t, -x - 2, y)) world
                let world =
                    pairUp [ for i in 0 .. n - 1 -> idsA[i], idsB[n - 1 - i] ] world
                world, [| idsA; idsB |]

    /// Number of weak bonds within the hairpin, or between the duplex's
    /// two strands.
    let private countPairs (world : World) (strands : AtomId[][]) =
        let weak = world.BondTypeMap[Weak]
        match strands with
            | [| ids |] ->
                let atoms = set ids
                (ids |> Array.sumBy (fun id ->
                    BondMap.getBonds id weak |> Set.intersect atoms |> Set.count)) / 2
            | [| idsA; idsB |] ->
                let atomsB = set idsB
                idsA |> Array.sumBy (fun id ->
                    BondMap.getBonds id weak |> Set.intersect atomsB |> Set.count)
            | _ -> failwith "Unexpected structure"

    /// Runs one combination with one seed. Answers the number of pairs in
    /// each sample.
    let private runOne settings structure kappa kT seed =
        let parms = getParameters kappa kT
        let rng = Random(seed)
        let world, strands = createWorld parms structure
        let mutable world = World.sweep parms rng settings.BurnIn world
        [|
            for _ in 1 .. settings.NumSamples do
                world <- World.sweep parms rng settings.SampleInterval world
                countPairs world strands
        |]

    /// Runs the experiment, prints a table of results, and writes them
    /// to the given CSV file.
    let run (settings : Settings) (csvPath : string) =

        let structures = [| Hairpin; ShortDuplex; LongDuplex |]
        let combos =
            [|
                for structure in structures do
                    for kappa in settings.BendStiffnesses do
                        for kT in settings.Temperatures do
                            for seed in 0 .. settings.NumRuns - 1 do
                                structure, kappa, kT, seed
            |]

            // run every combination in parallel
        let mutable nDone = 0
        let results =
            combos
                |> Array.Parallel.map (fun (structure, kappa, kT, seed) ->
                    let samples = runOne settings structure kappa kT seed
                    let n = Interlocked.Increment(&nDone)
                    if n % 50 = 0 then
                        eprintfn "Finished run %d of %d" n combos.Length
                    (structure, kappa, kT), samples)
                |> Array.groupBy fst
                |> Array.map (fun (key, group) -> key, group |> Array.collect snd)
                |> Map

        printfn "Experiment 18: melting of hairpins and duplexes"
        printfn "Hairpin: P W W W Y Y X X X Q, folded (4 pairs)"
        printfn "Short duplex: P W W W Q with P X X X Q (5 pairs)"
        printfn "Long duplex: P W W W X X Y X Y Q with its copy (10 pairs)"
        printfn "%d runs of each combination, alone in a small world; %d samples per run, %d sweeps apart, after %d sweeps"
            settings.NumRuns settings.NumSamples settings.SampleInterval settings.BurnIn
        printParameters (getParameters settings.BendStiffnesses[0] nan)

        use writer = new StreamWriter(csvPath)
        writer.WriteLine("kappa,kT,structure,paired,mean_pairs")
        let invariant (x : float) = x.ToString(Globalization.CultureInfo.InvariantCulture)
        for kappa in settings.BendStiffnesses do
            printfn ""
            printfn "Strong bend stiffness %g: fraction of samples paired (mean pairs)" kappa
            printfn "   kT        hairpin     short duplex      long duplex"
            for kT in settings.Temperatures do
                let cells =
                    [
                        for structure in structures do
                            let samples = results[structure, kappa, kT]
                            let paired = samples |> Array.averageBy (fun n -> if n >= 2 then 1.0 else 0.0)
                            let mean = samples |> Array.averageBy float
                            writer.WriteLine(
                                String.Join(",", [ invariant kappa; invariant kT; $"{structure}"; invariant paired; invariant mean ]))
                            sprintf "%5.2f (%4.1f)" paired mean
                    ]
                printfn "%5.2f   %s     %s     %s" kT cells[0] cells[1] cells[2]

        printfn ""
        printfn "paired: at least two weak bonds within the hairpin, or between the duplex's strands."
        printfn "Results written to %s" (Path.GetFullPath csvPath)
