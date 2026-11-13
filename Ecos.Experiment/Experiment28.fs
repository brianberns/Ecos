namespace Ecos.Experiment

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks

open Ecos
open Ecos.Experiment.Experiment3

/// Experiment 28: does stress favor hairpin-formers in the new physics?
///
/// The physics and food are experiment 27's best variant (strands start
/// only from H-H, H-H pair energy 2.0, free H 0.8%). Each world starts
/// with one of each hairpin-former and one of each non-former: a strand
/// made by shuffling the letters within each arm of a former, so that it
/// has the same length, atoms, hinge, primer and caps, but can't fold
/// (no more than 2 pairs within itself) and pairs no more than 3 in a
/// row with a former or its copy.
///
/// Each cycle: feast (cold, copying, or folding once food runs out);
/// stress (just as cold, but caps not protected by a stack peel off);
/// melt (hot; ends that lost a cap peel away). A control has no cap
/// peeling. A run and its control use the same seed, so they are the
/// same until the first cap peels.
///
/// Capped strands are classified as exact formers, non-formers (either
/// way, or their copies), other hairpin-formers (any strand that is its
/// own reverse complement, with HH at its center, e.g. P-WWW-HH-XXX-Q),
/// other self-complementary strands (e.g. two formers joined end to end),
/// and junk.
///
/// Hypotheses: under stress, formers outlast non-formers, against the
/// control; and in time, shorter hairpin-formers (deletions that keep the
/// fold) take over from the formers.
module Experiment28 =

    /// One variant: the barrier to peeling caps that aren't protected
    /// by a stack during the stress phase. Infinity means none peel.
    type Variant =
        {
            StressCapPeelBarrier : float
        }

    /// Settings of the experiment.
    type Settings =
        {
            Variants : Variant[]
            NumRuns : int
            NumCycles : int
            FeastSweeps : int
            StressSweeps : int
            MeltSweeps : int

            /// Print a summary after every this many cycles.
            SummaryInterval : int
        }

    /// Non-formers: each former's arms with their letters shuffled.
    let nonFormers =
        [|
            "P-WXXYWXWWZW-HH-ZWXXXXYWXW-Q"
            "P-YYZWZYYZXY-HH-YYXZZZZWZY-Q"
        |]

    /// Default settings.
    let defaultSettings =
        {
            Variants =
                [|
                    { StressCapPeelBarrier = infinity }   // control
                    { StressCapPeelBarrier = 1.4 }        // nearly every unprotected cap peels (1.7 at kT 0.3 in experiment 26)
                |]
            NumRuns = 12   // with two variants, one world per logical core on a 24-core machine
            NumCycles = 15
            FeastSweeps = 300_000
            StressSweeps = 50_000
            MeltSweeps = 200_000
            SummaryInterval = 5
        }

    /// The spec of the given variant.
    let getSpec settings variant : ScenarioSpec =
        let settings27 =
            { Experiment27.defaultSettings with
                ColdSweeps = settings.FeastSweeps
                HotSweeps = settings.MeltSweeps }
        let spec = Experiment27.getSpec settings27 { HHEnergy = 2.0; HOccupancy = 0.8 }
        {
            spec with
                Name = "Experiment 28"
                Strands =
                    Array.append Experiment27.formers nonFormers
                        |> Array.map (fun pattern ->
                            { Pattern = pattern; Placement = Scattered None; Count = 1; IsTemplate = true; Color = None })
                Schedule =
                    Cycle [|
                        { Name = Some "feast"; Temperature = 0.25; NumSweeps = settings.FeastSweeps; HydrolysisRate = None; CapPeelBarrier = None }
                        { Name = Some "stress"; Temperature = 0.25; NumSweeps = settings.StressSweeps; HydrolysisRate = None; CapPeelBarrier = Some variant.StressCapPeelBarrier }
                        { Name = Some "melt"; Temperature = 3.0; NumSweeps = settings.MeltSweeps; HydrolysisRate = None; CapPeelBarrier = None }
                    |]
        }

    /// Complement of an atom letter.
    let private complement = function
        | 'W' -> 'X' | 'X' -> 'W' | 'Y' -> 'Z' | 'Z' -> 'Y'
        | 'P' -> 'Q' | 'Q' -> 'P' | c -> c

    let private rev (s : string) = String(Array.rev (s.ToCharArray()))

    /// Reverse complement of a sequence.
    let reverseComplement (s : string) = rev (String.map complement s)

    /// A canonical name for the given sequence, the same for the
    /// sequence read either way and its complement read either way.
    let canonical (s : string) =
        let c = String.map complement s
        [ s; rev s; c; rev c ] |> List.min

    let private formerKeys = Experiment27.formers |> Array.map (fun p -> canonical (p.Replace("-", "")))
    let private nonFormerKeys = nonFormers |> Array.map (fun p -> canonical (p.Replace("-", "")))

    /// Kind of a capped strand.
    type Kind =
        | Former
        | NonFormer
        | OtherFormer       // its own reverse complement, with HH at its center
        | OtherSelfComp     // its own reverse complement, otherwise
        | Junk

    let allKinds = [| Former; NonFormer; OtherFormer; OtherSelfComp; Junk |]

    /// Kinds whose strands are given a state.
    let stateKinds = [| Former; NonFormer; OtherFormer |]

    /// Kind of the given capped sequence.
    let getKind (s : string) =
        let key = canonical s
        if Array.contains key formerKeys then Former
        elif Array.contains key nonFormerKeys then NonFormer
        elif reverseComplement s = s then
            let n = s.Length
            if n % 2 = 0 && s.Substring(n / 2 - 1, 2) = "HH" then OtherFormer
            else OtherSelfComp
        else Junk

    /// Pairing state of a strand.
    type State =
        | Folded            // at least 4 pairs within itself
        | SiblingDuplex     // at least 4 pairs with a strand of the same sequence (or its copy)
        | PairedOther       // at least 4 pairs with any other strand
        | Single

    let allStates = [| Folded; SiblingDuplex; PairedOther; Single |]

    /// Census of a world.
    type Census =
        {
            /// Capped strands of each kind.
            Counts : Map<Kind, int>

            /// Strands of each state, by kind.
            States : Map<Kind * State, int>

            /// Strands that lack a cap at either end.
            Uncapped : int

            /// Free H atoms.
            FreeH : int

            /// Other hairpin-formers, by canonical sequence.
            OtherFormers : string[]
        }

    /// Takes a census of the given world.
    let takeCensus (world : World) =
        let strands =
            getStrands world
                |> Array.where (fun st -> st.AtomIds.Length >= 2 && not st.IsRing)
        let owner = Dictionary<AtomId, int>()
        strands |> Array.iteri (fun i st -> for id in st.AtomIds do owner[id] <- i)
        let letter id = (ScenarioSpec.toLetter world.Atoms[id].Type)[0]
        let seqs = strands |> Array.map (fun st -> st.AtomIds |> Array.map letter |> String)
        let keys = seqs |> Array.map canonical
        let isCapped (s : string) = "PQ".Contains s[0] && "PQ".Contains s[s.Length - 1]
        let kinds = seqs |> Array.map (fun s -> if isCapped s then Some (getKind s) else None)
        let weak = world.BondTypeMap[Weak]

        let getState i =
            let partners =
                strands[i].AtomIds
                    |> Seq.collect (fun id -> BondMap.getBonds id weak)
                    |> Seq.choose (fun p -> match owner.TryGetValue p with | true, j -> Some j | _ -> None)
                    |> Seq.toArray
            let within = (partners |> Array.where ((=) i)).Length / 2
            let sibling = partners |> Array.where (fun j -> j <> i && keys[j] = keys[i]) |> Array.length
            let other = partners |> Array.where (fun j -> j <> i) |> Array.length
            if within >= 4 then Folded
            elif sibling >= 4 then SiblingDuplex
            elif other >= 4 then PairedOther
            else Single

        let classified =
            [| for i in 0 .. strands.Length - 1 do
                match kinds[i] with
                    | Some kind -> kind, (if Array.contains kind stateKinds then Some (getState i) else None), i
                    | None -> () |]
        {
            Counts = allKinds |> Array.map (fun k -> k, classified |> Array.where (fun (k', _, _) -> k' = k) |> Array.length) |> Map
            States =
                Map [
                    for k in stateKinds do
                        for s in allStates do
                            (k, s), classified |> Array.where (fun (k', s', _) -> k' = k && s' = Some s) |> Array.length
                ]
            Uncapped = kinds |> Array.where Option.isNone |> Array.length
            FreeH = world.Atoms |> Seq.where (fun a -> a.Type = AtomType.H && not (owner.ContainsKey a.Id)) |> Seq.length
            OtherFormers = classified |> Array.choose (fun (k, _, i) -> if k = OtherFormer then Some keys[i] else None)
        }

    /// Censuses of one cycle.
    type CycleResult =
        {
            /// At the end of the feast.
            Fed : Census

            /// At the end of the stress phase.
            Stressed : Census
        }

    let private kindName = function
        | Former -> "formers" | NonFormer -> "non_formers" | OtherFormer -> "other_formers"
        | OtherSelfComp -> "other_self_comp" | Junk -> "junk"

    let private stateName = function
        | Folded -> "folded" | SiblingDuplex -> "duplex" | PairedOther -> "paired" | Single -> "single"

    let private invariant (x : float) = x.ToString(Globalization.CultureInfo.InvariantCulture)

    let private csvHeader =
        String.concat "," [
            yield! [ "stress_cap_peel_barrier"; "seed"; "cycle"; "phase" ]
            for k in allKinds do yield kindName k
            for k in stateKinds do
                for s in allStates do yield $"{kindName k}_{stateName s}"
            yield! [ "uncapped"; "free_h"; "mean_other_former_length" ]
        ]

    let private csvRow variant seed iCycle phase (c : Census) =
        String.concat "," [
            yield! [ invariant variant.StressCapPeelBarrier; string seed; string iCycle; phase ]
            for k in allKinds do yield string c.Counts[k]
            for k in stateKinds do
                for s in allStates do yield string c.States[k, s]
            yield string c.Uncapped
            yield string c.FreeH
            yield (if c.OtherFormers.Length = 0 then "" else invariant (c.OtherFormers |> Array.averageBy (fun s -> float s.Length)))
        ]

    /// Share of the given census's formers and non-formers that are
    /// formers, if there are any.
    let tryGetFormerShare (c : Census) =
        let f = c.Counts[Former]
        let n = c.Counts[NonFormer]
        if f + n = 0 then None else Some (float f / float (f + n))

    /// First cycle in which other hairpin-formers outnumber exact
    /// formers after the feast, if any.
    let tryGetTakeover (cycles : CycleResult[]) =
        cycles
            |> Array.tryFindIndex (fun r -> r.Fed.Counts[OtherFormer] > r.Fed.Counts[Former])
            |> Option.map ((+) 1)

    /// Prints a summary of the given results, complete up to the given
    /// cycle.
    let printSummary settings (results : Map<Variant * int, CycleResult option[]>) upTo =
        printfn ""
        printfn "Summary after %d cycles (means over runs; states are shares of that kind)" upTo
        let cycles =
            [ 1; 2; 5; 10; 20; 30; 40; 50; 60; 70; 80; 90; 100 ]
                |> List.filter (fun c -> c <= upTo)
                |> fun cs -> if List.contains upTo cs then cs else cs @ [ upTo ]
        for variant in settings.Variants do
            let runs = [| for seed in 0 .. settings.NumRuns - 1 -> results[variant, seed] |]
            printfn ""
            printfn "Stress cap peel barrier %g" variant.StressCapPeelBarrier
            printfn "        |------------------ after the feast ------------------|   |------ after the stress ------|"
            printfn "  cycle  formers  folded  duplex  non-f.  other-f.  self-c.  junk   formers  non-f.  other-f.  junk  former share"
            for iCycle in cycles do
                let rs = runs |> Array.map (fun r -> r[iCycle - 1].Value)
                let mean f = rs |> Array.averageBy (f >> float)
                let share kind state =
                    let n = rs |> Array.sumBy (fun r -> r.Fed.Counts[kind])
                    if n = 0 then nan else float (rs |> Array.sumBy (fun r -> r.Fed.States[kind, state])) / float n
                let shares = rs |> Array.choose (fun r -> tryGetFormerShare r.Stressed)
                printfn "  %5d  %7.1f  %6.2f  %6.2f  %6.1f  %8.1f  %7.1f  %4.1f   %7.1f  %6.1f  %8.1f  %4.1f  %6.2f (%d)"
                    iCycle
                    (mean (fun r -> r.Fed.Counts[Former])) (share Former Folded) (share Former SiblingDuplex)
                    (mean (fun r -> r.Fed.Counts[NonFormer])) (mean (fun r -> r.Fed.Counts[OtherFormer]))
                    (mean (fun r -> r.Fed.Counts[OtherSelfComp])) (mean (fun r -> r.Fed.Counts[Junk]))
                    (mean (fun r -> r.Stressed.Counts[Former])) (mean (fun r -> r.Stressed.Counts[NonFormer]))
                    (mean (fun r -> r.Stressed.Counts[OtherFormer])) (mean (fun r -> r.Stressed.Counts[Junk]))
                    (if shares.Length = 0 then nan else Array.average shares) shares.Length

                // takeover by other hairpin-formers
            let takeovers = runs |> Array.map (fun r -> r[.. upTo - 1] |> Array.map Option.get |> tryGetTakeover)
            printfn "  Other hairpin-formers outnumber formers in %d of %d runs; first at cycles: %s"
                (takeovers |> Array.where Option.isSome |> Array.length) runs.Length
                (takeovers |> Array.choose id |> Array.sort |> Array.map string |> String.concat " ")

                // against the control, run for run
            let control = settings.Variants[0]
            if variant <> control then
                let diffs =
                    [| for seed in 0 .. settings.NumRuns - 1 do
                        let get v =
                            let cycles = results[v, seed]
                            cycles[upTo - 1].Value.Stressed |> tryGetFormerShare
                        match get variant, get control with
                            | Some s, Some c -> yield s - c
                            | _ -> () |]
                let nUp = diffs |> Array.where (fun d -> d > 0.0) |> Array.length
                let nDown = diffs |> Array.where (fun d -> d < 0.0) |> Array.length
                printfn "  Against the control at cycle %d, run for run: former share %+.3f on average; higher in %d runs, lower in %d; sign test p = %.3g (higher), %.3g (lower)"
                    upTo (if diffs.Length = 0 then nan else Array.average diffs) nUp nDown
                    (Experiment26.getSignTestP nUp nDown) (Experiment26.getSignTestP nDown nUp)

                // commonest other hairpin-formers
            let commonest =
                runs
                    |> Array.collect (fun r -> r[upTo - 1].Value.Fed.OtherFormers)
                    |> Array.countBy id
                    |> Array.sortByDescending snd
                    |> Array.truncate 6
            if commonest.Length > 0 then
                printfn "  Commonest other hairpin-formers at cycle %d (all runs): %s"
                    upTo (commonest |> Array.map (fun (s, n) -> $"{s} x{n}") |> String.concat ", ")
        stdout.Flush()

    /// Runs the experiment, prints summaries as it goes, and writes
    /// results to the given CSV file as each cycle finishes.
    let run (settings : Settings) (csvPath : string) =

        let combos =
            [| for variant in settings.Variants do
                for seed in 0 .. settings.NumRuns - 1 do
                    variant, seed |]

        printfn "Experiment 28: does stress favor hairpin-formers in the new physics?"
        printfn "Formers: %s" (String.concat ", " Experiment27.formers)
        printfn "Non-formers: %s" (String.concat ", " nonFormers)
        printfn "%d runs of %d cycles of each variant; feast kT 0.25 for %d sweeps, stress kT 0.25 for %d, melt kT 3 for %d"
            settings.NumRuns settings.NumCycles settings.FeastSweeps settings.StressSweeps settings.MeltSweeps
        printfn "Strands start only from H-H (start barrier inf, H-H 0); H-H pair energy 2; free W, X, Y, Z 2%% each, H 0.8%%, P and Q 0.4%% each"
        printParameters (ScenarioSpec.toParameters (getSpec settings settings.Variants[0]).Parameters)
        printfn ""
        stdout.Flush()

        let results =
            combos
                |> Array.map (fun combo -> combo, Array.create settings.NumCycles (None : CycleResult option))
                |> Map
        let nFinished : int[] = Array.zeroCreate (settings.NumCycles + 1)
        let stopwatch = Diagnostics.Stopwatch.StartNew()
        let gate = obj ()
        use writer = new StreamWriter(csvPath)
        writer.WriteLine(csvHeader)
        writer.Flush()

        let onCycle (variant, seed) iCycle result =
            lock gate (fun () ->
                let cycles = results[variant, seed]
                cycles[iCycle - 1] <- Some result
                writer.WriteLine(csvRow variant seed iCycle "feast" result.Fed)
                writer.WriteLine(csvRow variant seed iCycle "stress" result.Stressed)
                writer.Flush()
                nFinished[iCycle] <- nFinished[iCycle] + 1
                if nFinished[iCycle] = combos.Length then
                    printfn "Finished cycle %d of %d (%.0f min)" iCycle settings.NumCycles stopwatch.Elapsed.TotalMinutes
                    if iCycle % settings.SummaryInterval = 0 || iCycle = settings.NumCycles then
                        printSummary settings results iCycle
                        printfn ""
                    stdout.Flush())

        let runOne (variant, seed) =
            let mutable run = Run.create seed (ScenarioSpec.toScenario (getSpec settings variant))
            for iCycle in 1 .. settings.NumCycles do
                run <- Run.advance settings.FeastSweeps run
                let fed = takeCensus run.World
                run <- Run.advance settings.StressSweeps run
                let stressed = takeCensus run.World
                run <- Run.advance settings.MeltSweeps run
                onCycle (variant, seed) iCycle { Fed = fed; Stressed = stressed }

            // one dedicated thread per world, so that all worlds advance together
        combos
            |> Array.map (fun combo ->
                Task.Factory.StartNew((fun () -> runOne combo), TaskCreationOptions.LongRunning))
            |> Task.WaitAll

        printfn "Results written to %s (%.0f min)" (Path.GetFullPath csvPath) stopwatch.Elapsed.TotalMinutes
