namespace Ecos.Experiment

open System
open System.Collections.Generic
open System.IO
open System.Threading

open Ecos
open Ecos.Experiment.Experiment3

/// Experiment 27: copy in a feast, fold in a famine, with a primer.
///
/// Experiment 26 found that a hinged hairpin-former seldom folds, because
/// copying and folding get in each other's way. This experiment tries a
/// switch between them (the riboswitch idea in the README):
///
/// - Only a primer can start a strand: the strong start barrier is
///   infinite, except between two H atoms (start barrier 0). So a copy
///   can only start at a template's hinge (HH), where two free H atoms
///   pair side by side and join. Copies don't start on the arms, so they
///   don't run into a half-folded strand and switch templates.
/// - Weak pairs are weak (1.4) and stacks modest (0.6), so that unstacked
///   and lightly stacked pairs come apart, and stalled starts clear.
/// - Strands are floppy (bend stiffness 1, hinges free), so a strand left
///   alone folds quickly.
/// - Food is balanced: two hairpin-formers, one mostly W/X and one mostly
///   Y/Z, and H, P and Q in the proportions a copy uses, so all of them
///   run out at about the same time.
///
/// A closed world starts with one of each former among free atoms and
/// goes through cycles of cold (copying, or folding once food runs out)
/// and heat (separation). At the end of each cold phase, each exact
/// former is classified as folded, in a duplex with a sibling (each
/// former is its own reverse complement, so two can pair), paired with
/// some other strand (e.g. a partial copy), or single; and junk (capped
/// strands that aren't formers) is counted.
///
/// Hypothesis: the population grows until H runs out, after which
/// formers fold rather than pair; and junk stays rare. Variants: the H-H
/// pair energy (2.0, which makes one free H on a hinge long-lived, or the
/// ordinary 1.4, which needs two H at once, so that copying should depend
/// more steeply on H), and the H supply.
module Experiment27 =

    /// One combination of the varied settings.
    type Variant =
        {
            /// Weak H-H pair energy.
            HHEnergy : float

            /// Free H atoms, as a percentage of sites.
            HOccupancy : float
        }

    /// Settings of the experiment.
    type Settings =
        {
            Variants : Variant[]
            NumRuns : int
            NumCycles : int
            ColdSweeps : int
            HotSweeps : int
        }

    /// The two hairpin-formers: arms that are reverse complements of
    /// each other, around an HH hinge.
    let formers =
        [|
            "P-WWXWYXZXWW-HH-XXWYWZXWXX-Q"
            "P-YYZYWZXZYY-HH-ZZYWYXZYZZ-Q"
        |]

    /// Default settings.
    let defaultSettings =
        {
            Variants =
                [|
                    for hh in [ 2.0; 1.4 ] do
                        for h in [ 0.8; 0.4 ] do
                            { HHEnergy = hh; HOccupancy = h }
                |]
            NumRuns = 16
            NumCycles = 20
            ColdSweeps = 300_000
            HotSweeps = 200_000
        }

    /// The spec of the given variant.
    let getSpec settings variant : ScenarioSpec =
        let standard = ScenarioSpec.ofScenario 0 (Scenario.createScenarioC false)
        let strong =
            {
                standard.Parameters.Strong with
                    Energy = 34
                    PairEnergies = Map.empty
                    Barrier = 40
                    BarrierStackReduction = 40
                    StartBarrier = infinity
                    PairStartBarriers = Some (Map [ "H-H", 0.0 ])
                    Entropy = 0
                    HydrolysisRate = 0
                    ProtectedHydrolysisRate = 0
                    BendLimit = Bend.rightAngle
                    BendStiffness = 1
                    BendStiffnesses = Map [ "H", 0.0 ]
            }
        let weak =
            {
                standard.Parameters.Weak with
                    Energy = 1.4
                    PairEnergies = Map [ "H-H", variant.HHEnergy ]
                    Barrier = 0
                    BarrierStackReduction = 0
                    StartBarrier = 0
                    PairStartBarriers = None
                    Entropy = 3
                    HydrolysisRate = 0
                    ProtectedHydrolysisRate = 0
                    BendLimit = Bend.any
                    BendStiffness = 0
                    BendStiffnesses = Map.empty
            }
        {
            Name = "Experiment 27"
            Width = 160
            Height = 160
            Walls = [||]
            Strands =
                formers |> Array.map (fun pattern ->
                    { Pattern = pattern; Placement = Scattered None; Count = 1; IsTemplate = true; Color = None })
            WeakBonds = [||]
            Occupancy =
                Map [
                    "W", 2.0; "X", 2.0; "Y", 2.0; "Z", 2.0
                    "H", variant.HOccupancy; "P", 0.4; "Q", 0.4
                ]
            FoodWithin = None
            Parameters =
                {
                    MoveAtomWeight = 0.5
                    ToggleStrongBondWeight = 0.2
                    ToggleWeakBondWeight = 0.3
                    Strong = strong
                    Weak = weak
                    StackEnergy = 0.6
                    PeelBarrier = 6
                    CapPeelBarrier = infinity
                }
            Schedule =
                Cycle [|
                    { Name = Some "cold"; Temperature = 0.25; NumSweeps = settings.ColdSweeps; HydrolysisRate = None; CapPeelBarrier = None }
                    { Name = Some "hot"; Temperature = 3.0; NumSweeps = settings.HotSweeps; HydrolysisRate = None; CapPeelBarrier = None }
                |]
        }

    /// Complement of an atom letter.
    let private complement = function
        | 'W' -> 'X' | 'X' -> 'W' | 'Y' -> 'Z' | 'Z' -> 'Y'
        | 'P' -> 'Q' | 'Q' -> 'P' | c -> c

    /// The given sequence read either way, and its complement read
    /// either way.
    let private variants (s : string) =
        let rev (s : string) = String(Array.rev (s.ToCharArray()))
        let c = String.map complement s
        set [ s; rev s; c; rev c ]

    /// Each former's family: its exact sequence, its copy, either way.
    let private families =
        formers |> Array.map (fun p -> variants (p.Replace("-", "")))

    /// State of an exact former at the end of a cold phase.
    type FormerState =
        | Folded            // at least 4 pairs within itself
        | SiblingDuplex     // at least 4 pairs with a former of its family
        | PairedOther       // at least 4 pairs with any other strand
        | Single

    /// Census at the end of a cold phase.
    type Census =
        {
            /// Exact formers of each family.
            Formers : int[]

            /// Exact formers in each state, both families together.
            States : Map<FormerState, int>

            /// Capped strands that aren't exact formers.
            Junk : int

            /// Junk of 8 atoms or fewer that carries the hinge (HH).
            TinyPrimed : int

            /// Strands that lack a cap at either end (e.g. partial copies).
            Uncapped : int

            /// Free (unbonded by strong bonds) atoms of each type.
            Free : Map<char, int>
        }

    let allStates = [| Folded; SiblingDuplex; PairedOther; Single |]

    /// Takes a census of the given world.
    let takeCensus (world : World) =
        let strands = getStrands world |> Array.where (fun st -> st.AtomIds.Length >= 2)
        let owner = Dictionary<AtomId, int>()
        strands |> Array.iteri (fun i st -> for id in st.AtomIds do owner[id] <- i)
        let letter id = (ScenarioSpec.toLetter world.Atoms[id].Type)[0]
        let seqs = strands |> Array.map (fun st -> st.AtomIds |> Array.map letter |> String)
        let familyOf i = families |> Array.tryFindIndex (fun f -> f.Contains seqs[i])
        let weak = world.BondTypeMap[Weak]
        let isCapped (s : string) = "PQ".Contains s[0] && "PQ".Contains s[s.Length - 1]

        let formerStates =
            [|
                for i in 0 .. strands.Length - 1 do
                    match familyOf i with
                        | Some fam ->
                            let partners =
                                strands[i].AtomIds
                                    |> Seq.collect (fun id -> BondMap.getBonds id weak)
                                    |> Seq.choose (fun p -> match owner.TryGetValue p with | true, j -> Some j | _ -> None)
                                    |> Seq.toArray
                            let within = (partners |> Array.where ((=) i)).Length / 2
                            let sibling = partners |> Array.where (fun j -> j <> i && familyOf j = Some fam) |> Array.length
                            let other = partners |> Array.where (fun j -> j <> i) |> Array.length
                            let state =
                                if within >= 4 then Folded
                                elif sibling >= 4 then SiblingDuplex
                                elif other >= 4 then PairedOther
                                else Single
                            fam, state
                        | None -> ()
            |]
        let junk =
            [| for i in 0 .. strands.Length - 1 do
                if not strands[i].IsRing && isCapped seqs[i] && (familyOf i).IsNone then seqs[i] |]
        let free =
            world.Atoms
                |> Seq.where (fun a -> not (owner.ContainsKey a.Id))
                |> Seq.countBy (fun a -> (ScenarioSpec.toLetter a.Type)[0])
                |> Map
        {
            Formers = Array.init families.Length (fun f -> formerStates |> Array.where (fst >> (=) f) |> Array.length)
            States = allStates |> Array.map (fun s -> s, formerStates |> Array.where (snd >> (=) s) |> Array.length) |> Map
            Junk = junk.Length
            TinyPrimed = junk |> Array.where (fun s -> s.Length <= 8 && s.Contains "HH") |> Array.length
            Uncapped = seqs |> Array.mapi (fun i s -> not strands[i].IsRing && not (isCapped s)) |> Array.where id |> Array.length
            Free = free
        }

    /// Runs one world, taking a census at the end of each cold phase.
    let runOne settings variant seed onCycle =
        let mutable run = Run.create seed (ScenarioSpec.toScenario (getSpec settings variant))
        [|
            for iCycle in 1 .. settings.NumCycles do
                run <- Run.advance settings.ColdSweeps run
                let census = takeCensus run.World
                run <- Run.advance settings.HotSweeps run
                onCycle iCycle
                census
        |]

    let private stateName = function
        | Folded -> "folded" | SiblingDuplex -> "sibling_duplex"
        | PairedOther -> "paired_other" | Single -> "single"

    /// Runs the experiment, prints tables of results, and writes them
    /// to the given CSV file.
    let run (settings : Settings) (csvPath : string) =

        let combos =
            [| for variant in settings.Variants do
                for seed in 0 .. settings.NumRuns - 1 do
                    variant, seed |]

        printfn "Experiment 27: copy in a feast, fold in a famine, with a primer"
        printfn "Formers: %s" (String.concat ", " formers)
        printfn "%d runs of %d cycles of each variant (H-H pair energy x free H); cold kT 0.25 for %d sweeps, hot kT 3 for %d"
            settings.NumRuns settings.NumCycles settings.ColdSweeps settings.HotSweeps
        printfn "Strands start only from H-H (start barrier inf, H-H 0); free W, X, Y, Z 2%% each, P and Q 0.4%% each"
        printParameters (ScenarioSpec.toParameters (getSpec settings settings.Variants[0]).Parameters)
        printfn ""
        stdout.Flush()

        let nFinished : int[] = Array.zeroCreate (settings.NumCycles + 1)
        let stopwatch = Diagnostics.Stopwatch.StartNew()
        let onCycle iCycle =
            if Interlocked.Increment(&nFinished[iCycle]) = combos.Length then
                printfn "Finished cycle %d of %d (%.0f min)" iCycle settings.NumCycles stopwatch.Elapsed.TotalMinutes
                stdout.Flush()
        let results =
            combos
                |> Array.Parallel.map (fun (variant, seed) ->
                    (variant, seed), runOne settings variant seed onCycle)
                |> Map

        let invariant (x : float) = x.ToString(Globalization.CultureInfo.InvariantCulture)
        do
            use writer = new StreamWriter(csvPath)
            writer.WriteLine(
                "hh_energy,h_occupancy,seed,cycle,formers_wx,formers_yz,"
                    + (allStates |> Array.map stateName |> String.concat ",")
                    + ",junk,tiny_primed,uncapped,free_h,free_w,free_x,free_y,free_z")
            for variant in settings.Variants do
                for seed in 0 .. settings.NumRuns - 1 do
                    results[variant, seed] |> Array.iteri (fun i c ->
                        let free ch = c.Free |> Map.tryFind ch |> Option.defaultValue 0
                        writer.WriteLine(
                            String.Join(",",
                                [
                                    yield invariant variant.HHEnergy
                                    yield invariant variant.HOccupancy
                                    yield string seed
                                    yield string (i + 1)
                                    yield string c.Formers[0]
                                    yield string c.Formers[1]
                                    for s in allStates do yield string c.States[s]
                                    yield string c.Junk
                                    yield string c.TinyPrimed
                                    yield string c.Uncapped
                                    for ch in "HWXYZ" do yield string (free ch)
                                ])))

            // summary: means over runs at selected cycles
        printfn ""
        printfn "Means over runs at the end of the cold phase. States are shares of exact formers."
        for variant in settings.Variants do
            let runs = [| for seed in 0 .. settings.NumRuns - 1 -> results[variant, seed] |]
            printfn ""
            printfn "H-H pair energy %g, free H %g%%" variant.HHEnergy variant.HOccupancy
            printfn "   cycle  formers (WX+YZ)  folded  sib.duplex  paired  single   junk  tiny  uncapped  free H"
            for iCycle in [ 1; 3; 5; 10; 15; 20 ] |> List.filter (fun c -> c <= settings.NumCycles) do
                let cs = runs |> Array.map (fun r -> r[iCycle - 1])
                let mean f = cs |> Array.averageBy (f >> float)
                let nFormers = cs |> Array.sumBy (fun c -> Array.sum c.Formers)
                let share s =
                    if nFormers = 0 then nan
                    else float (cs |> Array.sumBy (fun c -> c.States[s])) / float nFormers
                printfn "   %5d  %5.1f (%4.1f+%4.1f)   %5.2f   %5.2f      %5.2f   %5.2f  %5.1f %5.1f  %6.1f  %6.1f"
                    iCycle (mean (fun c -> Array.sum c.Formers)) (mean (fun c -> c.Formers[0])) (mean (fun c -> c.Formers[1]))
                    (share Folded) (share SiblingDuplex) (share PairedOther) (share Single)
                    (mean _.Junk) (mean _.TinyPrimed) (mean _.Uncapped)
                    (mean (fun c -> c.Free |> Map.tryFind 'H' |> Option.defaultValue 0))
            let final = runs |> Array.map (fun r -> Array.sum r[settings.NumCycles - 1].Formers) |> Array.sort
            let tinyRuns = runs |> Array.where (fun r -> r[settings.NumCycles - 1].TinyPrimed > 0) |> Array.length
            printfn "   formers at cycle %d by run: %s; runs with tiny primed junk: %d of %d"
                settings.NumCycles (final |> Array.map string |> String.concat " ") tinyRuns runs.Length
        printfn ""
        printfn "Results written to %s (%.0f min)" (Path.GetFullPath csvPath) stopwatch.Elapsed.TotalMinutes
