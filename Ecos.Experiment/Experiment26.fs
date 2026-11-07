namespace Ecos.Experiment

open System
open System.IO
open System.Threading

open Ecos
open Ecos.Experiment.Experiment3

/// Experiment 26: folding in a famine, with a hinge. Experiment 23 asked
/// whether stress favors strands that fold when they starve, but its
/// hairpin-former hardly ever folded, so its uncopied strands were as
/// exposed as the non-formers'. Experiment 24h's hinged hairpin-former
/// folds about half the time in a famine, and folded strands then
/// survive a stress about as well as copied ones and much better than
/// single ones (experiment 25). Does that now pay?
///
/// A closed world, like one of experiment 24's chambers, is seeded with
/// equal numbers of the hinged hairpin-former and of a non-former made
/// of the same atoms, with the same hinge in the same place, in an order
/// that can't fold. Nothing is added or removed: food is what the world
/// starts with, and what the death of strands frees. The world goes
/// through cycles of:
///
/// - a feast: cold, for copying;
/// - stress: just as cold, but caps peel off unless a stack protects
///   them, as it does in a duplex or a folded hairpin;
/// - a melt: hot, so that duplexes and hairpins come apart, and strands
///   that lost a cap peel away from their exposed ends.
///
/// Nothing hydrolyzes. (In an earlier version of this experiment, stress
/// hydrolyzed unprotected bonds anywhere along a strand, which cut
/// strands into fragments that then multiplied on their own.)
///
/// The idea: early on, food is plentiful, and both kinds are copied, so
/// both are paired, and protected, through the stress. Later, the
/// population has used up the food, and most strands that come out of a
/// melt can't be copied. A non-former is then left single and exposed,
/// while a hairpin-former can fold and protect itself.
///
/// A control has no cap peeling in the stress phase either. A run and
/// its control use the same seed, so they are the same until the first
/// cap peels.
///
/// Copies slip, so chains are counted by family: each counted chain is
/// in the family of the seed it is nearest to (by edit distance, read
/// either way, against each kind and its copy), if it is near enough.
/// Others are counted as other.
module Experiment26 =

    /// A variant of the experiment.
    type Variant =
        {
            /// Barrier to peeling caps that aren't protected by a stack
            /// during the stress phase. Infinity means none peel.
            StressCapPeelBarrier : float
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

            /// Types of free atoms.
            FoodTypes : AtomType[]

            /// Number of free atoms to start with of each of the most
            /// demanded types (see getFoodSupply).
            NumFoodMax : int

            /// Strong-bond bend stiffness, with the standard bend limit
            /// (right angles).
            BendStiffness : float

            /// Bend stiffness of the hinge atoms (H).
            HingeStiffness : float

            /// Energy of a weak bond (a pair) between atoms with no pair
            /// energy of their own.
            PairEnergy : float

            /// Energy of a stack, beyond its pairs' energies.
            StackEnergy : float

            /// Energy of a weak bond between hinge atoms (H-H), if it has
            /// one of its own. A higher energy holds free H atoms on a
            /// hinge long enough to join, so that copies start there.
            HingePairEnergy : Option<float>

            /// Weights of attempts to move an atom, toggle a strong
            /// bond, and toggle a weak bond. They change how fast things
            /// happen, not the equilibrium.
            EventWeights : float * float * float

            /// Shortest capped chain counted in either family. Shorter
            /// ones are counted as other.
            MinLength : int

            /// Greatest edit distance at which a chain is counted in a
            /// family. Chains farther from both are counted as other.
            MaxEditDistance : int

            /// Shortest hairpin stem that makes a chain foldable.
            MinStem : int

            /// Shortest stem, in pairs, of a chain counted as folded: the
            /// longest run of its atoms that are each paired within it
            /// must be at least twice this.
            MinFoldedStem : int

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

    /// The hairpin-former: P, a stem that mixes all four coding types,
    /// a hinge (H H), the stem's reverse complement, Q. Its stem pairs
    /// strongly only in register: it folds with 11 pairs (caps included),
    /// but its longest misaligned fold is 2 pairs. H pairs with itself,
    /// so the strand is its own copy, and the hinge, which bends freely,
    /// is where it folds.
    let hairpinFormer =
        let stem =
            [| AtomType.W; AtomType.W; AtomType.X; AtomType.W; AtomType.Y
               AtomType.X; AtomType.Z; AtomType.X; AtomType.W; AtomType.W |]
        [|
            AtomType.P
            yield! stem
            AtomType.H; AtomType.H
            yield! stem |> Array.rev |> Array.map complement
            AtomType.Q
        |]

    /// The non-former: the hinged hairpin-former's atoms, with its hinge
    /// and caps in place and its arms' atoms shuffled until its longest
    /// stem is 2, it and its copy pair with the hairpin-former at most 3
    /// in a row, and no type repeats more than twice in a row (so that it
    /// shouldn't slip much more than the hairpin-former does).
    let nonFormer =
        [|
            AtomType.P
            AtomType.W; AtomType.Z; AtomType.X; AtomType.Z; AtomType.X
            AtomType.X; AtomType.Y; AtomType.W; AtomType.W; AtomType.X
            AtomType.H; AtomType.H
            AtomType.W; AtomType.X; AtomType.Y; AtomType.X; AtomType.W
            AtomType.X; AtomType.W; AtomType.X; AtomType.W; AtomType.W
            AtomType.Q
        |]

    /// Default settings: the hinged hairpin-former at the standard bends,
    /// two of each kind in a single chamber, enough food for about 15
    /// copies, and experiment 23's phases. Pairs are weak (1.4) unless
    /// stacked, so copies rarely start on their own, but H-H pairs are
    /// strong (2.5), so they start at the hinge: food decides whether a
    /// hairpin-former is copied or folds. Strong bonds are toggled twice
    /// as often as the standard, so that templated joins keep pace with
    /// weak pairs breaking; the stress cap peel barriers are raised by
    /// kT ln 2 (about 0.2) to match.
    let defaultSettings =
        {
            HairpinFormer = hairpinFormer
            NonFormer = nonFormer
            NumSeeds = 2
            FoodTypes =
                [| AtomType.W; AtomType.X; AtomType.Y; AtomType.Z
                   AtomType.P; AtomType.Q; AtomType.H |]
            NumFoodMax = 120
            BendStiffness = Scenario.defaultBendStiffness
            HingeStiffness = Scenario.defaultHingeStiffness
            PairEnergy = 1.4
            StackEnergy = 1.0
            HingePairEnergy = Some 2.5
            EventWeights = 0.5, 0.2, 0.3
            MinLength = 10
            MaxEditDistance = 4
            MinStem = 6
            MinFoldedStem = 4
            Feast = { Name = Some "feast"; Temperature = 0.3; NumSweeps = 200_000; HydrolysisRate = None; CapPeelBarrier = None }
            Stress = { Name = Some "stress"; Temperature = 0.3; NumSweeps = 50_000; HydrolysisRate = None; CapPeelBarrier = None }
            Melt = { Name = Some "melt"; Temperature = 3.0; NumSweeps = 100_000; HydrolysisRate = None; CapPeelBarrier = None }
            Variants =
                [|
                    { StressCapPeelBarrier = Double.PositiveInfinity }   // control
                    { StressCapPeelBarrier = 2.2 }
                    { StressCapPeelBarrier = 1.7 }   // nearly every unprotected cap peels
                |]
            NumRuns = 16
            NumCycles = 24
            NumCommonSequences = 8
        }

    /// Width of the world.
    let private worldSize = 160

    /// The hairpin-former.
    let private getHairpinFormer settings = settings.HairpinFormer

    /// Least share of the maximum that any food type starts with. A copy
    /// grows only as fast as the atom it needs next arrives, so a scarce
    /// type slows every copy that needs it.
    let private minFoodShare = 0.5

    /// Free atoms of each type to start with, for copying the seeded
    /// strands. Copying a strand uses its complement's atoms, and copying
    /// the copy uses the strand's, so a type's demand is the most, over
    /// the strands, of its count in a strand and its complement's. Each
    /// type starts in proportion to its demand (rounded up), so that the
    /// most demanded types get the maximum, but with at least the minimum
    /// share of it.
    let getFoodSupply settings =
        let strands = [| getHairpinFormer settings; settings.NonFormer |]
        let numMax = settings.NumFoodMax
        let getDemand atomType =
            strands
                |> Array.map (fun strand ->
                    let count t = strand |> Array.where ((=) t) |> Array.length
                    max (count atomType) (count (complement atomType)))
                |> Array.max
        let maxDemand = settings.FoodTypes |> Array.map getDemand |> Array.max
        let minNum = int (ceil (minFoodShare * float numMax))
        settings.FoodTypes
            |> Array.map (fun atomType ->
                let num = (numMax * getDemand atomType + maxDemand - 1) / maxDemand
                atomType, max num minNum)

    /// Adds the given number of free atoms of each type at random empty
    /// places (with even coordinates) anywhere in the given world.
    let private addFood (rng : Random) (supply : (AtomType * int)[]) (world : World) =
        (world, supply)
            ||> Array.fold (fun world (atomType, numFood) ->
                (world, [ 1 .. numFood ])
                    ||> Seq.fold (fun world _ ->
                        let rec loop () =
                            let loc = Point.create (2 * rng.Next(world.Size.X / 2), 2 * rng.Next(world.Size.Y / 2))
                            match World.tryAddAtom atomType loc world with
                                | Some (world, _) -> world
                                | None -> loop ()
                        loop ()))

    /// Fewest atoms between the two arms of a hairpin stem.
    let private minLoop = 2

    /// The longest hairpin stem the given sequence could form: the most
    /// consecutive complementary pairs between two stretches of it read
    /// in opposite directions, with at least minLoop atoms between them.
    let getMaxStem (sequence : AtomType[]) =
        let n = sequence.Length
        let stemAt a b =   // stem closing on the pair (a, b), growing inward
            let rec grow k =
                let i = a + k
                let j = b - k
                if j - i - 1 >= minLoop && complement sequence[i] = sequence[j] then
                    grow (k + 1)
                else k
            grow 0
        seq {
            for a in 0 .. n - 1 do
                for b in a + 1 .. n - 1 do
                    stemAt a b
        } |> Seq.fold max 0

    /// Chance of at least the given number of successes in the given
    /// number of trials, each with probability one half: the one-sided p
    /// value of a sign test.
    let getSignTestP nAhead nBehind =
        let n = nAhead + nBehind
        let choose n k =
            Seq.fold (fun acc i -> acc * float (n - k + i) / float i) 1.0 (seq { 1 .. k })
        seq { nAhead .. n }
            |> Seq.sumBy (fun k -> choose n k / 2.0 ** float n)

    /// Family of a chain.
    type private Family =
        | HairpinFamily
        | NonFormerFamily

    /// All families, in order.
    let private families = [| HairpinFamily; NonFormerFamily |]

    /// Sequence of the given atom types, as text.
    let private toSequenceText (types : AtomType[]) =
        types |> Array.map string |> String.concat ""

    /// Edit (Levenshtein) distance between the given texts.
    let private getEditDistance (a : string) (b : string) =
        let mutable prev = Array.init (b.Length + 1) id
        for i in 1 .. a.Length do
            let cur = Array.zeroCreate (b.Length + 1)
            cur[0] <- i
            for j in 1 .. b.Length do
                let cost = if a[i - 1] = b[j - 1] then 0 else 1
                cur[j] <- min (min (prev[j] + 1) (cur[j - 1] + 1)) (prev[j - 1] + cost)
            prev <- cur
        prev[b.Length]

    /// Each family's sequences, as text: each kind and its copy, each
    /// read either way.
    let private getFamilyTexts settings =
        let getTexts (strand : AtomType[]) =
            let copy = strand |> Array.rev |> Array.map complement
            [| strand; Array.rev strand; copy; Array.rev copy |]
                |> Array.map toSequenceText
                |> Array.distinct
        [|
            HairpinFamily, getTexts (getHairpinFormer settings)
            NonFormerFamily, getTexts settings.NonFormer
        |]

    /// Family of the chain with the given sequence, as text: the one
    /// with the nearest sequence, if just one is nearest and it is
    /// within the given edit distance.
    let private tryGetFamily maxDistance familyTexts (text : string) =
        let distances =
            familyTexts
                |> Array.map (fun (family, texts) ->
                    family, texts |> Array.map (getEditDistance text) |> Array.min)
        let best = distances |> Array.map snd |> Array.min
        match distances |> Array.where (snd >> (=) best) with
            | [| family, _ |] when best <= maxDistance -> Some family
            | _ -> None

    /// Sequence of the given chain, as text.
    let private getText (world : World) (atomIds : AtomId[]) =
        atomIds
            |> Array.map (fun id -> world.Atoms[id].Type)
            |> toSequenceText

    /// Labels the capped strands of the given world that are long
    /// enough to count by family: "former" or "non-former". Answers each
    /// labeled atom's label, by atom ID.
    let labelStrands settings =
        let familyTexts = getFamilyTexts settings
        fun (world : World) ->
            getStrands world
                |> Seq.where (fun strand ->
                    strand.AtomIds.Length >= settings.MinLength
                        && isCapped world strand)
                |> Seq.choose (fun strand ->
                    tryGetFamily settings.MaxEditDistance familyTexts (getText world strand.AtomIds)
                        |> Option.map (fun family ->
                            let label =
                                match family with
                                    | HairpinFamily -> "former"
                                    | NonFormerFamily -> "non-former"
                            strand.AtomIds |> Seq.map (fun id -> id, label)))
                |> Seq.concat
                |> Map

    /// The given variant's world: experiment 24's bonds and bends, in a
    /// single chamber with no walls, seeded with the settings' number of
    /// strands of each kind at random places, and then free atoms
    /// anywhere. Its schedule is the cycle, with the variant's
    /// cap peeling in the stress phase. Nothing hydrolyzes.
    let getScenario settings variant =
        let former = getHairpinFormer settings
        assert(settings.NonFormer.Length = former.Length)
        let build parms rng =
            let world =
                (World.createSquare worldSize, [ 1 .. settings.NumSeeds ])
                    ||> Seq.fold (fun world _ ->
                        world
                            |> Build.addStrandAtRandom parms rng 74 former
                            |> Build.addStrandAtRandom parms rng 74 settings.NonFormer)
            let hairpinFormers =   // seeded in alternation, so every other strand
                let n = former.Length
                [|
                    for i in 0 .. 2 .. 2 * settings.NumSeeds - 1 do
                        yield! [| i * n .. i * n + n - 1 |]
                |]
            let world = addFood rng (getFoodSupply settings) world
            world, hairpinFormers
        let scenario =
            Scenario.createScenarioC false
                |> Scenario.withBend Strong Bend.rightAngle settings.BendStiffness
                |> Scenario.withBendStiffness Strong AtomType.H settings.HingeStiffness
                |> Scenario.withHydrolysisRate 0.0
                |> Scenario.mapBondTypeParameters Weak (fun parms ->
                    { parms with Energy = settings.PairEnergy })
                |> Scenario.mapParameters (fun parms ->
                    { parms with StackEnergy = settings.StackEnergy })
                |> (fun scenario ->
                    let moveAtom, toggleStrong, toggleWeak = settings.EventWeights
                    Scenario.withEventWeights moveAtom toggleStrong toggleWeak scenario)
        let scenario =
            match settings.HingePairEnergy with
                | Some energy ->
                    scenario
                        |> Scenario.withPairEnergy Weak AtomType.H AtomType.H energy
                | None -> scenario
        { scenario with
            Name = $"Folding in a famine, with a hinge, stress cap peel barrier {variant.StressCapPeelBarrier}"
            Build = build
            TryLabelStrands = Some (labelStrands settings)
            Schedule =
                Cycle [|
                    settings.Feast
                    { settings.Stress with CapPeelBarrier = Some variant.StressCapPeelBarrier }
                    settings.Melt
                |] }

    /// The experiment's scenarios: one for each variant.
    let getScenarios settings =
        settings.Variants |> Array.map (getScenario settings)

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

            /// Number of its caps whose bonds are not protected by a
            /// stack, so that they can peel under stress (0 to 2).
            NumExposedCaps : int
        }

    /// Census of the capped chains of a world.
    type private Census =
        {
            /// Chains counted in either family.
            Chains : ChainCensus[]

            /// Number of other capped chains: shorter ones, and those as
            /// near to one family as to the other.
            Others : int

            /// Free atoms of each type, averaged over the hairpin-former's
            /// most common types (W and X).
            Food : float
        }

    /// Takes a census of the capped chains in the given world.
    let private getCensus settings familyTexts (world : World) =
        let strong = world.BondTypeMap[Strong]
        let weak = world.BondTypeMap[Weak]
        let capped = getStrands world |> Array.where (isCapped world)
        let chains =
            capped
                |> Array.where (fun strand -> strand.AtomIds.Length >= settings.MinLength)
                |> Array.choose (fun strand ->
                    let ids = strand.AtomIds
                    tryGetFamily settings.MaxEditDistance familyTexts (getText world ids)
                        |> Option.map (fun family ->
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
                            let numExposedCaps =
                                let n = ids.Length
                                [ ids[0], ids[1]; ids[n - 1], ids[n - 2] ]
                                    |> List.where (fun (a, b) ->
                                        not (World.isProtected world.Atoms[a] world.Atoms[b] world))
                                    |> List.length
                            let types = ids |> Array.map (fun id -> world.Atoms[id].Type)
                            {
                                AtomIds = ids
                                Family = family
                                IsFoldable = getMaxStem types >= settings.MinStem
                                State = state
                                NumExposed = numExposed
                                NumExposedCaps = numExposedCaps
                            }))
        let food =
            let isFree (atom : Atom) = Set.isEmpty (BondMap.getBonds atom.Id strong)
            let count atomType =
                world.Atoms
                    |> Array.where (fun atom -> atom.Type = atomType && isFree atom)
                    |> Array.length
            float (count AtomType.W + count AtomType.X) / 2.0
        { Chains = chains; Others = capped.Length - chains.Length; Food = food }

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
        let familyTexts = getFamilyTexts settings
        let mutable run = Run.create seed (getScenario settings variant)
        let cycles =
            [|
                for iCycle in 1 .. settings.NumCycles do
                    run <- Run.advance settings.Feast.NumSweeps run
                    let fed = getCensus settings familyTexts run.World
                    run <- Run.advance settings.Stress.NumSweeps run
                    let stressed = getCensus settings familyTexts run.World
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

        let former = getHairpinFormer settings
        let toText = Array.map string >> String.concat " "
        let phaseText (phase : Phase) = sprintf "kT %g for %d sweeps" phase.Temperature phase.NumSweeps
        printfn "Experiment 26: folding in a famine, with a hinge"
        printfn "Hairpin-former: %s (longest stem %d)" (toText former) (getMaxStem former)
        printfn "Non-former:     %s (longest stem %d)" (toText settings.NonFormer) (getMaxStem settings.NonFormer)
        printfn "Bends: stiffness %g, hinges (H) %g" settings.BendStiffness settings.HingeStiffness
        printfn "%d runs of %d cycles of each variant; %d strands of each kind to start, at random places, in a %dx%d world"
            settings.NumRuns settings.NumCycles settings.NumSeeds worldSize worldSize
        printfn "Free atoms: %s; nothing is added or removed"
            (getFoodSupply settings |> Array.map (fun (t, n) -> $"{n} {t}") |> String.concat ", ")
        printfn "Capped chains of %d atoms or more are counted by family: that of the nearest sequence (edit distance %d or less) among each kind and its copy, read either way"
            settings.MinLength settings.MaxEditDistance
        printfn "Each cycle: feast %s; stress %s; melt %s"
            (phaseText settings.Feast) (phaseText settings.Stress) (phaseText settings.Melt)
        printfn "No hydrolysis. Under stress, caps not protected by a stack peel; in the melt, exposed ends peel"
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
        writer.WriteLine("stress_cap_peel_barrier,seed,cycle,food,fed_hairpins,fed_non_formers,fed_foldable_hairpins,hairpins_copied,hairpins_folded,hairpins_single,non_formers_copied,non_formers_folded,non_formers_single,stressed_hairpins,stressed_non_formers,others")
        let invariant (x : float) = x.ToString(Globalization.CultureInfo.InvariantCulture)
        let familyName = function HairpinFamily -> "hairpin" | NonFormerFamily -> "non-former"
        let stateName = function Copied -> "copied" | Folded -> "folded" | Single -> "single"
        let inState family state (census : Census) =
            census.Chains |> Array.where (fun ch -> ch.Family = family && ch.State = state) |> Array.length
        for variant in settings.Variants do
            let runs = [| for seed in 0 .. settings.NumRuns - 1 -> results[variant, seed] |]

                // per run and cycle, for the CSV
            for seed in 0 .. settings.NumRuns - 1 do
                for iCycle in 0 .. settings.NumCycles - 1 do
                    let c = runs[seed].Cycles[iCycle]
                    writer.WriteLine(
                        String.Join(",",
                            [ invariant variant.StressCapPeelBarrier; string seed; string (iCycle + 1)
                              invariant c.Fed.Food
                              string (count HairpinFamily c.Fed); string (count NonFormerFamily c.Fed)
                              string (c.Fed.Chains |> Array.where (fun ch -> ch.Family = HairpinFamily && ch.IsFoldable) |> Array.length)
                              for family in families do
                                for state in states do
                                    string (inState family state c.Fed)
                              string (count HairpinFamily c.Stressed); string (count NonFormerFamily c.Stressed)
                              string c.Stressed.Others ]))

            printfn ""
            printfn "Stress cap peel barrier %g" variant.StressCapPeelBarrier
            printfn "        |------------------------- after the feast -------------------------|   |-- after the stress --|"
            printfn "cycle    food   hairpin / non   copied folded single   copied folded single   hairpin / non   share   other"
            printfn "                                |-- hairpin family --|   |-- non-formers ---|"
            for iCycle in 0 .. settings.NumCycles - 1 do
                let cycle = runs |> Array.map (fun r -> r.Cycles[iCycle])
                let average f = cycle |> Array.averageBy (f >> float)
                let fh = average (fun c -> count HairpinFamily c.Fed)
                let fn = average (fun c -> count NonFormerFamily c.Fed)
                let sh = average (fun c -> count HairpinFamily c.Stressed)
                let sn = average (fun c -> count NonFormerFamily c.Stressed)
                let share = if sh + sn > 0.0 then sh / (sh + sn) else nan
                let st family state = average (fun c -> inState family state c.Fed)
                printfn "%5d   %5.1f   %6.2f / %5.2f    %5.2f  %5.2f  %5.2f    %5.2f  %5.2f  %5.2f    %6.2f / %5.2f   %5.2f  %6.1f"
                    (iCycle + 1) (cycle |> Array.averageBy _.Fed.Food) fh fn
                    (st HairpinFamily Copied) (st HairpinFamily Folded) (st HairpinFamily Single)
                    (st NonFormerFamily Copied) (st NonFormerFamily Folded) (st NonFormerFamily Single)
                    sh sn share (average _.Stressed.Others)

                // survival through the stress, by family and state, in
                // each half of the run (plenty, then want)
            let half = settings.NumCycles / 2
            for name, cycles in [ "first", [| 0 .. half - 1 |]; "second", [| half .. settings.NumCycles - 1 |] ] do
                printfn "Survival through the stress, %s half (cycles %d-%d; unprotected caps per chain at the end of the feast):"
                    name (cycles[0] + 1) (Array.last cycles + 1)
                for family in families do
                    let cells =
                        states |> Array.map (fun state ->
                            let pairs =
                                runs
                                    |> Array.collect (fun r ->
                                        cycles |> Array.collect (fun i ->
                                            let c = r.Cycles[i]
                                            Array.zip c.Fed.Chains c.Survived))
                                    |> Array.where (fun (ch, _) -> ch.Family = family && ch.State = state)
                            let nSurvived = pairs |> Array.where snd |> Array.length
                            if pairs.Length = 0 then sprintf "%s -" (stateName state)
                            else
                                sprintf "%s %d/%d = %.2f (%.2f exposed caps)"
                                    (stateName state) nSurvived pairs.Length
                                    (float nSurvived / float pairs.Length)
                                    (pairs |> Array.averageBy (fun (ch, _) -> float ch.NumExposedCaps)))
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
                (getSignTestP nAhead nBehind)
                (getSignTestP nBehind nAhead)

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
                    (getSignTestP nUp nDown)
                    (getSignTestP nDown nUp)

                // commonest sequences at the end
            printfn "Commonest capped chains at the end (all runs):"
            let familyTexts = getFamilyTexts settings
            let sequences = runs |> Array.collect _.FinalSequences
            for family in families do
                let common =
                    sequences
                        |> Array.where (fun text ->
                            text.Length >= settings.MinLength
                                && tryGetFamily settings.MaxEditDistance familyTexts text = Some family)
                        |> Array.countBy id
                        |> Array.sortByDescending snd
                        |> Array.truncate settings.NumCommonSequences
                printfn "   %s family:" (familyName family)
                for text, n in common do
                    printfn "      %4d  %2d atoms  %s" n text.Length text
            let others =
                sequences
                    |> Array.where (fun text ->
                        text.Length < settings.MinLength
                            || tryGetFamily settings.MaxEditDistance familyTexts text = None)
                    |> Array.countBy id
                    |> Array.sortByDescending snd
                    |> Array.truncate settings.NumCommonSequences
            printfn "   other:"
            for text, n in others do
                printfn "      %4d  %2d atoms  %s" n text.Length text

        printfn ""
        printfn "food: free W and X atoms (no strong bonds), per type, per run. hairpin, non: capped chains of"
        printfn "   each family, per run. copied: half or more of a chain's atoms paired with another chain;"
        printfn "   folded: not copied, but a run of at least %d atoms paired within it; single: neither."
            (2 * settings.MinFoldedStem)
        printfn "share: hairpins / (hairpins + non-formers) after the stress, from the averages."
        printfn "other: other capped chains (shorter, too far from both families, or as near to either), after the stress."
        printfn "Results written to %s" (Path.GetFullPath csvPath)
