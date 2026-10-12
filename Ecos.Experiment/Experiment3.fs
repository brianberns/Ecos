namespace Ecos.Experiment

open System
open System.IO
open System.Threading

open Ecos

/// Experiment 3: temperature cycling. A template strand sits in a box
/// of free atoms of all four types. The temperature alternates between
/// cold, so that free atoms pair with the template and join into a
/// copy, and hot, so that the copy lets go of the template. Do copies
/// form, do they separate from the template intact, and does the
/// number of strands grow from one cycle to the next?
///
/// The world, the bond parameters, and the temperature of each phase
/// come from the scenario, which the desktop app shares.
module Experiment3 =

    /// Settings for the experiment.
    type Settings =
        {
            /// Number of independent runs (seeds).
            NumRuns : int

            /// Number of times to run through the scenario's phases.
            NumCycles : int
        }

    /// Default settings.
    let defaultSettings =
        {
            NumRuns = 24
            NumCycles = 3
        }

    /// A strand: two or more atoms connected by strong bonds.
    type private Strand =
        {
            /// The strand's atoms. If the strand is a chain, they're
            /// in order from one end to the other.
            AtomIds : AtomId[]

            /// A ring has no ends.
            IsRing : bool
        }

    /// The atoms that the given atom is bonded to.
    let private getPartners bondType (world : World) id =
        world.BondTypeMap[bondType]
            |> Map.tryFind id
            |> Option.defaultValue Set.empty

    /// All strands in the given world. An atom has at most two strong
    /// bonds, so every strand is either a chain or a ring.
    let private getStrands (world : World) =

        /// Walks along a strand from the given atom, away from the
        /// previous one.
        let rec walk (acc : List<AtomId>) idPrev id =
            let next =
                getPartners Strong world id
                    |> Seq.where (fun idNext ->
                        idNext <> idPrev && idNext <> List.last acc)   // stop where a ring closes
                    |> Seq.tryHead
            match next with
                | Some idNext when not (List.contains idNext acc) ->
                    walk (idNext :: acc) id idNext
                | _ -> List.rev acc

        let bonded = world.BondTypeMap[Strong].Keys |> Seq.toArray
        let ends =
            bonded
                |> Array.where (fun id ->
                    (getPartners Strong world id).Count = 1)
        let chains =
            [|
                for id in ends do
                    let atomIds = walk [ id ] id id |> List.toArray
                    if atomIds[0] < atomIds[atomIds.Length - 1] then   // each chain is found from both ends
                        { AtomIds = atomIds; IsRing = false }
            |]
        let inChains =
            chains |> Seq.collect _.AtomIds |> set
        let rings =
            ((Set.empty, []), bonded)
                ||> Seq.fold (fun (seen, rings) id ->
                    if inChains.Contains id || seen.Contains id then
                        seen, rings
                    else
                        let atomIds = walk [ id ] id id |> List.toArray
                        Set.union seen (set atomIds),
                        { AtomIds = atomIds; IsRing = true } :: rings)
                |> snd
        Array.append chains (List.toArray rings)

    /// The type that pairs with the given type.
    let private complement = function
        | AtomType.W -> AtomType.X
        | AtomType.X -> AtomType.W
        | AtomType.Y -> AtomType.Z
        | AtomType.Z -> AtomType.Y
        | AtomType.P -> AtomType.Q
        | AtomType.Q -> AtomType.P

    /// Indicates whether the given strand is a chain with a cap at
    /// each end, so that it can't be extended.
    let private isCapped (world : World) strand =
        let isCap id =
            match world.AtomMap[id].Type with
                | AtomType.P | AtomType.Q -> true
                | _ -> false
        not strand.IsRing
            && isCap strand.AtomIds[0]
            && isCap strand.AtomIds[strand.AtomIds.Length - 1]

    /// Indicates whether the given strand is a chain whose sequence
    /// is a contiguous piece of the given sequence, read from either
    /// end.
    let private hasPieceOf (sequence : AtomType[]) (world : World) strand =
        if strand.IsRing then false
        else
            let types =
                strand.AtomIds
                    |> Array.map (fun id -> world.AtomMap[id].Type)
            let isPiece (types : _[]) =
                sequence
                    |> Seq.windowed types.Length
                    |> Seq.contains types
            isPiece types || isPiece (Array.rev types)

    /// Indicates whether the given strand is a chain with the given
    /// sequence of atom types, read from either end.
    let private hasSequence (sequence : AtomType[]) world strand =
        strand.AtomIds.Length = sequence.Length
            && hasPieceOf sequence world strand

    /// Counts the atoms of the given strand that are paired with an
    /// atom of a different strand.
    let private countPaired (strandMap : Map<AtomId, int>) (world : World) iStrand strand =
        strand.AtomIds
            |> Seq.where (fun id ->
                getPartners Weak world id
                    |> Seq.exists (fun idPartner ->
                        match Map.tryFind idPartner strandMap with
                            | Some iPartner -> iPartner <> iStrand
                            | None -> false))
            |> Seq.length

    /// What a world contains at the end of a phase.
    type private Census =
        {
            /// Number of strands.
            Strands : int

            /// Chains whose sequence is the complement of the
            /// template's.
            Copies : int

            /// Atoms in the longest chain whose sequence is a
            /// contiguous piece of the complement of the template's.
            BestCopy : int

            /// Chains whose sequence is the same as the template's,
            /// including the template itself.
            Same : int

            /// Atoms in the longest strand.
            Longest : int

            /// Chains with a cap at each end, including the template.
            Capped : int

            /// Fraction of the atoms in strands that are paired with
            /// an atom of a different strand.
            Paired : float

            /// Whether the template is still a strand by itself.
            TemplateIntact : bool

            /// Atom sets of all strands.
            StrandSets : Set<Set<AtomId>>

            /// Atom sets of the copies.
            CopySets : Set<Set<AtomId>>
        }

    /// Takes a census of the given world.
    let private getCensus (template : AtomId[]) (world : World) =
        let strands = getStrands world
        let strandMap =
            seq {
                for iStrand, strand in Array.indexed strands do
                    for id in strand.AtomIds do
                        id, iStrand
            } |> Map
        let sequence =
            template |> Array.map (fun id -> world.AtomMap[id].Type)
        let complementSequence = Array.map complement sequence
        let atomSet strand = set strand.AtomIds
        let copies =
            strands
                |> Array.where (hasSequence complementSequence world)
        let nPaired =
            strands
                |> Array.indexed
                |> Array.sumBy (fun (iStrand, strand) ->
                    countPaired strandMap world iStrand strand)
        let strandSets = strands |> Seq.map atomSet |> set
        {
            Strands = strands.Length
            Copies = copies.Length
            BestCopy =
                strands
                    |> Array.where (hasPieceOf complementSequence world)
                    |> Array.map _.AtomIds.Length
                    |> Array.fold max 0
            Same =
                strands
                    |> Array.where (hasSequence sequence world)
                    |> Array.length
            Capped =
                strands
                    |> Array.where (isCapped world)
                    |> Array.length
            Longest =
                strands
                    |> Array.map _.AtomIds.Length
                    |> Array.fold max 0
            Paired =
                if strandMap.Count = 0 then 0.0
                else float nPaired / float strandMap.Count
            TemplateIntact = strandSets.Contains (set template)
            StrandSets = strandSets
            CopySets = copies |> Seq.map atomSet |> set
        }

    /// A join: a strong bond that formed between two atoms.
    type private Join =
        {
            /// Number of atoms in the strand that each of the two atoms
            /// belonged to before they joined, smaller first. (A free
            /// atom counts as 1.)
            SmallerSize : int
            LargerSize : int

            /// Whether either strand contained an atom of the original
            /// template.
            InvolvesTemplate : bool

            /// Number of atoms in the strand that templated the join:
            /// one that had adjacent atoms paired with each of the
            /// two atoms joined. Zero if there was none. (If there
            /// were two, one on each side, this is the larger.)
            SplintSize : int
        }

    /// The atoms of the strand that the given atom belongs to. A free
    /// atom is a strand of one, for this purpose.
    let private getStrandOf (world : World) id =
        let rec visit (seen : Set<AtomId>) = function
            | [] -> seen
            | id :: rest when seen.Contains id -> visit seen rest
            | id :: rest ->
                let partners = getPartners Strong world id |> Set.toList
                visit (seen.Add id) (partners @ rest)
        visit Set.empty [ id ]

    /// Describes a join that is about to happen between the given
    /// atoms in the given world.
    let private getJoin (template : Set<AtomId>) (world : World) idA idB =
        let strandA = getStrandOf world idA
        let strandB = getStrandOf world idB
        let splintSizes =
            [
                for idA' in getPartners Weak world idA do
                    for idB' in getPartners Weak world idB do
                        if (getPartners Strong world idA').Contains idB' then
                            (getStrandOf world idA').Count
            ]
        {
            SmallerSize = min strandA.Count strandB.Count
            LargerSize = max strandA.Count strandB.Count
            InvolvesTemplate =
                not (Set.intersect template strandA).IsEmpty
                    || not (Set.intersect template strandB).IsEmpty
            SplintSize = List.fold max 0 splintSizes
        }

    /// Result of one phase of one run.
    type private PhaseResult =
        {
            /// Census at the start of the phase.
            Before : Census

            /// Census at the end of the phase.
            After : Census

            /// Strong bonds formed during the phase.
            Joins : Join[]

            /// Number of strong bonds broken during the phase.
            Breaks : int
        }

    /// Runs the given number of sweeps. (One sweep is one update per
    /// atom.) Answers the resulting world, the strong bonds formed,
    /// and the number broken.
    let private runPhase template parms rng nSweeps (world : World) =
        let nAtoms = world.AtomMap.Count
        let mutable world = world
        let joins = ResizeArray()
        let mutable nBreaks = 0
        for _ in 1 .. nSweeps * nAtoms do
            let world', eventOpt = Ecos.World.update parms rng world
            match eventOpt with
                | Some (CreateBond (Strong, idA, idB)) ->
                    joins.Add(getJoin template world idA idB)
                | Some (BreakBond (Strong, _, _)) -> nBreaks <- nBreaks + 1
                | _ -> ()
            world <- world'
        world, joins.ToArray(), nBreaks

    /// The phases of the given scenario.
    let private getPhases scenario =
        match scenario.TemperatureType with
            | Cycle phases -> phases
            | Adjustable _ -> failwith "Scenario has no temperature cycle"

    /// Runs one world through the cycles. Answers a result for each
    /// phase of each cycle, in order.
    let private runOne settings seed =
        let scenario = Scenario.createExperiment3 seed
        let rng = scenario.Random
        let template = set scenario.Template
        let mutable world = scenario.World
        let mutable census = getCensus scenario.Template world
        [|
            for _ in 1 .. settings.NumCycles do
                for phase in getPhases scenario do
                    let parms =
                        {
                            Temperature = phase.Temperature
                            BondParameterMap = scenario.BondParameterMap
                        }
                    let world', joins, nBreaks =
                        runPhase template parms rng phase.NumSweeps world
                    let census' = getCensus scenario.Template world'
                    {
                        Before = census
                        After = census'
                        Joins = joins
                        Breaks = nBreaks
                    }
                    world <- world'
                    census <- census'
        |]

    /// Runs the experiment, prints tables of results, and writes them
    /// to the given CSV file.
    let run (settings : Settings) (csvPath : string) =

        let scenario = Scenario.createExperiment3 0
        if scenario.Template.Length = 0 then
            failwith "Scenario has no template"
        let phases = getPhases scenario
        let nTemplate = scenario.Template.Length

            // run every seed in parallel
        let mutable nDone = 0
        let runs =
            [| 0 .. settings.NumRuns - 1 |]
                |> Array.Parallel.map (fun seed ->
                    let results = runOne settings seed
                    let n = Interlocked.Increment(&nDone)
                    eprintfn "Finished run %d of %d" n settings.NumRuns
                    results)

        /// Results of the given phase of the given cycle, one per run.
        let getResults iCycle iPhase =
            runs
                |> Array.map (fun results ->
                    results[iCycle * phases.Length + iPhase])

        printfn "Experiment 3: temperature cycling"
        printfn "%d runs of %d cycles" settings.NumRuns settings.NumCycles
        for bondType in BondType.all do
            let bondParms = scenario.BondParameterMap[bondType]
            printfn
                "%A bonds: energy %g, barrier %g, barrier reduction per stack %g"
                bondType
                bondParms.Energy
                bondParms.Barrier
                bondParms.BarrierStackReduction
        printfn ""
        printfn "Strands"
        printfn "                         |----------------- at the end of the phase -----------------|  |- during -|   |--- still intact ---|"
        printfn "cycle    kT   sweeps     strands  capped  copies   best   same  longest  paired        joins  breaks   strands  copies  template"

        use writer = new StreamWriter(csvPath)
        writer.WriteLine(
            "cycle,kT,sweeps,"
                + "strands,copies,best_copy,same,longest,paired,"
                + "joins,breaks,"
                + "strands_intact_fraction,copies_intact_fraction,template_intact_fraction,capped,"
                + "joins_atom_atom,joins_atom_strand,joins_strand_strand,joins_untemplated,"
                + "template_joins,template_joins_atom,template_joins_strand,template_joins_strand_size,"
                + "template_splint_none,template_splint_short,template_splint_medium,template_splint_full")

        /// Joins per run that satisfy the given test.
        let joinsPerRun (results : PhaseResult[]) test =
            results
                |> Array.averageBy (fun result ->
                    result.Joins
                        |> Array.where test
                        |> Array.length
                        |> float)

        /// Join statistics for the given results.
        let getJoinValues (results : PhaseResult[]) =
            let onTemplate (join : Join) = join.InvolvesTemplate
            let attached =
                results
                    |> Array.collect _.Joins
                    |> Array.where (fun join ->
                        onTemplate join && join.SmallerSize > 1)
            [|
                    // all joins, by what was joined
                joinsPerRun results (fun join -> join.LargerSize = 1)
                joinsPerRun results (fun join ->
                    join.SmallerSize = 1 && join.LargerSize > 1)
                joinsPerRun results (fun join -> join.SmallerSize > 1)
                joinsPerRun results (fun join -> join.SplintSize = 0)

                    // joins onto the template's strand, by what was joined
                joinsPerRun results onTemplate
                joinsPerRun results (fun join ->
                    onTemplate join && join.SmallerSize = 1)
                joinsPerRun results (fun join ->
                    onTemplate join && join.SmallerSize > 1)
                (if attached.Length = 0 then nan
                 else attached |> Array.averageBy (fun join -> float join.SmallerSize))

                    // joins onto the template's strand, by size of splint
                joinsPerRun results (fun join ->
                    onTemplate join && join.SplintSize = 0)
                joinsPerRun results (fun join ->
                    onTemplate join
                        && join.SplintSize > 0
                        && join.SplintSize < nTemplate / 2)
                joinsPerRun results (fun join ->
                    onTemplate join
                        && join.SplintSize >= nTemplate / 2
                        && join.SplintSize < nTemplate)
                joinsPerRun results (fun join ->
                    onTemplate join && join.SplintSize >= nTemplate)
            |]

        for iCycle in 0 .. settings.NumCycles - 1 do
            for iPhase in 0 .. phases.Length - 1 do
                let phase = phases[iPhase]
                let results = getResults iCycle iPhase
                let average (f : PhaseResult -> float) =
                    Array.averageBy f results

                    // of the strands at the start of the phase, the
                    // fraction that the phase left intact
                let fraction (getSets : Census -> Set<Set<AtomId>>) =
                    let nBefore =
                        results
                            |> Array.sumBy (fun result ->
                                (getSets result.Before).Count)
                    if nBefore = 0 then nan
                    else
                        let nAfter =
                            results
                                |> Array.sumBy (fun result ->
                                    Set.intersect
                                        (getSets result.Before)
                                        result.After.StrandSets
                                        |> Set.count)
                        float nAfter / float nBefore

                let v =
                    [|
                        float (iCycle + 1)
                        phase.Temperature
                        float phase.NumSweeps
                        average (fun r -> float r.After.Strands)
                        average (fun r -> float r.After.Copies)
                        average (fun r -> float r.After.BestCopy)
                        average (fun r -> float r.After.Same)
                        average (fun r -> float r.After.Longest)
                        average (fun r -> r.After.Paired)
                        average (fun r -> float r.Joins.Length)
                        average (fun r -> float r.Breaks)
                        fraction _.StrandSets
                        fraction _.CopySets
                        average (fun r ->
                            if r.After.TemplateIntact then 1.0 else 0.0)
                        average (fun r -> float r.After.Capped)
                    |]
                printfn
                    "%5d  %4.2f  %7d     %7.2f  %6.2f  %6.2f  %5.2f  %5.2f  %7.2f  %6.2f       %6.2f  %6.2f   %7.2f  %6.2f  %8.2f"
                    (iCycle + 1) v[1] phase.NumSweeps
                    v[3] v[14] v[4] v[5] v[6] v[7] v[8]
                    v[9] v[10]
                    v[11] v[12] v[13]
                writer.WriteLine(
                    String.Join(
                        ",",
                        Array.append v (getJoinValues results)
                            |> Seq.map (fun (x : float) ->
                                x.ToString(Globalization.CultureInfo.InvariantCulture))))

        printfn ""
        printfn "All columns are averages per run, except paired and the last three, which are fractions."
        printfn "strands: two or more atoms connected by strong bonds, including the template."
        printfn "capped: chains with a cap at each end, including the template."
        printfn "copies: chains whose sequence is the complement of the template's, caps included."
        printfn "best: atoms in the longest chain whose sequence is a piece of that complement (%d is a copy)." nTemplate
        printfn "same: chains with the template's own sequence, including the template."
        printfn "longest: atoms in the longest strand (the template has %d)." nTemplate
        printfn "paired: fraction of the atoms in strands that are paired with an atom of another strand."
        printfn "joins, breaks: strong bonds formed and broken during the phase."
        printfn "still intact: of the strands (and of the copies) at the start of the phase, the fraction"
        printfn "   unchanged at the end of it; and the fraction of runs in which the template is still a"
        printfn "   strand by itself at the end of it. NaN means there were none."

        printfn ""
        printfn "Joins during each phase (averages per run)"
        printfn "                         |---------- all joins ----------|   |------------- joins onto the template's strand -------------|"
        printfn "                           atom    atom  strand                         what was joined          size of the splint"
        printfn "cycle    kT   sweeps      +atom  +strand +strand  no splint    total   atom  strand  (atoms)    none  short  medium   full"
        for iCycle in 0 .. settings.NumCycles - 1 do
            for iPhase in 0 .. phases.Length - 1 do
                let phase = phases[iPhase]
                let j = getJoinValues (getResults iCycle iPhase)
                printfn
                    "%5d  %4.2f  %7d     %6.2f  %6.2f  %6.2f     %6.2f   %6.2f %6.2f  %6.2f   %6.1f  %6.2f %6.2f  %6.2f %6.2f"
                    (iCycle + 1) phase.Temperature phase.NumSweeps
                    j[0] j[1] j[2] j[3]
                    j[4] j[5] j[6] j[7]
                    j[8] j[9] j[10] j[11]
        printfn ""
        printfn "atom+atom: two free atoms joined, starting a strand."
        printfn "atom+strand: a free atom joined the end of a strand."
        printfn "strand+strand: two strands joined end to end."
        printfn "splint: the strand that templated a join. It has adjacent atoms paired with each of"
        printfn "   the two atoms joined. Short is under %d atoms, medium is %d to %d, full is %d or more."
            (nTemplate / 2) (nTemplate / 2) (nTemplate - 1) nTemplate
        printfn "template's strand: the strand containing the atoms of the original template."
        printfn "   'strand (atoms)' is the average size of the strands attached to it."
        printfn "Results written to %s" (Path.GetFullPath csvPath)
