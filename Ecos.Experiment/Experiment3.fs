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

            /// Seed of the first run. The others follow in order.
            FirstSeed : int

            /// Number of times to run through the scenario's phases.
            NumCycles : int

            /// Scenario to run. It must have a template and a
            /// temperature cycle.
            Scenario : Scenario

            /// Overrides the scenario's rate of hydrolysis of strong
            /// bonds, if given. Zero means strands never die.
            HydrolysisRate : float option
        }

    /// Default settings: scenario B. Nothing hydrolyzes, so that the
    /// template survives, since this experiment measures what happens
    /// to it.
    let defaultSettings =
        {
            NumRuns = 24
            FirstSeed = 0
            NumCycles = 3
            Scenario = Scenario.createScenarioB true
            HydrolysisRate = Some 0.0
        }

    /// Settings for scenario C, which was designed for copying.
    let copyingSettings =
        {
            defaultSettings with
                Scenario = Scenario.createScenarioC true
                HydrolysisRate = None
        }

    /// The settings' scenario, with their hydrolysis rate instead of
    /// its own, if any.
    let getScenario settings =
        let scenario = settings.Scenario
        match settings.HydrolysisRate with
            | Some rate ->
                scenario
                    |> Scenario.withHydrolysisRate rate
                    |> Scenario.withName $"{scenario.Name}, hydrolysis rate {rate}"
            | None -> scenario

    /// The experiment's scenarios: just one.
    let getScenarios settings =
        [| getScenario settings |]

    /// Prints the given parameters, apart from temperature.
    let printParameters (parms : Parameters) =
        printfn
            "Peel barrier %g"
            parms.PeelBarrier
        for bondType in BondType.all do
            let bondTypeParms = parms.BondTypeParameterMap[bondType]
            printfn
                "%A bonds: energy %g, entropy %g, barrier %g, barrier reduction per stack %g, start barrier %g, hydrolysis rate %g"
                bondType
                bondTypeParms.Energy
                bondTypeParms.Entropy
                bondTypeParms.Barrier
                bondTypeParms.BarrierStackReduction
                bondTypeParms.StartBarrier
                bondTypeParms.HydrolysisRate

    /// A strand: two or more atoms connected by strong bonds.
    type Strand =
        {
            /// The strand's atoms. If the strand is a chain, they're
            /// in order from one end to the other.
            AtomIds : AtomId[]

            /// A ring has no ends.
            IsRing : bool
        }

    /// The atoms that the given atom is bonded to.
    let getPartners bondType (world : World) id =
        world.BondTypeMap[bondType]
            |> BondMap.getBonds id

    /// All strands in the given world. An atom has at most two strong
    /// bonds, so every strand is either a chain or a ring.
    let getStrands (world : World) =

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
    let complement = function
        | AtomType.W -> AtomType.X
        | AtomType.X -> AtomType.W
        | AtomType.Y -> AtomType.Z
        | AtomType.Z -> AtomType.Y
        | AtomType.P -> AtomType.Q
        | AtomType.Q -> AtomType.P

    /// Indicates whether the given strand is a chain with a cap at
    /// each end, so that it can't be extended.
    let isCapped (world : World) strand =
        let isCap id =
            match world.Atoms[id].Type with
                | AtomType.P | AtomType.Q -> true
                | _ -> false
        not strand.IsRing
            && isCap strand.AtomIds[0]
            && isCap strand.AtomIds[strand.AtomIds.Length - 1]

    /// Indicates whether the given strand is a chain whose sequence
    /// is a contiguous piece of the given sequence, read from either
    /// end.
    /// A key shared by all chains that could be copies of each other,
    /// directly or through further copies: those with the given
    /// sequence, or its complement, read from either end.
    let getFamily (sequence : AtomType[]) =
        let complemented = Array.map complement sequence
        List.min [
            List.ofArray sequence
            List.ofArray (Array.rev sequence)
            List.ofArray complemented
            List.ofArray (Array.rev complemented)
        ]

    let private hasPieceOf (sequence : AtomType[]) (world : World) strand =
        if strand.IsRing then false
        else
            let types =
                strand.AtomIds
                    |> Array.map (fun id -> world.Atoms[id].Type)
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

    /// The given chain's atom IDs, in order from its head (the atom
    /// with no predecessor) to its tail.
    let private fromHead (world : World) (atomIds : AtomId[]) =
        let successor id =
            Map.tryFind id world.BondTypeMap.StrongBondDirectionMap
        if atomIds.Length >= 2 && successor atomIds[0] <> Some atomIds[1] then
            Array.rev atomIds
        else atomIds

    /// The given chain's sequence of atom types, read from its head.
    let private readFromHead (world : World) atomIds =
        fromHead world atomIds
            |> Array.map (fun id -> world.Atoms[id].Type)

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

            /// Copies and chains with the template's sequence that
            /// run the way copying predicts: each copy is antiparallel
            /// to its template, so a copy read from its head is the
            /// template read from its head, reversed and complemented,
            /// and a copy of a copy reads as the template does.
            Oriented : int

            /// Atoms in the longest strand.
            Longest : int

            /// Chains with a cap at each end, including the template.
            Capped : int

            /// Number of atoms of each type with no strong bond.
            FreeAtoms : Map<AtomType, int>

            /// Family of each capped chain. (See getFamily.)
            CappedFamilies : List<AtomType>[]

            /// Fraction of the atoms in strands that are paired with
            /// an atom of a different strand.
            Paired : float

            /// Whether the template is still a strand by itself.
            TemplateIntact : bool

            /// Family of the template. (Each run has its own template.)
            TemplateFamily : List<AtomType>

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
            template |> Array.map (fun id -> world.Atoms[id].Type)
        let complementSequence = Array.map complement sequence
        let atomSet strand = set strand.AtomIds
        let copies =
            strands
                |> Array.where (hasSequence complementSequence world)
        let same =
            strands
                |> Array.where (hasSequence sequence world)
        let oriented =
            let templateFromHead = readFromHead world template
            let copyFromHead = templateFromHead |> Array.rev |> Array.map complement
            let count (chains : Strand[]) expected =
                chains
                    |> Array.where (fun strand -> readFromHead world strand.AtomIds = expected)
                    |> Array.length
            count copies copyFromHead + count same templateFromHead
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
            Same = same.Length
            Oriented = oriented
            FreeAtoms =
                world.Atoms
                    |> Seq.where (fun atom -> not (strandMap.ContainsKey atom.Id))
                    |> Seq.countBy _.Type
                    |> Map
            CappedFamilies =
                strands
                    |> Array.where (isCapped world)
                    |> Array.map (fun strand ->
                        strand.AtomIds
                            |> Array.map (fun id -> world.Atoms[id].Type)
                            |> getFamily)
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
            TemplateFamily = getFamily sequence
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

    /// The atoms that the given atom is bonded to, in the given bonds.
    let private partnersIn (bonds : BondTypeMap) bondType id =
        bonds[bondType]
            |> BondMap.getBonds id

    /// The atoms of the strand that the given atom belongs to. A free
    /// atom is a strand of one, for this purpose.
    let private getStrandOf (bonds : BondTypeMap) id =
        let rec visit (seen : Set<AtomId>) = function
            | [] -> seen
            | id :: rest when seen.Contains id -> visit seen rest
            | id :: rest ->
                let partners = partnersIn bonds Strong id |> Set.toList
                visit (seen.Add id) (partners @ rest)
        visit Set.empty [ id ]

    /// Describes a join that is about to happen between the given
    /// atoms, given the bonds just before it.
    let private getJoin (template : Set<AtomId>) bonds idA idB =
        let strandA = getStrandOf bonds idA
        let strandB = getStrandOf bonds idB
        let splintSizes =
            [
                for idA' in partnersIn bonds Weak idA do
                    for idB' in partnersIn bonds Weak idB do
                        if (partnersIn bonds Strong idA').Contains idB' then
                            (getStrandOf bonds idA').Count
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
    /// atom.) Answers the resulting run, the strong bonds formed,
    /// and the number broken.
    let private runPhase template nSweeps run =
        let joins = ResizeArray()
        let mutable nBreaks = 0
        let onEvent event (bonds : BondTypeMap) =
            match event with
                | CreateBond (Strong, idA, idB) ->

                        // these are the bonds just after the join, so
                        // take it out again to see the strands as they
                        // were just before
                    let before =
                        { bonds with
                            StrongBondMap =
                                bonds[Strong]
                                    |> BondMap.removeBond idA idB
                            StrongBondDirectionMap =
                                Map.remove idA bonds.StrongBondDirectionMap }   // a new strong bond runs from A to B
                    joins.Add(getJoin template before idA idB)
                | BreakBond (Strong, _, _) -> nBreaks <- nBreaks + 1
                | _ -> ()
        let run = Run.advanceWith onEvent nSweeps run
        run, joins.ToArray(), nBreaks

    /// The phases of the given scenario.
    let getPhases (scenario : Scenario) =
        Schedule.getPhases scenario.Schedule

    /// Runs one world through the cycles. Answers a result for each
    /// phase of each cycle, in order.
    let private runOne settings seed =
        let scenario = getScenario settings
        let mutable run = Run.create seed scenario
        let template = set run.Template
        let mutable census = getCensus run.Template run.World
        [|
            for iCycle in 1 .. settings.NumCycles do
                for phase in getPhases scenario do
                    let run', joins, nBreaks =
                        runPhase template phase.NumSweeps run
                    let census' = getCensus run.Template run'.World
                    {
                        Before = census
                        After = census'
                        Joins = joins
                        Breaks = nBreaks
                    }
                    run <- run'
                    census <- census'
                eprintfn "Seed %d: finished cycle %d of %d" seed iCycle settings.NumCycles
        |]

    /// Runs the experiment, prints tables of results, and writes them
    /// to the given CSV file.
    let run (settings : Settings) (csvPath : string) =

        let scenario = getScenario settings
        let example = Run.create 0 scenario
        if example.Template.Length = 0 then
            failwith "Scenario has no template"
        let phases = getPhases scenario
        let nTemplate = example.Template.Length

            // run every seed in parallel
        let mutable nDone = 0
        let runs =
            [| settings.FirstSeed .. settings.FirstSeed + settings.NumRuns - 1 |]
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
        printfn "Scenario %s: %d runs of %d cycles (seeds %d to %d); %d atoms" scenario.Name settings.NumRuns settings.NumCycles settings.FirstSeed (settings.FirstSeed + settings.NumRuns - 1) example.World.Atoms.Length
        for phase in phases do
            printfn "Phase: kT %g for %d sweeps" phase.Temperature phase.NumSweeps
        printParameters scenario.Parameters
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
                + "template_splint_none,template_splint_short,template_splint_medium,template_splint_full,"
                + "oriented_fraction")

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

                    // fraction of copies and same-sequence chains that run
                    // the way copying predicts
                let orientedFraction =
                    let nOriented = results |> Array.sumBy (fun r -> r.After.Oriented)
                    let nChains = results |> Array.sumBy (fun r -> r.After.Copies + r.After.Same)
                    if nChains = 0 then nan else float nOriented / float nChains
                printfn
                    "%5d  %4.2f  %7d     %7.2f  %6.2f  %6.2f  %5.2f  %5.2f  %7.2f  %6.2f       %6.2f  %6.2f   %7.2f  %6.2f  %8.2f"
                    (iCycle + 1) v[1] phase.NumSweeps
                    v[3] v[14] v[4] v[5] v[6] v[7] v[8]
                    v[9] v[10]
                    v[11] v[12] v[13]
                writer.WriteLine(
                    String.Join(
                        ",",
                        Array.concat [ v; getJoinValues results; [| orientedFraction |] ]
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

        printfn ""
        printfn "Capped chains at the end of each phase (averages per run)"
        printfn "                         |---------------- number, by atoms in the chain ----------------|   |--- families other than the template's ---|"
        printfn "cycle    kT   sweeps      total      3    4-5    6-8  9-%-3d   %4d  over %-3d   average     families  with copies  largest  its atoms"
            (nTemplate - 1) nTemplate nTemplate
        for iCycle in 0 .. settings.NumCycles - 1 do
            for iPhase in 0 .. phases.Length - 1 do
                let phase = phases[iPhase]
                let results = getResults iCycle iPhase

                /// Capped chains per run whose length satisfies the
                /// given test.
                let perRun test =
                    results
                        |> Array.averageBy (fun result ->
                            result.After.CappedFamilies
                                |> Array.where (fun family -> test family.Length)
                                |> Array.length
                                |> float)
                let lengths =
                    results
                        |> Array.collect (fun result ->
                            result.After.CappedFamilies
                                |> Array.map (fun family -> float family.Length))

                    // families other than the template's, in each run:
                    // family and number of chains, largest first
                let otherFamilies =
                    results
                        |> Array.map (fun result ->
                            result.After.CappedFamilies
                                |> Array.where ((<>) result.After.TemplateFamily)
                                |> Array.countBy id
                                |> Array.sortByDescending snd)
                let averageOfLargest (f : List<AtomType> * int -> float) =
                    let largest =
                        otherFamilies
                            |> Array.choose Array.tryHead
                    if largest.Length = 0 then nan
                    else Array.averageBy f largest

                printfn
                    "%5d  %4.2f  %7d     %6.2f  %5.2f  %5.2f  %5.2f  %5.2f  %5.2f    %5.2f    %6.2f       %6.2f       %6.2f   %6.2f     %6.2f"
                    (iCycle + 1) phase.Temperature phase.NumSweeps
                    (perRun (fun _ -> true))
                    (perRun (fun n -> n = 3))
                    (perRun (fun n -> n >= 4 && n <= 5))
                    (perRun (fun n -> n >= 6 && n <= 8))
                    (perRun (fun n -> n >= 9 && n < nTemplate))
                    (perRun (fun n -> n = nTemplate))
                    (perRun (fun n -> n > nTemplate))
                    (if lengths.Length = 0 then nan else Array.average lengths)
                    (otherFamilies |> Array.averageBy (Array.length >> float))
                    (otherFamilies
                        |> Array.averageBy (fun families ->
                            families
                                |> Array.where (fun (_, count) -> count > 1)
                                |> Array.length
                                |> float))
                    (averageOfLargest (snd >> float))
                    (averageOfLargest (fun (family, _) -> float family.Length))
        printfn ""
        printfn "family: chains that could be copies of each other, directly or through further copies."
        printfn "   They have the same sequence, or its complement, read from either end."
        printfn "families: number of families, not counting the template's. 'with copies' is how many of"
        printfn "   them have more than one chain."
        printfn "largest: number of chains in the largest of those families, and the atoms in each chain"
        printfn "   (averaged over the runs that have one)."

            // free atoms, by type
        let atomTypes =
            [| AtomType.W; AtomType.X; AtomType.Y; AtomType.Z; AtomType.P; AtomType.Q |]
        printfn ""
        printfn "Free atoms at the end of each phase (averages per run, of %d atoms in all)"
            example.World.Atoms.Length
        printfn "cycle    kT   sweeps          W       X       Y       Z       P       Q     total"
        for iCycle in 0 .. settings.NumCycles - 1 do
            for iPhase in 0 .. phases.Length - 1 do
                let phase = phases[iPhase]
                let results = getResults iCycle iPhase
                let free atomType =
                    results
                        |> Array.averageBy (fun result ->
                            result.After.FreeAtoms
                                |> Map.tryFind atomType
                                |> Option.defaultValue 0
                                |> float)
                let counts = Array.map free atomTypes
                printfn
                    "%5d  %4.2f  %7d     %6.1f  %6.1f  %6.1f  %6.1f  %6.1f  %6.1f    %6.1f"
                    (iCycle + 1) phase.Temperature phase.NumSweeps
                    counts[0] counts[1] counts[2] counts[3] counts[4] counts[5]
                    (Array.sum counts)
        printfn ""
        printfn "free: not strongly bonded to anything."

            // spread between runs at the end, for comparing settings
        let final = getResults (settings.NumCycles - 1) (phases.Length - 1)
        let summarize name (f : PhaseResult -> float) =
            let values = Array.map f final
            let mean = Array.average values
            let sd =
                sqrt ((values |> Array.sumBy (fun v -> (v - mean) ** 2.0)) / float (values.Length - 1))
            printfn "%-24s %6.2f +/- %.2f (sd %.2f)" name mean (sd / sqrt (float values.Length)) sd
        printfn ""
        printfn "At the end, per run: mean +/- standard error"
        summarize "full-length capped" (fun r ->
            r.After.CappedFamilies
                |> Array.where (fun family -> family.Length = nTemplate)
                |> Array.length
                |> float)
        summarize "copies" (fun r -> float r.After.Copies)
        summarize "capped" (fun r -> float r.After.Capped)
        let nOriented = final |> Array.sumBy (fun r -> r.After.Oriented)
        let nChains = final |> Array.sumBy (fun r -> r.After.Copies + r.After.Same)
        printfn "%-24s %d of %d copies and same-sequence chains (should be all)" "oriented as predicted" nOriented nChains
        printfn "Results written to %s" (Path.GetFullPath csvPath)
