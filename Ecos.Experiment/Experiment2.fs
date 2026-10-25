namespace Ecos.Experiment

open System
open System.IO

open Ecos

/// Experiment 2: templated joining. Free atoms sit in a box, with or
/// without a template strand that they can pair with. How much faster
/// do the free atoms join to each other (form strong bonds) when the
/// template is present?
module Experiment2 =

    /// Settings for the experiment.
    type Settings =
        {
            /// Temperatures to run at.
            Temperatures : float[]

            /// Strong bond barriers to run with.
            StrongBondBarriers : float[]

            /// Strong bond barrier reductions (per stack) to run with.
            /// A reduction larger than the barrier just removes it.
            StrongBondBarrierStackReductions : float[]

            /// Bond-type parameters to run with. The strong bond barrier
            /// and its reduction are replaced by each combination of
            /// the values above in turn.
            BondTypeParameterMap : BondTypeParameterMap

            /// Number of independent runs (seeds) of each world, at
            /// each combination of the values above.
            NumRuns : int

            /// Sweeps to run before measuring, so that the free
            /// atoms settle on the template. (One sweep is one update
            /// per atom.) Strong bonds can't form or break during
            /// these sweeps.
            EquilibrationSweeps : int

            /// A run stops as soon as this many pairs of free atoms
            /// have joined. This measures the rate of joining before
            /// the free atoms start to run out.
            MaxJoins : int

            /// A run stops after this many sweeps of measurement, if
            /// it hasn't already.
            MaxSweeps : int
        }

    /// Default settings.
    let defaultSettings =
        {
            Temperatures = [| 0.2; 0.25; 0.3 |]
            StrongBondBarriers = [| 2.0; 3.0 |]
            StrongBondBarrierStackReductions = [| 0.0; 1.0; 1.5; 2.0; 3.0 |]
            BondTypeParameterMap =
                Map [
                    Strong,
                        {
                            Energy = 17.0
                            PairEnergyMap = Map.empty
                            Barrier = 0.0                 // replaced
                            BarrierStackReduction = 0.0   // replaced
                            StartBarrier = 0
                            Entropy = 0
                            HydrolysisRate = 0
                            PairedHydrolysisRate = 0
                            BendLimit = Bend.straight
                            BendStiffness = 0
                        }
                    Weak,
                        {
                            Energy = 1.0
                            PairEnergyMap = Map.empty
                            Barrier = 0.0
                            BarrierStackReduction = 0.0
                            StartBarrier = 0
                            Entropy = 0
                            HydrolysisRate = 0
                            PairedHydrolysisRate = 0
                            BendLimit = Bend.any
                            BendStiffness = 0
                        }
                ] |> BondTypeParameterMap.ofMap
            NumRuns = 16
            EquilibrationSweeps = 10000
            MaxJoins = 5
            MaxSweeps = 40000
        }

    /// Result of a single run.
    type private RunResult =
        {
            /// Fraction of the template that was paired when
            /// measurement began. (Zero if there's no template.)
            Occupancy : float

            /// Pairs of adjacent template atoms that were both
            /// paired when measurement began.
            AdjacentPairs : int

            /// Those of the pairs above whose partners were on the
            /// same side of the template, and so could join.
            SameSidePairs : int

            /// Sweeps of measurement.
            Sweeps : int

            /// Strong bonds formed between two free atoms.
            Joins : int

            /// Joins in which the two free atoms were paired with
            /// adjacent template atoms.
            TemplatedJoins : int

            /// Strong bonds formed between a free atom and a template
            /// atom.
            TemplateEndJoins : int

            /// Number of atoms in the longest chain of free atoms
            /// when the run stopped. (One if none had joined.)
            LongestChain : int
        }

    /// The given atom's weak bond partners (it can have two) that are
    /// among the given atoms.
    let private getPartners (candidates : Set<AtomId>) (bonds : BondTypeMap) id =
        bonds[Weak]
            |> Map.tryFind id
            |> Option.defaultValue Set.empty
            |> Set.intersect candidates

    /// Indicates whether the given atoms are strongly bonded.
    let private areStronglyBonded (bonds : BondTypeMap) idA idB =
        bonds[Strong]
            |> Map.tryFind idA
            |> Option.exists (Set.contains idB)

    /// Indicates whether the given atoms are weakly bonded to template
    /// atoms that are strongly bonded to each other.
    let private isTemplated template bonds idA idB =
        Seq.allPairs
            (getPartners template bonds idA)
            (getPartners template bonds idB)
            |> Seq.exists (fun (idA', idB') ->
                areStronglyBonded bonds idA' idB')

    /// Counts pairs of adjacent template atoms that are both paired
    /// with free atoms, and how many of those have partners on the
    /// same side of the line through the two template atoms.
    let private countAdjacentPairs (template : AtomId[]) (free : Set<AtomId>) (world : World) =
        let loc id = world.Atoms[id].Location
        let side (a : Point) (b : Point) (c : Point) =
            sign ((b.X - a.X) * (c.Y - a.Y) - (b.Y - a.Y) * (c.X - a.X))
        let pairs =
            [
                for idA in template do
                    for idB in template do
                        if idA < idB && areStronglyBonded world.BondTypeMap idA idB then
                            let idsA' = getPartners free world.BondTypeMap idA
                            let idsB' = getPartners free world.BondTypeMap idB
                            if not idsA'.IsEmpty && not idsB'.IsEmpty then
                                Seq.allPairs idsA' idsB'
                                    |> Seq.exists (fun (idA', idB') ->
                                        let sideA = side (loc idA) (loc idB) (loc idA')
                                        let sideB = side (loc idA) (loc idB) (loc idB')
                                        sideA <> 0 && sideA = sideB)
            ]
        pairs.Length, (pairs |> List.where id |> List.length)

    /// Number of atoms in the largest group of the given atoms that
    /// are connected by strong bonds.
    let private getLongestChain (atoms : Set<AtomId>) (world : World) =
        let strongBondMap = world.BondTypeMap[Strong]
        let rec visit (seen : Set<AtomId>) = function
            | [] -> seen
            | id :: rest when seen.Contains id -> visit seen rest
            | id :: rest ->
                let partners =
                    strongBondMap
                        |> Map.tryFind id
                        |> Option.defaultValue Set.empty
                        |> Seq.where atoms.Contains
                        |> Seq.toList
                visit (seen.Add id) (partners @ rest)
        let _, longest =
            ((Set.empty, 0), atoms)
                ||> Seq.fold (fun (seen, longest) id ->
                    if seen.Contains id then seen, longest
                    else
                        let chain = visit Set.empty [ id ]
                        Set.union seen chain, max longest chain.Count)
        longest

    /// Scenario A's world, with or without a template, with the
    /// settings' bond parameters, the given strong bond barrier and
    /// reduction per stack, no peeling, and the given fixed temperature.
    let getScenario settings barrier reduction kT withTemplate =
        let name =
            sprintf "barrier %g, reduction %g, kT %g%s"
                barrier reduction kT (if withTemplate then "" else ", no template")
        { Scenario.createScenarioA withTemplate with
            Schedule = Constant kT }
            |> Scenario.withName name
            |> Scenario.mapParameters (fun parms ->
                { parms with
                    BondTypeParameterMap = settings.BondTypeParameterMap
                    PeelBarrier = Double.PositiveInfinity })   // no peeling
            |> Scenario.mapBondTypeParameters Strong (fun bondTypeParms ->
                { bondTypeParms with
                    Barrier = barrier
                    BarrierStackReduction = reduction })

    /// The experiment's scenarios: one for each combination of settings.
    /// (The desktop app runs them without the experiment's settling
    /// period.)
    let getScenarios settings =
        [|
            for barrier in settings.StrongBondBarriers do
                for reduction in settings.StrongBondBarrierStackReductions do
                    for kT in settings.Temperatures do
                        for withTemplate in [ false; true ] do
                            getScenario settings barrier reduction kT withTemplate
        |]

    /// Runs one world of the given scenario.
    let private runOne settings (scenario : Scenario) seed =
        let run = Run.create seed scenario
        let template = set run.Template
        let free = Set.difference (run.World.Atoms |> Seq.map _.Id |> set) template
        let withTemplate = not template.IsEmpty

            // settle, with strong bonds frozen by an impassable barrier
        let frozen =
            scenario
                |> Scenario.mapBondTypeParameters Strong (fun bondTypeParms ->
                    { bondTypeParms with
                        Barrier = Double.PositiveInfinity
                        BarrierStackReduction = 0.0 })
        let run =
            { run with Scenario = frozen }
                |> Run.advance settings.EquilibrationSweeps
        let mutable run = { run with Scenario = scenario }
        let occupancy =
            if withTemplate then
                Experiment1.getOccupancy run.Template run.World
            else 0.0
        let nAdjacent, nSameSide =
            countAdjacentPairs run.Template free run.World

            // measure
        let mutable nSweeps = 0
        let mutable nJoins = 0
        let mutable nTemplated = 0
        let mutable nTemplateEnd = 0
        let onEvent event bonds =
            match event with
                | CreateBond (Strong, idA, idB) ->
                    match template.Contains idA, template.Contains idB with
                        | false, false ->
                            nJoins <- nJoins + 1
                            if isTemplated template bonds idA idB then   // a join doesn't change weak bonds
                                nTemplated <- nTemplated + 1
                        | true, true -> ()   // can't happen: template is a chain
                        | _ -> nTemplateEnd <- nTemplateEnd + 1
                | _ -> ()
        while nSweeps < settings.MaxSweeps && nJoins < settings.MaxJoins do
            run <- Run.advanceWith onEvent 1 run
            nSweeps <- nSweeps + 1

        {
            Occupancy = occupancy
            AdjacentPairs = nAdjacent
            SameSidePairs = nSameSide
            Sweeps = nSweeps
            Joins = nJoins
            TemplatedJoins = nTemplated
            TemplateEndJoins = nTemplateEnd
            LongestChain = getLongestChain free run.World
        }

    /// Joins per 10,000 sweeps, pooled over the given runs, and the
    /// number of joins. Joins are rare, independent events, so the
    /// relative error of the rate is one over the square root of the
    /// number of joins.
    let private getRate (runs : RunResult[]) =
        let nJoins = runs |> Array.sumBy _.Joins
        let nSweeps = runs |> Array.sumBy _.Sweeps
        10000.0 * float nJoins / float nSweeps, nJoins, nSweeps

    /// Runs the experiment, prints a table of results, and writes them
    /// to the given CSV file.
    let run (settings : Settings) (csvPath : string) =

            // run every combination in parallel
        let results =
            [|
                for barrier in settings.StrongBondBarriers do
                    for reduction in settings.StrongBondBarrierStackReductions do
                        for kT in settings.Temperatures do
                            for withTemplate in [ false; true ] do
                                for seed in 0 .. settings.NumRuns - 1 do
                                    barrier, reduction, kT, withTemplate, seed
            |]
                |> Array.Parallel.map (fun (barrier, reduction, kT, withTemplate, seed) ->
                    let scenario = getScenario settings barrier reduction kT withTemplate
                    (barrier, reduction, kT),
                    (withTemplate, runOne settings scenario seed))
                |> Array.groupBy fst
                |> Array.map (fun (key, group) ->
                    let runs = Array.map snd group
                    let select flag =
                        runs
                            |> Array.where (fun (withTemplate, _) -> withTemplate = flag)
                            |> Array.map snd
                    key, select false, select true)
                |> Array.sortBy (fun (key, _, _) -> key)

        printfn "Experiment 2: joining of free atoms, with and without a template"
        printfn
            "%d runs of each world; %d sweeps to settle; each run stops after %d joins or %d sweeps"
            settings.NumRuns
            settings.EquilibrationSweeps
            settings.MaxJoins
            settings.MaxSweeps
        printfn
            "Strong bond energy %g; weak bond energy %g"
            settings.BondTypeParameterMap[Strong].Energy
            settings.BondTypeParameterMap[Weak].Energy
        printfn ""
        printfn "                                           joins per 10,000 sweeps"
        printfn "barrier  reduction    kT   occupancy   same side   no template   with template   enhancement      +/-   templated   longest   end joins"

        use writer = new StreamWriter(csvPath)
        writer.WriteLine(
            "barrier,reduction,kT,occupancy,same_side_fraction,"
                + "rate_no_template,rate_with_template,"
                + "enhancement,enhancement_stderr,enhancement_is_lower_bound,"
                + "templated_fraction,longest_chain,"
                + "joins_no_template,joins_with_template,template_end_joins")

        for (barrier, reduction, kT), controlRuns, templateRuns in results do
            let occupancy = templateRuns |> Array.averageBy _.Occupancy
            let sameSide =
                let nAdjacent = templateRuns |> Array.sumBy _.AdjacentPairs
                if nAdjacent = 0 then nan
                else
                    float (templateRuns |> Array.sumBy _.SameSidePairs)
                        / float nAdjacent
            let controlRate, nControl, nControlSweeps = getRate controlRuns
            let templateRate, nTemplate, _ = getRate templateRuns

                // ratio of two rates; relative errors add in quadrature. If
                // nothing joined without the template, the ratio is at least
                // what a single join there would have given.
            let enhancement, enhancementErr, isLowerBound =
                if nTemplate = 0 then nan, nan, false
                elif nControl = 0 then
                    let rateOfOneJoin = 10000.0 / float nControlSweeps
                    templateRate / rateOfOneJoin, nan, true
                else
                    let ratio = templateRate / controlRate
                    ratio,
                    ratio * sqrt (1.0 / float nControl + 1.0 / float nTemplate),
                    false

            let templatedFraction =
                if nTemplate = 0 then nan
                else
                    float (templateRuns |> Array.sumBy _.TemplatedJoins)
                        / float nTemplate
            let longestChain =
                templateRuns |> Array.averageBy (fun run -> float run.LongestChain)
            let nEndJoins = templateRuns |> Array.sumBy _.TemplateEndJoins

            printfn
                "%7.2f    %7.2f  %4.2f     %5.3f       %5.2f     %9.3f       %9.3f     %s%8.1f   %6.1f       %5.2f     %5.2f       %5d"
                barrier reduction kT occupancy sameSide controlRate templateRate
                (if isLowerBound then ">" else " ")
                enhancement enhancementErr templatedFraction longestChain nEndJoins
            writer.WriteLine(
                String.Join(
                    ",",
                    [
                        barrier; reduction; kT; occupancy; sameSide
                        controlRate; templateRate
                        enhancement; enhancementErr
                        (if isLowerBound then 1.0 else 0.0)
                        templatedFraction; longestChain
                        float nControl; float nTemplate; float nEndJoins
                    ]
                        |> Seq.map (fun (x : float) ->
                            x.ToString(Globalization.CultureInfo.InvariantCulture))))

        printfn ""
        printfn "reduction: how much each stack lowers the strong bond barrier."
        printfn "occupancy: fraction of the template paired when measurement began."
        printfn "same side: of adjacent template atoms that were both paired, the fraction whose"
        printfn "   partners were on the same side of the template (and so could join)."
        printfn "enhancement: rate with the template divided by rate without; +/- is its standard error."
        printfn "   > means nothing joined without the template, so this is a lower bound."
        printfn "templated: fraction of joins (with template) between atoms paired to adjacent template atoms."
        printfn "longest: atoms in the longest chain of free atoms when a run stopped (with template; average)."
        printfn "end joins: free atoms that bonded to an end of the template (total over runs)."
        printfn "NaN: can't be estimated (nothing joined, or no adjacent pairs)."
        printfn "Results written to %s" (Path.GetFullPath csvPath)
