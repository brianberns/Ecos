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

            /// Bond parameters to run with. The strong bond barrier
            /// is replaced by each of the barriers above in turn.
            BondParameterMap : Map<BondType, BondTypeParameters>

            /// Number of independent runs (seeds) of each world, at
            /// each combination of temperature and barrier.
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
            Temperatures = [| 0.2; 0.25; 0.3; 0.4 |]
            StrongBondBarriers = [| 0.5; 0.75; 1.0; 1.5; 2.0 |]
            BondParameterMap =
                Map [
                    Strong, { Energy = 17.0; Barrier = 0.0 }   // barrier is replaced
                    Weak, { Energy = 1.0; Barrier = 0.0 }
                ]
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
        }

    /// Indicates whether the given atoms are weakly bonded to template
    /// atoms that are strongly bonded to each other.
    let private isTemplated (template : Set<AtomId>) (world : World) idA idB =
        let tryGetPartner id =
            world.BondTypeMap[Weak]
                |> Map.tryFind id
                |> Option.bind Seq.tryExactlyOne
                |> Option.filter template.Contains
        match tryGetPartner idA, tryGetPartner idB with
            | Some idA', Some idB' ->
                world.BondTypeMap[Strong]
                    |> Map.tryFind idA'
                    |> Option.exists (Set.contains idB')
            | _ -> false

    /// Runs one world, with or without a template, using the given
    /// parameters.
    let private runOne settings withTemplate (parms : Parameters) seed =
        let scenario =
            if withTemplate then Scenario.createExperiment1 seed
            else Scenario.createExperiment2 seed
        let rng = scenario.Random
        let template = set scenario.Template
        let nAtoms = scenario.World.AtomMap.Count
        let mutable world = scenario.World

            // settle, with strong bonds frozen by an impassable barrier
        let frozenParms =
            let strongParms =
                { parms.BondParameterMap[Strong] with
                    Barrier = Double.PositiveInfinity }
            { parms with
                BondParameterMap =
                    Map.add Strong strongParms parms.BondParameterMap }
        for _ in 1 .. settings.EquilibrationSweeps * nAtoms do
            world <- Ecos.World.update frozenParms rng world |> fst
        let occupancy =
            if withTemplate then
                Experiment1.getOccupancy scenario.Template world
            else 0.0

            // measure
        let mutable nSweeps = 0
        let mutable nJoins = 0
        let mutable nTemplated = 0
        let mutable nTemplateEnd = 0
        while nSweeps < settings.MaxSweeps && nJoins < settings.MaxJoins do
            for _ in 1 .. nAtoms do
                let world', eventOpt = Ecos.World.update parms rng world
                match eventOpt with
                    | Some (CreateBond (Strong, idA, idB)) ->
                        match template.Contains idA, template.Contains idB with
                            | false, false ->
                                nJoins <- nJoins + 1
                                if isTemplated template world' idA idB then
                                    nTemplated <- nTemplated + 1
                            | true, true -> ()   // can't happen: template is a chain
                            | _ -> nTemplateEnd <- nTemplateEnd + 1
                    | _ -> ()
                world <- world'
            nSweeps <- nSweeps + 1

        {
            Occupancy = occupancy
            Sweeps = nSweeps
            Joins = nJoins
            TemplatedJoins = nTemplated
            TemplateEndJoins = nTemplateEnd
        }

    /// Joins per 10,000 sweeps, pooled over the given runs, and the
    /// standard error of that rate. Joins are rare, independent events,
    /// so the error is the rate divided by the square root of the
    /// number of joins.
    let private getRate (runs : RunResult[]) =
        let nJoins = runs |> Array.sumBy _.Joins
        let nSweeps = runs |> Array.sumBy _.Sweeps
        let rate = 10000.0 * float nJoins / float nSweeps
        let stdErr =
            if nJoins = 0 then nan
            else rate / sqrt (float nJoins)
        rate, stdErr, nJoins

    /// Runs the experiment, prints a table of results, and writes them
    /// to the given CSV file.
    let run (settings : Settings) (csvPath : string) =

            // run every combination in parallel
        let results =
            [|
                for barrier in settings.StrongBondBarriers do
                    for kT in settings.Temperatures do
                        for withTemplate in [ false; true ] do
                            for seed in 0 .. settings.NumRuns - 1 do
                                barrier, kT, withTemplate, seed
            |]
                |> Array.Parallel.map (fun (barrier, kT, withTemplate, seed) ->
                    let parms =
                        let strongParms =
                            { settings.BondParameterMap[Strong] with
                                Barrier = barrier }
                        {
                            Temperature = kT
                            BondParameterMap =
                                Map.add Strong strongParms settings.BondParameterMap
                        }
                    (barrier, kT), (withTemplate, runOne settings withTemplate parms seed))
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
            settings.BondParameterMap[Strong].Energy
            settings.BondParameterMap[Weak].Energy
        printfn ""
        printfn "                         joins per 10,000 sweeps"
        printfn "barrier    kT   occupancy   no template   with template   enhancement     +/-   templated   end joins"

        use writer = new StreamWriter(csvPath)
        writer.WriteLine(
            "barrier,kT,occupancy,rate_no_template,rate_no_template_stderr,"
                + "rate_with_template,rate_with_template_stderr,"
                + "enhancement,enhancement_stderr,templated_fraction,"
                + "joins_no_template,joins_with_template,template_end_joins")

        for (barrier, kT), controlRuns, templateRuns in results do
            let occupancy = templateRuns |> Array.averageBy _.Occupancy
            let controlRate, controlErr, nControl = getRate controlRuns
            let templateRate, templateErr, nTemplate = getRate templateRuns

                // ratio of two rates; relative errors add in quadrature
            let enhancement, enhancementErr =
                if nControl = 0 || nTemplate = 0 then nan, nan
                else
                    let ratio = templateRate / controlRate
                    ratio,
                    ratio * sqrt (1.0 / float nControl + 1.0 / float nTemplate)

            let templatedFraction =
                if nTemplate = 0 then nan
                else
                    float (templateRuns |> Array.sumBy _.TemplatedJoins)
                        / float nTemplate
            let nEndJoins = templateRuns |> Array.sumBy _.TemplateEndJoins

            printfn
                "%7.2f  %4.2f     %5.3f     %9.3f       %9.3f      %8.2f  %6.2f      %5.2f       %5d"
                barrier kT occupancy controlRate templateRate
                enhancement enhancementErr templatedFraction nEndJoins
            writer.WriteLine(
                String.Join(
                    ",",
                    [
                        barrier; kT; occupancy
                        controlRate; controlErr
                        templateRate; templateErr
                        enhancement; enhancementErr; templatedFraction
                        float nControl; float nTemplate; float nEndJoins
                    ]
                        |> Seq.map (fun (x : float) ->
                            x.ToString(Globalization.CultureInfo.InvariantCulture))))

        printfn ""
        printfn "occupancy: fraction of the template paired when measurement began."
        printfn "enhancement: rate with the template divided by rate without; +/- is its standard error."
        printfn "templated: fraction of joins (with template) between atoms paired to adjacent template atoms."
        printfn "end joins: free atoms that bonded to an end of the template (total over runs)."
        printfn "NaN: no joins were seen, so the value can't be estimated."
        printfn "Results written to %s" (Path.GetFullPath csvPath)
