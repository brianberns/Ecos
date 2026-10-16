namespace Ecos.Experiment

open System
open System.IO
open System.Threading

open Ecos
open Ecos.Experiment.Experiment3

/// Experiment 6: competition. The template starts among free atoms, as
/// in experiment 3, but with the conditions of a living world: strands
/// can also arise by chance, as in experiment 4, and they die when
/// their caps come off. Does the template's lineage get established,
/// do short replicators arise alongside it, and which kind comes to
/// dominate?
///
/// Capped chains are sorted into four kinds:
/// * the template's family: its sequence or its complement, read
///   from either end
/// * short chains (3 to 5 atoms), which almost always arise by chance
/// * mid-length chains (6 to 11 atoms), mostly copies of the template
///   that lost atoms
/// * anything else
/// The split by length is a rule of thumb: a short chain could in
/// principle be a copy of the template that lost most of its atoms.
module Experiment6 =

    /// Settings for the experiment.
    type Settings =
        {
            /// Strong bond barriers to run with. The barrier reduction
            /// per stack is set to the same value.
            StrongBarriers : float[]

            /// Number of independent runs (seeds) with each barrier.
            NumRuns : int

            /// Number of times to run through the scenario's phases.
            NumCycles : int
        }

    /// Default settings.
    let defaultSettings =
        {
            StrongBarriers = [| 20.0; 18.0; 16.0 |]
            NumRuns = 12
            NumCycles = 40
        }

    /// What a world contains at the end of a cycle.
    type private CycleResult =
        {
            /// Capped chains in the template's family.
            TemplateFamily : int

            /// Other capped chains of 3 to 5 atoms.
            Short : int

            /// Other capped chains of 6 to 11 atoms.
            Mid : int

            /// Any other capped chains.
            Other : int
        }

    /// The given scenario's parameters at the given temperature, with
    /// the given strong bond barrier (and reduction per stack).
    let private getParameters scenario barrier kT =
        let parms = Experiment3.getParameters scenario None kT
        let strongParms =
            { parms.BondParameterMap[Strong] with
                Barrier = barrier
                BarrierStackReduction = barrier }
        { parms with
            BondParameterMap =
                Map.add Strong strongParms parms.BondParameterMap }

    /// Sorts the capped chains in the given world.
    let private getCensus templateFamily (world : World) =
        let chains =
            getStrands world
                |> Array.where (isCapped world)
                |> Array.map (fun strand ->
                    strand.AtomIds
                        |> Array.map (fun id -> world.Atoms[id].Type))
        let count test =
            chains |> Array.where test |> Array.length
        let isTemplate types = getFamily types = templateFamily
        {
            TemplateFamily = count isTemplate
            Short =
                count (fun types ->
                    not (isTemplate types)
                        && types.Length >= 3 && types.Length <= 5)
            Mid =
                count (fun types ->
                    not (isTemplate types)
                        && types.Length >= 6 && types.Length <= 11)
            Other =
                count (fun types ->
                    not (isTemplate types)
                        && (types.Length < 3 || types.Length > 11))
        }

    /// Runs one world through the cycles. Answers a result for each
    /// cycle.
    let private runOne settings barrier seed =
        let scenario = Scenario.createScenarioB true seed
        let templateFamily =
            scenario.Template
                |> Array.map (fun id -> scenario.World.Atoms[id].Type)
                |> getFamily
        let phases = getPhases scenario
        let rng = scenario.Random
        let mutable world = scenario.World
        [|
            for iCycle in 1 .. settings.NumCycles do
                for phase in phases do
                    let parms = getParameters scenario barrier phase.Temperature
                    world <- World.sweep parms rng phase.NumSweeps world
                eprintfn "Barrier %g, seed %d: finished cycle %d of %d" barrier seed iCycle settings.NumCycles
                getCensus templateFamily world
        |]

    /// Runs the experiment, prints tables of results, and writes them
    /// to the given CSV file.
    let run (settings : Settings) (csvPath : string) =

        let combos =
            [|
                for barrier in settings.StrongBarriers do
                    for seed in 0 .. settings.NumRuns - 1 do
                        barrier, seed
            |]

            // run every combination in parallel
        let mutable nDone = 0
        let results =
            combos
                |> Array.Parallel.map (fun (barrier, seed) ->
                    let cycles = runOne settings barrier seed
                    let n = Interlocked.Increment(&nDone)
                    eprintfn "Finished run %d of %d" n combos.Length
                    barrier, cycles)
                |> Array.groupBy fst
                |> Array.map (fun (barrier, group) ->
                    barrier, Array.map snd group)
                |> Map

        let scenario = Scenario.createScenarioB true 0
        printfn "Experiment 6: competition between the template's lineage and chance replicators"
        printfn
            "%d runs of %d cycles with each barrier; %d atoms; template of %d atoms"
            settings.NumRuns settings.NumCycles
            scenario.World.Atoms.Length scenario.Template.Length
        for phase in getPhases scenario do
            printfn "Phase: kT %g for %d sweeps" phase.Temperature phase.NumSweeps
        printParameters (Experiment3.getParameters scenario None nan)
        printfn "(The strong bond barrier and its reduction per stack are replaced by each value below.)"

        use writer = new StreamWriter(csvPath)
        writer.WriteLine(
            "barrier,cycle,template_family,short,mid,other,"
                + "runs_template_alive,runs_with_short,runs_short_ahead")

        for barrier in settings.StrongBarriers do
            let runs = results[barrier]
            printfn ""
            printfn "Strong bond barrier %g" barrier
            printfn "              |------ capped chains per run ------|   |------- fraction of runs -------|"
            printfn "cycle         template    short     mid   other      template alive   any short   short ahead"
            for iCycle in 0 .. settings.NumCycles - 1 do
                let cycle = runs |> Array.map (fun cycles -> cycles[iCycle])
                let average f = cycle |> Array.averageBy (f >> float)
                let fraction test =
                    cycle |> Array.averageBy (fun c -> if test c then 1.0 else 0.0)
                let values =
                    [|
                        barrier
                        float (iCycle + 1)
                        average _.TemplateFamily
                        average _.Short
                        average _.Mid
                        average _.Other
                        fraction (fun c -> c.TemplateFamily > 0)
                        fraction (fun c -> c.Short > 0)
                        fraction (fun c -> c.Short > c.TemplateFamily)
                    |]
                printfn
                    "%5d           %6.2f   %6.2f  %6.2f  %6.2f          %6.2f      %6.2f        %6.2f"
                    (iCycle + 1) values[2] values[3] values[4] values[5]
                    values[6] values[7] values[8]
                writer.WriteLine(
                    String.Join(
                        ",",
                        values
                            |> Seq.map (fun (x : float) ->
                                x.ToString(Globalization.CultureInfo.InvariantCulture))))

                // when short chains first appeared, and how runs ended
            let firsts =
                runs
                    |> Array.choose (fun cycles ->
                        cycles
                            |> Array.tryFindIndex (fun c -> c.Short > 0)
                            |> Option.map ((+) 1))
            if firsts.Length = 0 then
                printfn "Short chains: none appeared."
            else
                let sorted = Array.sort firsts
                printfn
                    "Short chains first appeared in %d of %d runs: at cycle %d at the earliest, %d at the median, %d at the latest."
                    firsts.Length runs.Length
                    sorted[0] sorted[sorted.Length / 2] sorted[sorted.Length - 1]
            let final = runs |> Array.map (fun cycles -> cycles[settings.NumCycles - 1])
            let count test = final |> Array.where test |> Array.length
            printfn
                "At the end: template's family ahead in %d runs, short chains ahead in %d, tied in %d; template's family gone in %d."
                (count (fun c -> c.TemplateFamily > c.Short))
                (count (fun c -> c.Short > c.TemplateFamily))
                (count (fun c -> c.Short = c.TemplateFamily))
                (count (fun c -> c.TemplateFamily = 0))

        printfn ""
        printfn "template: capped chains with the template's sequence or its complement, read from either end."
        printfn "short: other capped chains of 3 to 5 atoms (almost always arisen by chance)."
        printfn "mid: other capped chains of 6 to 11 atoms (mostly copies of the template that lost atoms)."
        printfn "other: any other capped chains."
        printfn "short ahead: runs with more short chains than chains of the template's family."
        printfn "Results written to %s" (Path.GetFullPath csvPath)
