namespace Ecos.Experiment

open System
open System.IO
open System.Threading

open Ecos
open Ecos.Experiment.Experiment3

/// Experiment 14: walls. Does a template copy less well near a wall, or
/// in a corner? One template is placed in scenario C's conditions (it is
/// copied only on a template, nothing dies, and nothing else competes
/// with it), at the center of the box, or a given gap from one wall or
/// from two. Near one wall it lies either along the wall (side) or with
/// its end pointing at it (end); in a corner it does both. The free atoms
/// are scattered at random over the whole box, rather than in strips
/// near the center, so that a template's place doesn't decide how many
/// atoms start out near it.
module Experiment14 =

    /// Where a template is placed.
    type Placement =
        | Center
        | Side of gap : int
        | End of gap : int
        | Corner of gap : int

    /// Settings for the experiment.
    type Settings =
        {
            /// Gaps from the wall (distance from the center of the wall
            /// to the center of the nearest template atom) to try.
            Gaps : int[]

            /// Number of independent runs (seeds) at each placement.
            NumRuns : int

            /// Number of times to run through the scenario's phases.
            NumCycles : int
        }

    /// Default settings.
    let defaultSettings =
        {
            Gaps = [| 4; 8; 16; 32 |]
            NumRuns = 12
            NumCycles = 3
        }

    /// The template's coding atoms.
    let private nCoding = 10

    /// Inside of a 160x160 box of walls, which sits in a slightly larger
    /// torus (so that the walls on opposite sides are distinct). Walls
    /// are this experiment's subject; the standard world has none.
    /// The walls are 80 from the center of the world, on each side.
    let private wall = 80

    /// Location of the template's first atom, for the given placement.
    /// The template lies along the x axis, so that it ends near the left
    /// wall and lies along the top one.
    let private getStart = function
        | Center -> -nCoding - 2, 0
        | Side gap -> -nCoding - 2, wall - gap
        | End gap -> -wall + gap, 0
        | Corner gap -> -wall + gap, wall - gap

    /// Name of the given placement.
    let private getName = function
        | Center -> "center"
        | Side gap -> $"side, gap {gap}"
        | End gap -> $"end, gap {gap}"
        | Corner gap -> $"corner, gap {gap}"

    /// Scenario C, with its template at the given placement and its free
    /// atoms scattered at random.
    let getScenario placement =
        let scenario = Scenario.createScenarioC true
        let build parms (rng : Random) =
            let codingTypes = [| AtomType.W; AtomType.Y |]
            let types =
                [|
                    AtomType.P
                    for _ in 1 .. nCoding do
                        codingTypes[rng.Next(codingTypes.Length)]
                    AtomType.Q
                |]
            let x, y = getStart placement
            let world =
                World.createSquare (2 * wall + 4)
                    |> Build.addBox (2 * wall) (2 * wall)
                    |> Build.tryAddStrand parms x y types
                    |> Option.get
            let template = Array.map _.Id world.Atoms
            let world = Build.addFreeAtomsNear rng (wall - 2) (12 * nCoding) world
            world, template
        { scenario with
            Name = $"Wall test, {getName placement}"
            Build = build }

    /// The placements to try.
    let private getPlacements settings =
        [|
            Center
            for gap in settings.Gaps do
                Side gap
                End gap
                Corner gap
        |]

    /// The experiment's scenarios: one for each placement.
    let getScenarios settings =
        getPlacements settings |> Array.map getScenario

    /// Runs one world through the cycles. Answers the number of
    /// full-length capped chains in the template's family after each
    /// cycle.
    let private runOne settings placement seed =
        let scenario = getScenario placement
        let mutable run = Run.create seed scenario
        let family =
            run.Template
                |> Array.map (fun id -> run.World.Atoms[id].Type)
                |> getFamily
        [|
            for _ in 1 .. settings.NumCycles do
                for phase in getPhases scenario do
                    run <- Run.advance phase.NumSweeps run
                let world = run.World
                getStrands world
                    |> Array.where (fun strand ->
                        isCapped world strand
                            && strand.AtomIds.Length = nCoding + 2
                            && (strand.AtomIds
                                    |> Array.map (fun id -> world.Atoms[id].Type)
                                    |> getFamily) = family)
                    |> Array.length
        |]

    /// Runs the experiment, prints a table of results, and writes them
    /// to the given CSV file.
    let run (settings : Settings) (csvPath : string) =

        let placements = getPlacements settings
        let combos =
            [|
                for placement in placements do
                    for seed in 0 .. settings.NumRuns - 1 do
                        placement, seed
            |]

            // run every combination in parallel
        let mutable nDone = 0
        let results =
            combos
                |> Array.Parallel.map (fun (placement, seed) ->
                    let counts = runOne settings placement seed
                    let n = Interlocked.Increment(&nDone)
                    eprintfn "Finished run %d of %d" n combos.Length
                    placement, counts)
                |> Array.groupBy fst
                |> Array.map (fun (placement, group) ->
                    placement, Array.map snd group)
                |> Map

        printfn "Experiment 14: copying near walls and in corners"
        printfn
            "%d runs of %d cycles at each placement; one template in scenario C's conditions; free atoms scattered at random"
            settings.NumRuns settings.NumCycles
        printParameters (Scenario.createScenarioC true).Parameters
        printfn ""
        let header =
            [ for i in 1 .. settings.NumCycles -> sprintf "  cycle %d      " i ]
                |> String.concat ""
        printfn "placement           %s" header

        use writer = new StreamWriter(csvPath)
        writer.WriteLine("placement,gap,cycle,mean,stderr")

        for placement in placements do
            let runs = results[placement]
            let cells =
                [
                    for iCycle in 0 .. settings.NumCycles - 1 do
                        let values = runs |> Array.map (fun counts -> float counts[iCycle])
                        let mean = Array.average values
                        let sd =
                            sqrt (
                                (values |> Array.sumBy (fun v -> (v - mean) ** 2.0))
                                    / float (values.Length - 1))
                        let se = sd / sqrt (float values.Length)
                        let kind, gap =
                            match placement with
                                | Center -> "center", ""
                                | Side gap -> "side", string gap
                                | End gap -> "end", string gap
                                | Corner gap -> "corner", string gap
                        writer.WriteLine(
                            String.Join(
                                ",",
                                [
                                    kind; gap; string (iCycle + 1)
                                    mean.ToString(Globalization.CultureInfo.InvariantCulture)
                                    se.ToString(Globalization.CultureInfo.InvariantCulture)
                                ]))
                        sprintf "%5.2f +/- %4.2f  " mean se
                ]
            printfn "%-18s  %s" (getName placement) (String.concat "" cells)

        printfn ""
        printfn "Full-length capped chains in the template's family (its sequence or its complement), per run."
        printfn "gap: distance from the center of the wall to the center of the template's nearest atom."
        printfn "side: along the top wall. end: pointing at the left wall. corner: both."
        printfn "+/-: standard error over runs."
        printfn "Results written to %s" (Path.GetFullPath csvPath)

    /// Where each of the competing templates starts: the center, along
    /// the top wall, pointing at the left wall, and in the bottom-right
    /// corner, so that each starts well away from the others.
    let private getCompetitorStarts gap =
        let length = 2 * (nCoding + 2) - 2   // from first atom to last
        [|
            "center", (-nCoding - 2, 0)
            "side", (-nCoding - 2, wall - gap)
            "end", (-wall + gap, 0)
            "corner", (wall - gap - length, -wall + gap)
        |]

    /// Settings for the competitive version (experiment 14b).
    type CompetitionSettings =
        {
            /// Gap from the wall (as above) of the templates near walls.
            Gap : int

            /// Number of independent runs (seeds).
            NumRuns : int

            /// Number of times to run through the scenario's phases.
            NumCycles : int
        }

    /// Default settings for the competitive version.
    let defaultCompetitionSettings =
        {
            Gap = 8
            NumRuns = 24
            NumCycles = 3
        }

    /// Scenario C with four templates, each with its own random sequence,
    /// at the competitors' starts, and free atoms scattered at random.
    /// They compete for the same free atoms.
    let getCompetitionScenario gap =
        let scenario = Scenario.createScenarioC true
        let build parms (rng : Random) =
            let codingTypes = [| AtomType.W; AtomType.Y |]
            let world =
                (World.createSquare (2 * wall + 4)
                    |> Build.addBox (2 * wall) (2 * wall), getCompetitorStarts gap)
                    ||> Array.fold (fun world (_, (x, y)) ->
                        let types =
                            [|
                                AtomType.P
                                for _ in 1 .. nCoding do
                                    codingTypes[rng.Next(codingTypes.Length)]
                                AtomType.Q
                            |]
                        Build.tryAddStrand parms x y types world |> Option.get)
            let templates = Array.map _.Id world.Atoms
            let world = Build.addFreeAtomsNear rng (wall - 2) (12 * nCoding) world
            world, templates
        { scenario with
            Name = $"Wall competition, gap {gap}"
            Build = build }

    /// Runs one world through the cycles. Answers, after each cycle, the
    /// number of full-length capped chains in each template's family.
    let private runCompetitionOne settings seed =
        let scenario = getCompetitionScenario settings.Gap
        let mutable run = Run.create seed scenario
        let nTemplates = (getCompetitorStarts settings.Gap).Length
        let families =
            Array.init nTemplates (fun i ->
                run.Template[i * (nCoding + 2) .. (i + 1) * (nCoding + 2) - 1]
                    |> Array.map (fun id -> run.World.Atoms[id].Type)
                    |> getFamily)
        [|
            for _ in 1 .. settings.NumCycles do
                for phase in getPhases scenario do
                    run <- Run.advance phase.NumSweeps run
                let world = run.World
                let chainFamilies =
                    getStrands world
                        |> Array.where (fun strand ->
                            isCapped world strand
                                && strand.AtomIds.Length = nCoding + 2)
                        |> Array.map (fun strand ->
                            strand.AtomIds
                                |> Array.map (fun id -> world.Atoms[id].Type)
                                |> getFamily)
                families
                    |> Array.map (fun family ->
                        chainFamilies
                            |> Array.where ((=) family)
                            |> Array.length)
        |]

    /// Runs the competitive version, prints a table of results, and
    /// writes them to the given CSV file.
    let runCompetition (settings : CompetitionSettings) (csvPath : string) =

        let starts = getCompetitorStarts settings.Gap
        let mutable nDone = 0
        let runs =
            [| 0 .. settings.NumRuns - 1 |]
                |> Array.Parallel.map (fun seed ->
                    let counts = runCompetitionOne settings seed
                    let n = Interlocked.Increment(&nDone)
                    eprintfn "Finished run %d of %d" n settings.NumRuns
                    counts)

        printfn "Experiment 14b: templates competing from the center, a side, an end, and a corner"
        printfn
            "%d runs of %d cycles; four templates with their own random sequences, a gap of %d from the walls; scenario C's conditions; free atoms scattered at random"
            settings.NumRuns settings.NumCycles settings.Gap
        printParameters (Scenario.createScenarioC true).Parameters
        printfn ""
        let header =
            [ for i in 1 .. settings.NumCycles -> sprintf "  cycle %d      " i ]
                |> String.concat ""
        printfn "template   %s   largest at the end" header

        use writer = new StreamWriter(csvPath)
        writer.WriteLine("template,cycle,mean,stderr")

        let iLast = settings.NumCycles - 1
        for iTemplate, (name, _) in Array.indexed starts do
            let cells =
                [
                    for iCycle in 0 .. settings.NumCycles - 1 do
                        let values =
                            runs |> Array.map (fun (counts : int[][]) -> float (counts[iCycle][iTemplate]))
                        let mean = Array.average values
                        let sd =
                            sqrt (
                                (values |> Array.sumBy (fun v -> (v - mean) ** 2.0))
                                    / float (values.Length - 1))
                        let se = sd / sqrt (float values.Length)
                        writer.WriteLine(
                            String.Join(
                                ",",
                                [
                                    name; string (iCycle + 1)
                                    mean.ToString(Globalization.CultureInfo.InvariantCulture)
                                    se.ToString(Globalization.CultureInfo.InvariantCulture)
                                ]))
                        sprintf "%5.2f +/- %4.2f  " mean se
                ]

                // runs in which this template's lineage alone was the largest
            let nLargest =
                runs
                    |> Array.where (fun counts ->
                        let final = counts[iLast]
                        final
                            |> Array.indexed
                            |> Array.forall (fun (j, n) -> j = iTemplate || n < final[iTemplate]))
                    |> Array.length
            printfn "%-9s  %s   %d of %d runs" name (String.concat "" cells) nLargest settings.NumRuns

        printfn ""
        printfn "Full-length capped chains in each template's family (its sequence or its complement), per run."
        printfn "center: middle of the box. side: along the top wall. end: pointing at the left wall."
        printfn "corner: in the bottom-right corner, along the bottom wall, with its end pointing at the right one."
        printfn "+/-: standard error over runs. largest: runs in which that lineage alone was the largest."
        printfn "Results written to %s" (Path.GetFullPath csvPath)
