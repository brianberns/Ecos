namespace Ecos.Experiment

open System
open System.IO
open System.Threading

open Ecos
open Ecos.Experiment.Experiment3

/// Experiment 16: hairpins in a living world. The world of experiment 5
/// (scenario B with no template, so strands arise from free atoms and
/// die by hydrolysis), whose strands can now fold (see experiment 15).
/// Do hairpins become more common over the cycles, or are they only as
/// common as chance makes them?
///
/// At the end of each cold phase, when strands are paired, each capped
/// strand is counted as:
/// * folded, if at least two of its atoms are weakly bonded to other
///   atoms of the same strand;
/// * in a duplex, if it is weakly bonded to exactly one other strand;
/// * in a sheet, if it is weakly bonded to two or more other strands.
///
/// Its sequence is also checked for its longest possible hairpin stem:
/// the most complementary pairs it could form by folding back on
/// itself, with a loop of at least two atoms between the two arms.
///
/// This is the baseline for protection: a reduced rate of hydrolysis
/// for paired strong bonds, which should favor strands that fold.
module Experiment16 =

    /// Settings for the experiment.
    type Settings =
        {
            /// Number of independent runs (seeds).
            NumRuns : int

            /// Number of times to run through the scenario's phases.
            NumCycles : int

            /// Scenario to run. It must have a temperature cycle whose
            /// first phase is cold.
            Scenario : Scenario
        }

    /// Default settings: the living world of experiment 5, with the
    /// strong bond barrier of the living-world check.
    let defaultSettings =
        {
            NumRuns = 8
            NumCycles = 30
            Scenario =
                Experiment4.getScenario Experiment4.livingWorldSettings 15.0
                    |> Scenario.withName "Living world, barrier 15"
        }

    /// Smallest loop, in atoms, between the two arms of a hairpin.
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

    /// Census of one strand.
    type private StrandCensus =
        {
            /// Number of atoms.
            Length : int

            /// Number of weak bonds within the strand.
            InnerWeakBonds : int

            /// Number of other strands it is weakly bonded to.
            PartnerStrands : int

            /// Longest hairpin stem its sequence could form.
            MaxStem : int
        }

    /// Census of the capped strands in the given world.
    let private getCensus (world : World) =
        let strands = getStrands world
        let strandOf =
            dict [
                for iStrand, strand in Array.indexed strands do
                    for id in strand.AtomIds do
                        id, iStrand
            ]
        let weak = world.BondTypeMap[Weak]
        strands
            |> Array.indexed
            |> Array.where (snd >> isCapped world)
            |> Array.map (fun (iStrand, strand) ->
                let partners =
                    strand.AtomIds
                        |> Array.collect (fun id ->
                            BondMap.getBonds id weak |> Set.toArray)
                let partnerStrands =
                    partners
                        |> Array.choose (fun id ->
                            match strandOf.TryGetValue(id) with
                                | true, i -> Some i
                                | _ -> None)   // a free atom
                let inner =
                    (partnerStrands |> Array.where ((=) iStrand)).Length / 2   // each counted from both ends
                {
                    Length = strand.AtomIds.Length
                    InnerWeakBonds = inner
                    PartnerStrands =
                        partnerStrands
                            |> Array.where ((<>) iStrand)
                            |> Array.distinct
                            |> Array.length
                    MaxStem =
                        strand.AtomIds
                            |> Array.map (fun id -> world.Atoms[id].Type)
                            |> getMaxStem
                })

    /// Runs one world through the cycles. Answers the census at the end
    /// of each cycle's cold phase.
    let private runOne settings seed =
        let phases = Schedule.getPhases settings.Scenario.Schedule
        let mutable run = Run.create seed settings.Scenario
        [|
            for iCycle in 1 .. settings.NumCycles do
                run <- Run.advance phases[0].NumSweeps run   // cold
                getCensus run.World
                for phase in phases[1..] do
                    run <- Run.advance phase.NumSweeps run
                eprintfn "Seed %d: finished cycle %d of %d" seed iCycle settings.NumCycles
        |]

    /// Runs the experiment, prints a table of results, and writes them
    /// to the given CSV file.
    let run (settings : Settings) (csvPath : string) =

            // run every seed in parallel
        let mutable nDone = 0
        let runs =
            [| 0 .. settings.NumRuns - 1 |]
                |> Array.Parallel.map (fun seed ->
                    let results = runOne settings seed
                    let n = Interlocked.Increment(&nDone)
                    eprintfn "Finished run %d of %d" n settings.NumRuns
                    results)

        let scenario = settings.Scenario
        printfn "Experiment 16: hairpins in a living world"
        printfn "Scenario %s: %d runs of %d cycles" scenario.Name settings.NumRuns settings.NumCycles
        for phase in Schedule.getPhases scenario.Schedule do
            printfn "Phase: kT %g for %d sweeps" phase.Temperature phase.NumSweeps
        printParameters scenario.Parameters
        let strong = scenario.Parameters.BondTypeParameterMap[Strong]
        printfn "Strong bend limit %d/%d, stiffness %g"
            strong.BendLimit.Numerator strong.BendLimit.Denominator strong.BendStiffness
        printfn ""
        printfn "Capped strands at the end of each cold phase (pooled over runs)"
        printfn "cycle   capped  length   folded   stem>=2  stem>=3  folded of stem>=3   duplex    sheet"

        use writer = new StreamWriter(csvPath)
        writer.WriteLine("cycle,capped,mean_length,folded,stem2,stem3,folded_of_stem3,duplex,sheet")
        let invariant (x : float) = x.ToString(Globalization.CultureInfo.InvariantCulture)
        for iCycle in 0 .. settings.NumCycles - 1 do
            let strands = runs |> Array.collect (fun results -> results[iCycle])
            let fraction (strands : StrandCensus[]) test =
                if strands.Length = 0 then nan
                else
                    float (strands |> Array.where test).Length
                        / float strands.Length
            let capped = float strands.Length / float settings.NumRuns
            let meanLength =
                if strands.Length = 0 then nan
                else strands |> Array.averageBy (fun s -> float s.Length)
            let folded = fraction strands (fun s -> s.InnerWeakBonds >= 2)
            let stem2 = fraction strands (fun s -> s.MaxStem >= 2)
            let stem3 = fraction strands (fun s -> s.MaxStem >= 3)
            let foldedOfStem3 =
                fraction
                    (strands |> Array.where (fun s -> s.MaxStem >= 3))
                    (fun s -> s.InnerWeakBonds >= 2)
            let duplex = fraction strands (fun s -> s.PartnerStrands = 1)
            let sheet = fraction strands (fun s -> s.PartnerStrands >= 2)
            printfn "%5d  %7.1f  %6.2f   %6.3f   %7.3f  %7.3f  %17.3f   %6.3f   %6.3f"
                (iCycle + 1) capped meanLength folded stem2 stem3 foldedOfStem3 duplex sheet
            writer.WriteLine(
                String.Join(
                    ",",
                    [
                        string (iCycle + 1)
                        invariant capped; invariant meanLength
                        invariant folded; invariant stem2; invariant stem3; invariant foldedOfStem3
                        invariant duplex; invariant sheet
                    ]))

        printfn ""
        printfn "capped: capped strands per run. length: their mean number of atoms, caps included."
        printfn "folded: fraction with at least two weak bonds within the strand."
        printfn "stem>=k: fraction whose sequence could form a hairpin stem of at least k pairs,"
        printfn "   with a loop of at least %d atoms. folded of stem>=3: of those, the fraction folded." minLoop
        printfn "duplex: fraction weakly bonded to exactly one other strand; sheet: to two or more."
        printfn "Results written to %s" (Path.GetFullPath csvPath)
