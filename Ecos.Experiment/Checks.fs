namespace Ecos.Experiment

open System
open Ecos
open Ecos.Experiment.Experiment3

/// Checks that the model still does what it has been shown to do. The
/// unit tests check that each rule is followed; these check that the
/// rules, with the standard scenarios, still add up to the model's key
/// accomplishments. Each is a reduced version of an experiment, with a
/// threshold set from that experiment's results, well below them but
/// far above what a broken model would give.
///
/// They take several minutes, so run them after changing the core or
/// the scenarios, not with every build.
module Checks =

    /// Outcome of one check.
    type private Outcome =
        {
            /// What was checked.
            Name : string

            /// Whether the check passed.
            Passed : bool

            /// What was measured, and the threshold.
            Detail : string
        }

    /// Runs the given scenario with each of the given seeds, in
    /// parallel, through the given number of its cycles. Answers each
    /// run's world after each cycle.
    let private runCycles (scenario : Scenario) nCycles (seeds : int[]) =
        let period = Schedule.tryGetPeriod scenario.Schedule |> Option.get
        seeds
            |> Array.Parallel.map (fun seed ->
                let mutable run = Run.create seed scenario
                [|
                    for _ in 1 .. nCycles do
                        run <- Run.advance period run
                        run.World
                |])

    /// Capped chains in the given world, as their atom IDs.
    let private getCappedChains (world : World) =
        getStrands world
            |> Array.where (isCapped world)
            |> Array.map _.AtomIds

    /// Faithful copying: in scenario C, one twelve-atom template is
    /// copied, and its copies are copied, without loss. (Experiment 3c:
    /// about 3.3, 6.4 and 10.1 full-length capped strands per run after
    /// three cycles.)
    let private checkCopying () =
        let nCycles = 3
        let threshold = 7.0
        let worlds = runCycles (Scenario.createScenarioC true) nCycles [| 0 .. 11 |]
        let nFull =
            worlds
                |> Array.averageBy (fun cycles ->
                    getCappedChains cycles[nCycles - 1]
                        |> Array.where (fun ids -> ids.Length = 12)
                        |> Array.length
                        |> float)
        {
            Name = "Faithful copying (scenario C)"
            Passed = nFull >= threshold
            Detail = sprintf "%.1f full-length capped strands per run after %d cycles (need %.0f)" nFull nCycles threshold
        }

    /// Abiogenesis and turnover: in a living world with no template,
    /// replicators arise by chance and are copied, and strands keep
    /// dying and being replaced. (Experiment 5.)
    let private checkLivingWorld () =
        let nCycles = 15
        let scenario =
            Experiment4.getScenario Experiment4.livingWorldSettings 15.0
        let seeds = [| 0 .. 7 |]
        let worlds = runCycles scenario nCycles seeds

            // runs with a family of more than one capped chain at the end
        let nCopied =
            worlds
                |> Array.where (fun cycles ->
                    let world = cycles[nCycles - 1]
                    getCappedChains world
                        |> Array.countBy (fun ids ->
                            ids
                                |> Array.map (fun id -> world.Atoms[id].Type)
                                |> getFamily)
                        |> Array.exists (fun (_, n) -> n > 1))
                |> Array.length

            // of the capped chains five cycles before the end, the
            // fraction still intact at the end, pooled over runs
        let survived, total =
            worlds
                |> Array.fold (fun (survived, total) cycles ->
                    let before = getCappedChains cycles[nCycles - 6] |> Array.map set
                    let after = getCappedChains cycles[nCycles - 1] |> Array.map set |> set
                    let n = before |> Array.where after.Contains |> Array.length
                    survived + n, total + before.Length) (0, 0)
        let survival = if total > 0 then float survived / float total else nan
        let nAlive =
            worlds
                |> Array.where (fun cycles ->
                    (getCappedChains cycles[nCycles - 1]).Length > 0)
                |> Array.length

        let copiedOk = nCopied * 2 >= seeds.Length
        let turnoverOk = total > 0 && survival < 0.9 && nAlive * 2 >= seeds.Length
        {
            Name = "Abiogenesis and turnover (living world, barrier 15)"
            Passed = copiedOk && turnoverOk
            Detail =
                sprintf "copied families in %d of %d runs (need half); %.0f%% of capped strands survive 5 cycles (need under 90%%); strands alive in %d runs (need half)"
                    nCopied seeds.Length (100.0 * survival) nAlive
        }

    /// Selection on pairing strength: with Y-Z stronger than W-X,
    /// starting from equal numbers of each kind, Y/Z strands take over.
    /// (Experiment 8.)
    let private checkSelection () =
        let nCycles = 15
        let threshold = 0.58   // experiment 8: about 0.66 at cycle 15, and 0.5 without selection
        let seeds = [| 0 .. 11 |]
        let worlds = runCycles (Experiment8.getScenario 8 3.0) nCycles seeds
        let shares =
            worlds
                |> Array.map (fun cycles ->
                    let census = Experiment8.getCensus cycles[nCycles - 1]
                    let n = census.WX + census.YZ
                    if n > 0 then float census.YZ / float n else nan)
                |> Array.where (Double.IsNaN >> not)
        let mean = if shares.Length > 0 then Array.average shares else nan
        let nAhead = shares |> Array.where (fun s -> s > 0.5) |> Array.length
        {
            Name = "Selection on pairing strength (Y-Z energy 3)"
            Passed = mean >= threshold && nAhead * 4 >= seeds.Length * 3
            Detail =
                sprintf "Y/Z share %.2f after %d cycles (need %.2f); Y/Z ahead in %d of %d runs (need three quarters)"
                    mean nCycles threshold nAhead seeds.Length
        }

    /// Runs every check, prints the outcomes, and answers whether all
    /// of them passed.
    let run () =
        let checks = [ checkCopying; checkLivingWorld; checkSelection ]
        let outcomes =
            [
                for check in checks do
                    let stopwatch = Diagnostics.Stopwatch.StartNew()
                    let outcome = check ()
                    printfn
                        "%s  %s (%.0f s)\n      %s"
                        (if outcome.Passed then "PASS" else "FAIL")
                        outcome.Name
                        stopwatch.Elapsed.TotalSeconds
                        outcome.Detail
                    outcome
            ]
        let nFailed = outcomes |> List.where (fun o -> not o.Passed) |> List.length
        printfn ""
        if nFailed = 0 then printfn "All %d checks passed." outcomes.Length
        else printfn "%d of %d checks failed." nFailed outcomes.Length
        nFailed = 0
