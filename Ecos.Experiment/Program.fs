namespace Ecos.Experiment

module Program =

    /// Each experiment's name, description, and how to run it, given
    /// a CSV path for its results.
    let private experiments : (string * string * (string -> unit)) list =
        [
            "experiment1", "Fraction of template paired, by temperature",
                Experiment1.run Experiment1.defaultSettings
            "experiment2", "Joining of free atoms, with and without a template",
                Experiment2.run Experiment2.defaultSettings
            "experiment3", "Copying and separation under temperature cycling",
                Experiment3.run Experiment3.defaultSettings
            "experiment3c", "The same, in scenario C (designed for copying)",
                Experiment3.run Experiment3.copyingSettings
            "experiment4", "Emergence of strands from free atoms, with no template",
                Experiment4.run Experiment4.defaultSettings
            "experiment5", "A living world: emergence, as in experiment 4, with turnover",
                Experiment4.run Experiment4.livingWorldSettings
            "experiment8", "Head to head between W/X and Y/Z strands, by Y-Z bond energy",
                Experiment8.run Experiment8.defaultSettings
            "experiment11", "Melting of ten-letter duplexes, by temperature",
                Experiment11.run Experiment11.defaultSettings
            "experiment14", "Copying near walls and in corners",
                Experiment14.run Experiment14.defaultSettings
            "experiment14b", "Templates competing from the center, a side, an end, and a corner",
                Experiment14.runCompetition Experiment14.defaultCompetitionSettings
            "experiment15", "Hairpins of a self-complementary strand, by bend limit and stiffness",
                Experiment15.run Experiment15.defaultSettings
            "experiment16", "Hairpins, duplexes and sheets in a living world, by cycle",
                Experiment16.run Experiment16.defaultSettings
        ]

    /// Prints usage.
    let private printUsage () =
        printfn "Usage: dotnet run -c Release -- <experiment> [csv path]"
        printfn "   or: dotnet run -c Release -- checks"
        printfn ""
        printfn "Experiments:"
        for name, description, _ in experiments do
            printfn "   %-13s %s" name description
        printfn ""
        printfn "Experiments 6, 7, 9, 10, 12 and 13 are archived: see the git history."
        printfn ""
        printfn "checks: confirms, in several minutes, that the model's key accomplishments still hold."

    [<EntryPoint>]
    let main argv =
        let tryRun name csvPath =
            experiments
                |> List.tryFind (fun (name', _, _) -> name' = name)
                |> Option.map (fun (_, _, run) -> run csvPath)
        let result =
            match List.ofArray argv with
                | [ "checks" ] -> Some (if Checks.run () then 0 else 1)
                | [ name ] -> tryRun name $"{name}.csv" |> Option.map (fun () -> 0)
                | [ name; csvPath ] -> tryRun name csvPath |> Option.map (fun () -> 0)
                | _ -> None
        match result with
            | Some exitCode -> exitCode
            | None ->
                printUsage ()
                1
