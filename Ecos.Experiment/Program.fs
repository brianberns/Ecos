namespace Ecos.Experiment

module Program =

    /// Prints usage.
    let private printUsage () =
        printfn "Usage: dotnet run -c Release -- <experiment> [csv path]"
        printfn ""
        printfn "Experiments:"
        printfn "   experiment1   Fraction of template paired, by temperature"

    [<EntryPoint>]
    let main argv =
        match List.ofArray argv with
            | [ "experiment1" ] ->
                Experiment1.run Experiment1.defaultSettings "experiment1.csv"
                0
            | [ "experiment1"; csvPath ] ->
                Experiment1.run Experiment1.defaultSettings csvPath
                0
            | _ ->
                printUsage ()
                1
