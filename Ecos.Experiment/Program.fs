namespace Ecos.Experiment

module Program =

    /// Prints usage.
    let private printUsage () =
        printfn "Usage: dotnet run -c Release -- <experiment> [csv path]"
        printfn ""
        printfn "Experiments:"
        printfn "   experiment1   Fraction of template paired, by temperature"
        printfn "   experiment2   Joining of free atoms, with and without a template"

    [<EntryPoint>]
    let main argv =
        match List.ofArray argv with
            | [ "experiment1" ] ->
                Experiment1.run Experiment1.defaultSettings "experiment1.csv"
                0
            | [ "experiment1"; csvPath ] ->
                Experiment1.run Experiment1.defaultSettings csvPath
                0
            | [ "experiment2" ] ->
                Experiment2.run Experiment2.defaultSettings "experiment2.csv"
                0
            | [ "experiment2"; csvPath ] ->
                Experiment2.run Experiment2.defaultSettings csvPath
                0
            | _ ->
                printUsage ()
                1
