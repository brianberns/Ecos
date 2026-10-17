namespace Ecos.Experiment

module Program =

    /// Prints usage.
    let private printUsage () =
        printfn "Usage: dotnet run -c Release -- <experiment> [csv path]"
        printfn ""
        printfn "Experiments:"
        printfn "   experiment1   Fraction of template paired, by temperature"
        printfn "   experiment2   Joining of free atoms, with and without a template"
        printfn "   experiment3   Copying and separation under temperature cycling"
        printfn "   experiment4   Emergence of strands from free atoms, with no template"
        printfn "   experiment5   A living world: emergence, as in experiment 4, with turnover"
        printfn "   experiment6   Competition between the template and chance replicators"
        printfn "   experiment7   Selection between W/X and Y/Z pairing, by Y-Z bond strength"
        printfn "   experiment8   Head to head between W/X and Y/Z strands, by Y-Z bond energy"

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
            | [ "experiment3" ] ->
                Experiment3.run Experiment3.defaultSettings "experiment3.csv"
                0
            | [ "experiment3"; csvPath ] ->
                Experiment3.run Experiment3.defaultSettings csvPath
                0
            | [ "experiment4" ] ->
                Experiment4.run Experiment4.defaultSettings "experiment4.csv"
                0
            | [ "experiment4"; csvPath ] ->
                Experiment4.run Experiment4.defaultSettings csvPath
                0
            | [ "experiment5" ] ->
                Experiment4.run Experiment4.livingWorldSettings "experiment5.csv"
                0
            | [ "experiment5"; csvPath ] ->
                Experiment4.run Experiment4.livingWorldSettings csvPath
                0
            | [ "experiment6" ] ->
                Experiment6.run Experiment6.defaultSettings "experiment6.csv"
                0
            | [ "experiment6"; csvPath ] ->
                Experiment6.run Experiment6.defaultSettings csvPath
                0
            | [ "experiment7" ] ->
                Experiment7.run Experiment7.defaultSettings "experiment7.csv"
                0
            | [ "experiment7"; csvPath ] ->
                Experiment7.run Experiment7.defaultSettings csvPath
                0
            | [ "experiment8" ] ->
                Experiment8.run Experiment8.defaultSettings "experiment8.csv"
                0
            | [ "experiment8"; csvPath ] ->
                Experiment8.run Experiment8.defaultSettings csvPath
                0
            | _ ->
                printUsage ()
                1
