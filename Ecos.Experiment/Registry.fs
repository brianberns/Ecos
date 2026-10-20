namespace Ecos.Experiment

/// Every scenario that can be run, in groups: the standard scenarios,
/// then each experiment's, with its default settings. The desktop app
/// offers them all, so that any world an experiment ran can be watched
/// in it.
module Registry =

    /// A named group of scenarios.
    type Group =
        {
            /// Name of the group.
            Name : string

            /// Scenarios in the group.
            Scenarios : Scenario[]
        }

    /// All groups.
    let groups =
        [|
            {
                Name = "Standard"
                Scenarios =
                    [|
                        Scenario.createScenarioA true
                        Scenario.createScenarioA false
                        Scenario.createScenarioB true
                        Scenario.createScenarioB false
                        Scenario.createScenarioC true
                        Scenario.createScenarioC false
                        Scenario.createVentScenario 8 3.0
                    |]
            }
            {
                Name = "Experiment 1"
                Scenarios = Experiment1.getScenarios Experiment1.defaultSettings
            }
            {
                Name = "Experiment 2"
                Scenarios = Experiment2.getScenarios Experiment2.defaultSettings
            }
            {
                Name = "Experiment 3"
                Scenarios =
                    Array.append
                        (Experiment3.getScenarios Experiment3.defaultSettings)
                        (Experiment3.getScenarios Experiment3.copyingSettings)
            }
            {
                Name = "Experiment 4"
                Scenarios = Experiment4.getScenarios Experiment4.defaultSettings
            }
            {
                Name = "Experiment 5"
                Scenarios = Experiment4.getScenarios Experiment4.livingWorldSettings
            }
            {
                Name = "Experiment 6"
                Scenarios = Experiment6.getScenarios Experiment6.defaultSettings
            }
            {
                Name = "Experiment 7"
                Scenarios = Experiment7.getScenarios Experiment7.defaultSettings
            }
            {
                Name = "Experiment 8"
                Scenarios = Experiment8.getScenarios Experiment8.defaultSettings
            }
            {
                Name = "Experiment 9"
                Scenarios = Experiment9.getScenarios Experiment9.defaultSettings
            }
            {
                Name = "Experiment 10"
                Scenarios = Experiment9.getScenarios Experiment9.templateSettings
            }
        |]
