namespace Ecos

open System
open System.Diagnostics

open Elmish

open Avalonia.Threading

open Ecos.Experiment

/// Elmish model.
type Model =
    {
        /// Index of the selected group of scenarios in the registry.
        GroupIndex : int

        /// Index of the selected scenario within its group.
        ScenarioIndex : int

        /// RNG seed.
        Seed : int

        /// The selected scenario, being run.
        Run : Run

        /// Indicates whether updates are enabled.
        IsRunning : bool

        /// Number of sweeps per timer tick.
        Speed : int

        /// Number of updates per second.
        UpdateRate : float
    }

/// Elmish messages.
type Message =

    /// Timer tick.
    | Tick

    /// Selects a group of scenarios.
    | SelectGroup of int

    /// Selects a scenario within the current group.
    | SelectScenario of int

    /// Sets the temperature by hand.
    | SetTemperature of float

    /// Turns updates on or off.
    | SetRun of bool

    /// Sets speed.
    | SetSpeed of int

    /// Sets RNG seed.
    | SetSeed of int

    /// Resets model.
    | Reset

module Model =

    /// Creates a model that runs the given scenario from the start.
    let create groupIndex scenarioIndex seed speed =
        let scenario =
            Registry.groups[groupIndex].Scenarios[scenarioIndex]
        {
            GroupIndex = groupIndex
            ScenarioIndex = scenarioIndex
            Seed = seed
            Run = Run.create seed scenario
            IsRunning = false
            Speed = speed
            UpdateRate = 0
        }

    /// Initializes a model.
    let init () =
        create 0 0 0 200

    let private stopwatch = Stopwatch()

    let mutable private lastElapsed = stopwatch.Elapsed

    /// Runs one tick's worth of sweeps.
    let handleTick model =
        if model.IsRunning then
            let run = Run.advance model.Speed model.Run
            let updateRate =
                let prev = lastElapsed
                lastElapsed <- stopwatch.Elapsed
                1.0 / (lastElapsed - prev).TotalSeconds
            { model with
                Run = run
                UpdateRate = updateRate }
        else model

    /// Handles selection of a group of scenarios.
    let handleSelectGroup groupIndex model =
        if groupIndex = model.GroupIndex then model
        else create groupIndex 0 model.Seed model.Speed

    /// Handles selection of a scenario.
    let handleSelectScenario scenarioIndex model =
        if scenarioIndex = model.ScenarioIndex then model
        else create model.GroupIndex scenarioIndex model.Seed model.Speed

    /// Handles temperature change.
    let handleSetTemperature kT (model : Model) =
        { model with
            Run = { model.Run with Schedule = Constant kT } }

    /// Handles `run` flag change.
    let handleSetRun isRunning model =
        if isRunning then
            stopwatch.Start()
            lastElapsed <- stopwatch.Elapsed
        else stopwatch.Stop()
        { model with IsRunning = isRunning }

    /// Handles speed change.
    let handleSetSpeed speed model =
        { model with Speed = speed }

    /// Handles RNG seed change.
    let handleSetSeed seed model =
        { model with Seed = seed }

    /// Handles model reset.
    let handleReset model =
        create model.GroupIndex model.ScenarioIndex model.Seed model.Speed

    /// Handles a message.
    let update msg model =
        match msg with
            | Tick -> handleTick model
            | SelectGroup index -> handleSelectGroup index model
            | SelectScenario index -> handleSelectScenario index model
            | SetTemperature kT -> handleSetTemperature kT model
            | SetRun isRunning -> handleSetRun isRunning model
            | SetSpeed speed -> handleSetSpeed speed model
            | SetSeed seed -> handleSetSeed seed model
            | Reset -> handleReset model

    /// Subscribable timer.
    let timer : Subscribe<_> =
        let timerSub dispatch =
            let timer =
                new DispatcherTimer(
                    TimeSpan.FromSeconds(1.0 / 24.0),
                    DispatcherPriority.Normal,
                    EventHandler(fun _ _ ->
                        dispatch Tick))
            timer.Start()
            { new IDisposable with
                member _.Dispose() = timer.Stop() }
        timerSub

    /// Timer subscription.
    let subscribe world : Sub<_> =
        [ ["timer"], timer ]
