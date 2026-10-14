namespace Ecos

open System
open Elmish
open Avalonia.Threading
open Ecos.Experiment

/// Elmish model.
type Model =
    {
        /// RNG seed.
        Seed : int

        /// Initial scenario.
        Scenario : Scenario

        /// Current world state.
        World : World

        /// Current temperature.
        TemperatureType : TemperatureType

        /// Indicates whether updates are enabled.
        Run : bool

        /// Number of sweeps per timer tick.
        Speed : int

        /// Number of sweeps run so far.
        SweepCount : int
    }

    /// Gets the model's current temperature.
    member this.Temperature =
        match this.TemperatureType with
            | Adjustable kT -> kT
            | Cycle phases -> phases[0].Temperature

/// Elmish messages.
type Message =

    /// Timer tick.
    | Tick

    /// Sets temperature.
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

    /// Creates a model.
    let create seed speed =
        let scenario = Scenario.createExperiment3 seed
        {
            Seed = seed
            Scenario = scenario
            World = scenario.World
            TemperatureType = scenario.TemperatureType
            Run = false
            Speed = speed
            SweepCount = 0
        }

    /// Initializes a model.
    let init () =
        create 0 200

    /// Runs one sweep.
    let private runSweep (model : Model) =

            // update world
        let world =
            let parms =
                {
                    Temperature = model.Temperature
                    BondParameterMap = model.Scenario.BondParameterMap
                    PeelBarrier = 6
                    CapPeelBarrier = 27
                }
            World.sweep parms model.Scenario.Random model.World

            // update temperature
        let tempType =
            match model.TemperatureType with
                | Cycle phases ->

                        // update current phase
                    let phase = phases[0]
                    assert(phase.NumSweeps > 0)
                    let phase =
                        { phase with NumSweeps = phase.NumSweeps - 1 }

                        // current phase is over?
                    if phase.NumSweeps = 0 then
                        if phases.Length = 1 then
                            model.Scenario.TemperatureType   // start cycling from the beginning again
                        else
                            Cycle phases[1..]                // move to next phase
                    else
                        Cycle [|
                            phase
                            yield! phases[1..]
                        |]

                | tempType -> tempType

            // update model
        { model with
            World = world
            TemperatureType = tempType
            SweepCount = model.SweepCount + 1 }

    /// Moves N random atoms per tick. Duplicates or omissions are
    /// possible.
    let handleTick model =
        if model.Run then
            (model, [ 1 .. model.Speed ])
                ||> Seq.fold (fun model _ ->
                    runSweep model)
        else model

    /// Handles temperature change.
    let handleSetTemperature kT (model : Model) =
        { model with
            TemperatureType = Adjustable kT }

    /// Handles `run` flag change.
    let handleSetRun run model =
        { model with Run = run }

    /// Handles speed change.
    let handleSetSpeed speed model =
        { model with Speed = speed }

    /// Handles RNG seed change.
    let handleSetSeed seed model =
        { model with Seed = seed }

    /// Handles model reset.
    let handleReset model =
        create model.Seed model.Speed

    /// Handles a message.
    let update msg model =
        match msg with
            | Tick -> handleTick model
            | SetTemperature kT -> handleSetTemperature kT model
            | SetRun run -> handleSetRun run model
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
