namespace Ecos

open System
open Elmish
open Avalonia.Threading
open Ecos.Experiment

/// Elmish model.
type Model =
    {
        /// Current world state.
        World : World

        /// RNG used to update the world.
        Random : Random

        /// Current update parameters.
        Parameters : Parameters

        /// Indicates whether updates are enabled.
        Run : bool
    }

/// Elmish messages.
type Message =

    /// Timer tick.
    | Tick

    /// Sets temperature.
    | SetTemperature of float

    /// Turns updates on or off.
    | SetRun of bool

module Model =

    /// Initializes a model.
    let init () =
        let scenario = Scenario.createExperiment1 0
        {
            World = scenario.World
            Random = scenario.Random
            Parameters =
                {
                    Temperature = 0.25
                    BondParameterMap =
                        let strong =
                            {
                                Energy = 17
                                Barrier = 3
                                BarrierStackReduction = 3
                            }
                        let weak =
                            {
                                Energy = 1
                                Barrier = 0
                                BarrierStackReduction = 0
                            }
                        Map [
                            Strong, strong
                            Weak, weak
                        ]
                }
            Run = false
        }

    /// Moves N random atoms per tick. Duplicates or omissions are
    /// possible.
    let handleTick model =
        if model.Run then
            let world =
                (model.World, [ 1 .. 25 ])
                    ||> Seq.fold (fun world _ ->
                        World.sweep
                            model.Parameters
                            model.Random
                            world)
            { model with World = world }
        else model

    /// Handlees temperature change.
    let handleSetTemperature kT model =
        { model with
            Parameters = {
                model.Parameters with
                    Temperature = kT } }

    /// Handles `run` flag change.
    let handleSetRun run model =
        { model with Run = run }

    /// Handles a message.
    let update msg model =
        match msg with
            | Tick -> handleTick model
            | SetTemperature kT -> handleSetTemperature kT model
            | SetRun run -> handleSetRun run model

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
