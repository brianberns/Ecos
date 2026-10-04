namespace Ecos

open System
open Elmish
open Avalonia.Threading
open Ecos.Experiment

type Model =
    {
        World : World
        Random : Random
        Temperature : float   // kT
    }

type Message =

    /// Timer tick.
    | Tick

    /// Sets temperature.
    | SetTemperature of float

module Model =

    let init () =
        let scenaioro = Scenario.createExperiment1 0
        {
            World = scenaioro.World
            Random = scenaioro.Random
            Temperature = 1.0
        }

    let updateTick model =
        let world =
            let n = 25 * model.World.AtomMap.Count
            (model.World, [ 1 .. n ])
                ||> Seq.fold (fun world _ ->
                    World.update
                        model.Temperature
                        model.Random
                        world
                        |> fst)
        { model with World = world }

    /// Moves N random atoms per tick. Duplicates or omissions are
    /// possible.
    let update msg model =
        match msg with
            | Tick -> updateTick model
            | SetTemperature kT -> { model with Temperature = kT }

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
