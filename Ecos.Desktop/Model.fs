namespace Ecos

open System
open Elmish
open Avalonia.Threading
open Ecos.Experiment

type Model =
    {
        World : World
        Random : Random
        Parameters : Parameters
    }

type Message =

    /// Timer tick.
    | Tick

    /// Sets temperature.
    | SetTemperature of float

module Model =

    let init () =
        let scenario = Scenario.twoStrands
        {
            World = scenario.World
            Random = scenario.Random
            Parameters =
                {
                    Temperature = 1.0
                    BondParameterMap =
                        let strong =
                            {
                                Energy = 12
                                Barrier = 6
                                BarrierStackReduction = 0
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
        }

    let updateTick model =
        let world =
            let n = 25 * model.World.AtomMap.Count
            (model.World, [ 1 .. n ])
                ||> Seq.fold (fun world _ ->
                    World.update
                        model.Parameters
                        model.Random
                        world
                        |> fst)
        { model with World = world }

    /// Moves N random atoms per tick. Duplicates or omissions are
    /// possible.
    let update msg model =
        match msg with
            | Tick -> updateTick model
            | SetTemperature kT ->
                { model with
                    Parameters = {
                        model.Parameters with
                            Temperature = kT } }

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
