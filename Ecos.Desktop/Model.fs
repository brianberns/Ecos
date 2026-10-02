namespace Ecos

open System
open Elmish
open Avalonia.Threading

type Model =
    {
        World : World
        Temperature : float   // kT
        Random : Random
    }

type Message =

    /// Timer tick.
    | Tick

    /// Sets temperature.
    | SetTemperature of float

module Model =

    let private addVerticalWall x yStart yEnd world =
        assert(x % 2 = 0)
        assert(yStart % 2 = 0)
        assert(yEnd % 2 = 0)
        (world, [ yStart .. 2.. yEnd ])
            ||> Seq.fold (fun world y ->
                let loc = Point.create (x, y)
                World.addWall loc world)

    let private addHorizontalWall xStart xEnd y world =
        assert(xStart % 2 = 0)
        assert(xEnd % 2 = 0)
        assert(y % 2 = 0)
        (world, [ xStart .. 2 .. xEnd ])
            ||> Seq.fold (fun world x ->
                let loc = Point.create (x, y)
                World.addWall loc world)

    let private addAtom
        (rng : Random) (atomTypes : _[]) x y id bond world =
        assert(x % 2 = 0)
        assert(y % 2 = 0)
        let atomType =
            atomTypes[rng.Next(atomTypes.Length)]
        let loc = Point.create (x, y)
        let atom = Atom.create atomType loc
        let world = World.addAtom atom world
        if bond then
            World.addStrongBond (id - 1) id world
        else world

    let init () =

        let rng = Random(0)
        let n = 40
        let dist = 6

        let world =
            let width = 2 * n + 6 * dist
            let height = 2 * n
            let height' = height - 2 * 2
            World.empty
                |> addHorizontalWall -(width/2) (width/2) -(height/2)
                |> addHorizontalWall -(width/2) (width/2) (height/2)
                |> addVerticalWall -(width/2) -(height'/2) (height'/2)
                |> addVerticalWall (width/2) -(height'/2) (height'/2)

            // first strand
        let world =
            (world, [ 0 .. n - 1 ])
                ||> Seq.fold (fun world i ->
                    let x = 2 * i - n
                    let atomTypes = [| AtomType.W; AtomType.Y |]
                    addAtom rng atomTypes x -dist i (i > 0) world)

            // second strand
        let world =
            (world, [ 0 .. n - 1 ])
                ||> Seq.fold (fun world i ->
                    let x = 2 * i - n
                    let atomTypes = [| AtomType.X; AtomType.Z |]
                    addAtom rng atomTypes x dist (n + i) (i > 0) world)

        {
            World = world
            Temperature = 1.0
            Random = rng
        }

    let updateTick model =
        let world =
            let n = 25 * model.World.AtomMap.Count
            (model.World, [ 1 .. n ])
                ||> Seq.fold (fun world _ ->
                    World.update
                        model.Temperature
                        model.Random
                        world)
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
