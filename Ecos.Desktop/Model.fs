namespace Ecos

open System
open Elmish
open Avalonia.Threading

type Message = Tick

module Model =

    let addAtom (x, y) world =
        let atom = Atom.create Hydrophobic (Point.create (x, y))
        World.addAtom atom world

    let private addMolecule y world =
        let n = world.AtomMap.Count
        world
            |> addAtom (-4, y)       // 0
            |> addAtom (-2, y)       // 1
            |> World.addBond (n + 0) (n + 1)
            |> addAtom (0, y)        // 2
            |> World.addBond (n + 1) (n + 2)
            |> addAtom (2, y)        // 3
            |> World.addBond (n + 2) (n + 3)
            |> addAtom (0, y + 2)    // 4
            |> World.addBond (n + 2) (n + 4)

    let init () =
        let n = 8
        ([ -4 * n .. 4 .. 4 * n ], World.empty)
            ||> Seq.foldBack addMolecule

    let rng = System.Random(0)

    /// Moves N random atoms per tick. Duplicates or omissions are
    /// possible.
    let update Tick world =
        (world, [ 1 .. world.AtomMap.Count ])
            ||> Seq.fold (fun world _ ->
                World.update 1.0 rng world)

    /// Subscribable timer.
    let timer Tick : Subscribe<_> =
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
        [ ["timer"], timer Tick ]
