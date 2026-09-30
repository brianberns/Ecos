namespace Ecos

open System

open Elmish

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.Threading

open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts

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
            ||> Seq.fold (fun world _ -> World.update rng world)

    let timer onTick =
        let timerSub (dispatch: Message -> unit) =
            let timer = new DispatcherTimer(
                TimeSpan.FromSeconds(1.0 / 24.0),
                DispatcherPriority.Normal,
                EventHandler(fun _ _ ->
                    dispatch Tick))
            timer.Start()
            { new IDisposable with
                member _.Dispose() = timer.Stop() }
        timerSub

    let subscribe world =
        [ ["timer"], timer Tick ]

type MainWindow() as this =
    inherit HostWindow()
    do
        Elmish.Program.mkSimple Model.init Model.update View.render
            |> Program.withSubscription Model.subscribe
            |> Program.withHost this
            // |> Program.withConsoleTrace
            |> Program.run

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add (FluentTheme())

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            let mainWindow = MainWindow()
            desktopLifetime.MainWindow <- mainWindow
        | _ -> ()

module Program =

    [<EntryPoint>]
    let main(args: string[]) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)
