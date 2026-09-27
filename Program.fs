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

    let init () =
        let atoms =
            [
                0, 0
                2, 0
                4, 1
            ]
                |> Seq.map (Point.create >> Atom.create)
        (atoms, World.empty)
            ||> Seq.foldBack World.addAtom
            |> World.addBond 0 1
            |> World.addBond 1 2    

    let rng = System.Random(0)

    let update Tick world =
        World.update rng world

    let timer onTick =
        let timerSub (dispatch: Message -> unit) =
            let timer = new DispatcherTimer(
                TimeSpan.FromMilliseconds(10.0),
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
