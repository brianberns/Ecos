namespace Ecos

open Elmish

open Avalonia
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Avalonia.Controls.ApplicationLifetimes

module Model =

    open Avalonia.FuncUI.DSL
    open Avalonia.Controls
    open Avalonia.Input
    open Avalonia.Layout

    let init () =
        [
            0, 0
            2, 0
            4, 1
        ]
            |> Seq.map (Point.ofTuple >> Atom.create)
            |> World.initialize
            |> World.addBond 0 1
            |> World.addBond 1 2    

    let update () world =
        world

    let view world dispatch =
        Window.create [
            Window.title "Ecos"
            Window.width 400.0
            Window.height 400.0
            Window.child (DockPanel.create [])
        ]

type MainWindow() as this =
    inherit HostWindow()
    do
        Elmish.Program.mkSimple Model.init Model.update Model.view
            |> Program.withHost this
            |> Program.withConsoleTrace
            |> Program.run

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add (FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Dark

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
