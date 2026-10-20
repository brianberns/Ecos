namespace Ecos

open System

open Elmish

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Rendering
open Avalonia.Themes.Fluent
open Avalonia.Threading

open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts

type MainWindow() as this =
    inherit HostWindow()
    do
        this.RendererDiagnostics.DebugOverlays <-
            RendererDebugOverlays.Fps
        Elmish.Program.mkSimple Model.init Model.update View.render
            |> Program.withSubscription (Model.subscribe this)
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
