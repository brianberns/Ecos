namespace Ecos.Builder

open Elmish

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Input
open Avalonia.Media
open Avalonia.Themes.Fluent

open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts

/// Main window: Ctrl+R restarts the run, and Ctrl+S saves the spec.
type MainWindow() as this =
    inherit HostWindow()
    do
        this.Title <- "Ecos Scenario Builder"
        this.Width <- 1600.0
        this.Height <- 1000.0
        this.Background <- Brushes.White
        this.WindowStartupLocation <- WindowStartupLocation.CenterScreen
        let keys (_ : Model) : Sub<Message> =
            let subscribe dispatch =
                let handler =
                    System.EventHandler<KeyEventArgs>(fun _ args ->
                        if args.KeyModifiers = KeyModifiers.Control then
                            match args.Key with
                                | Key.R -> dispatch Restart; args.Handled <- true
                                | Key.S -> dispatch Save; args.Handled <- true
                                | _ -> ())
                this.KeyDown.AddHandler(handler)
                { new System.IDisposable with
                    member _.Dispose() = this.KeyDown.RemoveHandler(handler) }
            [ [ "keys" ], subscribe ]
        Program.mkProgram Model.init Model.update View.render
            |> Program.withSubscription (fun model -> Model.subscribe this model @ keys model)
            |> Program.withHost this
            |> Program.run

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add (FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Light

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
            | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
                desktopLifetime.MainWindow <- MainWindow()
            | _ -> ()

module Program =

    [<EntryPoint>]
    let main (args : string[]) =
        System.Diagnostics.Process.GetCurrentProcess().PriorityClass <- System.Diagnostics.ProcessPriorityClass.BelowNormal
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)
