namespace Ecos.Desktop

open System

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Threading

open Ecos.Engine

module Canvas =

    /// Number of engine time steps per frame.
    let stepsPerFrame = 1

    /// Extent of the world.
    let extentMin, extentMax =
        let width = 40.0
        let height = 600.0 * width / 800.0
        let pt = (Point.create width height) / 2.0
        -pt, pt

type WorldCanvas() as this =
    inherit Control()

        // random number generator
    let random =
        let seed = DateTime.Now.Millisecond
        Random(seed)

    let mutable world =
        Ecos.Desktop.World.create
            random Canvas.extentMin Canvas.extentMax 200

    let timer =
        DispatcherTimer(
            TimeSpan.FromMilliseconds(16.0),
            DispatcherPriority.Render,
            EventHandler(fun _ _ ->
                world <-
                    (world, [1 .. Canvas.stepsPerFrame])
                        ||> Seq.fold (fun world _ ->
                            World.step world)
                this.InvalidateVisual()))

    do timer.Start()

    member _.Reset() = ()

    override _.Render(ctx) =
        base.Render(ctx)
        let group = TransformGroup()
        let s =
            this.Bounds.Width
                / (Canvas.extentMax.X - Canvas.extentMin.X)
        ScaleTransform(s, s)
            |> group.Children.Add
        TranslateTransform(
            this.Bounds.Width / 2.0,
            this.Bounds.Height / 2.0)
            |> group.Children.Add
        ctx.PushTransform(group.Value) |> ignore
        World.draw ctx world

type MainWindow() as this =
    inherit Window()

    let canvas = WorldCanvas()

    let resetButton =
        Button(
            Content = "Reset",
            Margin = Thickness(5.0),
            Padding = Thickness(10.0),
            Background = Brushes.LightGray,
            BorderBrush = Brushes.Black,
            BorderThickness = Thickness(1.0),
            FontWeight = FontWeight.Bold,
            HorizontalAlignment = HorizontalAlignment.Left)

    let dock = DockPanel()

    do
        this.Title <- "Ecos"
        this.Width <- 800.0
        this.Height <- 600.0

        DockPanel.SetDock(resetButton, Dock.Top)
        dock.Children.Add(resetButton) |> ignore
        dock.Children.Add(canvas) |> ignore

        this.Content <- dock

        resetButton.Click.Add(fun _ -> canvas.Reset())

type App() =
    inherit Application()
    override _.Initialize() = ()
    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop ->
            desktop.MainWindow <- MainWindow()
        | _ -> ()
        base.OnFrameworkInitializationCompleted()

module Program =

    [<EntryPoint>]
    let main argv =
        AppBuilder.Configure<App>()
            .UsePlatformDetect()
            .LogToTrace()
            .StartWithClassicDesktopLifetime(argv)
