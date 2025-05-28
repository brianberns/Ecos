namespace Ecos.Desktop

open System

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Threading

open Ecos.Engine

type WorldCanvas() as this =
    inherit Control()

    /// Number of engine time steps per frame.
    let stepsPerFrame = 1

    /// Extent of the world.
    let extentMin, extentMax =
        let width = 40.0
        let height = 600.0 * width / 800.0
        let pt = (Point.create width height) / 2.0
        -pt, pt

        // random number generator
    let random =
        let seed = DateTime.Now.Millisecond
        Random(seed)

    let mutable world =
        Ecos.Desktop.World.create
            random extentMin extentMax 200

    let timer =
        DispatcherTimer(
            TimeSpan.FromMilliseconds(16.0),
            DispatcherPriority.Render,
            EventHandler(fun _ _ ->
                world <-
                    (world, [1 .. stepsPerFrame])
                        ||> Seq.fold (fun world _ ->
                            World.step world)
                this.InvalidateVisual()))

    do timer.Start()

    member _.Reset() = ()

    override _.Render(ctx) =
        base.Render(ctx)
        let group = TransformGroup()
        let s =
            this.Bounds.Width / (extentMax.X - extentMin.X)
        ScaleTransform(s, s)
            |> group.Children.Add
        TranslateTransform(
            this.Bounds.Width / 2.0,
            this.Bounds.Height / 2.0)
            |> group.Children.Add
        use _ = ctx.PushTransform(group.Value)
        World.draw ctx world

type MainWindow() as this =
    inherit Window()

    let canvas =
        WorldCanvas()

    let border =
        Border(
            Width = 800.0,
            Height = 600.0,
            BorderBrush = Brushes.Black,
            BorderThickness = Thickness(1.0),
            Child = canvas,
            Margin = Thickness(5.0),
            HorizontalAlignment = HorizontalAlignment.Left)

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

    let panel = StackPanel()

    do
        this.Title <- "Ecos"
        this.Width <- 820.0
        this.Height <- 660.0

        panel.Children.Add(border) |> ignore
        panel.Children.Add(resetButton) |> ignore

        this.Content <- panel

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
