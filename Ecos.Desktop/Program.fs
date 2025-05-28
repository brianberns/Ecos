namespace Ecos.Desktop

open System
open System.Diagnostics

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Themes.Simple
open Avalonia.Threading

open Ecos.Engine

/// Control that displays the world.
type WorldView() as this =
    inherit Control()

    /// Number of engine time steps per frame.
    let stepsPerFrame = 1

    /// Extent of the world.
    let extentMin, extentMax =
        let pt = (Point.create 40.0 30.0) / 2.0
        -pt, pt

    /// Random number generator
    let random =
        let seed = DateTime.Now.Millisecond
        Random(seed)

    let createWorld () =
        Ecos.Desktop.World.create
            random extentMin extentMax 200

    let mutable world = createWorld ()

    let timer =
        let stopwatch = Stopwatch.StartNew()
        let check = 100
        let mutable prev = stopwatch.ElapsedMilliseconds
        let mutable iFrame = 0
        DispatcherTimer(
            TimeSpan.FromMilliseconds(10.0),
            DispatcherPriority.Render,
            EventHandler(fun _ _ ->

                iFrame <- iFrame + 1
                if iFrame % check = 0 then
                    let cur = stopwatch.ElapsedMilliseconds
                    $"Frames/sec: %0.3f{1000.0 * float check / float (cur - prev)}"
                        |> Trace.WriteLine
                    prev <- cur

                world <-
                    (world, [1 .. stepsPerFrame])
                        ||> Seq.fold (fun world _ ->
                            World.step world)

                this.InvalidateVisual()))

    do timer.Start()

    member _.Reset() =
        world <- createWorld ()

    override _.Render(ctx) =
        base.Render(ctx)

        let group = TransformGroup()
        let s =
            this.Bounds.Width / (extentMax.X - extentMin.X)
        group.Children.AddRange [
            ScaleTransform(s, s)
            TranslateTransform(
                this.Bounds.Width / 2.0,
                this.Bounds.Height / 2.0) ]

        use _ = ctx.PushTransform(group.Value)
        World.draw ctx world

/// Main window.
type MainWindow() as this =
    inherit Window(
        Title = "Ecos",
        Width = 820.0,
        Height = 660.0)

    let worldView = WorldView()

    let border =
        Border(
            Width = 800.0,
            Height = 600.0,
            Background = Brushes.White,
            Child = worldView,
            Margin = Thickness(5.0),
            HorizontalAlignment = HorizontalAlignment.Left)

    let resetBtn =
        let btn =
            Button(
                Content = "Reset",
                Margin = Thickness(5.0),
                HorizontalAlignment = HorizontalAlignment.Left)
        btn.Click.Add(fun _ -> worldView.Reset())
        btn

    let panel =
        let panel = StackPanel()
        panel.Children.AddRange [ border; resetBtn ]
        panel

    do this.Content <- panel

/// Application.
type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(SimpleTheme())

    override this.OnFrameworkInitializationCompleted() =
        let desktop =
            this.ApplicationLifetime
                :?> IClassicDesktopStyleApplicationLifetime
        desktop.MainWindow <- MainWindow()
        base.OnFrameworkInitializationCompleted()

module Program =

    [<EntryPoint>]
    let main argv =
        AppBuilder.Configure<App>()
            .UsePlatformDetect()
            .LogToTrace()
            .StartWithClassicDesktopLifetime(argv)
