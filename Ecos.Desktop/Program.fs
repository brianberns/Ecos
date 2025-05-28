namespace Ecos.Desktop

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Threading
open System

type BouncingBallCanvas() as this =
    inherit Control()

    let radius = 20.0
    let mutable x = 50.0
    let mutable y = 50.0
    let mutable vx = 3.0
    let mutable vy = 2.5

    let resetPosition () =
        x <- 50.0
        y <- 50.0
        vx <- 3.0
        vy <- 2.5

    let timer = DispatcherTimer(TimeSpan.FromMilliseconds(16.0), DispatcherPriority.Render, EventHandler(fun _ _ ->
        let bounds = this.Bounds

        x <- x + vx
        y <- y + vy

        if x <= 0.0 || x + radius * 2.0 >= bounds.Width then vx <- -vx
        if y <= 0.0 || y + radius * 2.0 >= bounds.Height then vy <- -vy

        this.InvalidateVisual()
    ))

    do
        timer.Start()

    member _.Reset() = resetPosition()

    override this.Render(context: DrawingContext) =
        base.Render(context)
        let brush = SolidColorBrush(Color.Parse("#FF00FF")) :> IBrush
        let pen = Pen(Brushes.Black)
        context.DrawEllipse(brush, pen, Point(x + radius, y + radius), radius, radius)

type BouncingBallWindow() as this =
    inherit Window()

    let canvas = BouncingBallCanvas()

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
        this.Title <- "Bouncing Ball (Custom Render)"
        this.Width <- 400.0
        this.Height <- 300.0

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
            desktop.MainWindow <- BouncingBallWindow()
        | _ -> ()
        base.OnFrameworkInitializationCompleted()

module Program =

    [<EntryPoint>]
    let main argv =
        AppBuilder.Configure<App>()
            .UsePlatformDetect()
            .LogToTrace()
            .StartWithClassicDesktopLifetime(argv)
