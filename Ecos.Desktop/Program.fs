open System

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Controls.Shapes
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Threading

type BouncingBallWindow() as this =
    inherit Window()

    let mutable x = 50.0
    let mutable y = 50.0
    let mutable vx = 3.0
    let mutable vy = 2.5
    let radius = 20.0

    let canvas = Canvas(Background = Brushes.White)

    let ball = Ellipse(Width = radius * 2.0, Height = radius * 2.0, Fill = Brushes.Red)

    let resetPosition () =
        x <- 50.0
        y <- 50.0
        vx <- 3.0
        vy <- 2.5
        Canvas.SetLeft(ball, x)
        Canvas.SetTop(ball, y)

    let resetButton =
        let btn = Button(Content = "Reset")
        btn.Background <- Brushes.LightGray
        btn.Padding <- Thickness(10.0)
        btn.Margin <- Thickness(5.0)
        btn.BorderBrush <- Brushes.Black
        btn.BorderThickness <- Thickness(1.0)
        btn.FontWeight <- FontWeight.Bold
        btn.HorizontalAlignment <- HorizontalAlignment.Left
        btn

    let border =
        let b = Border()
        b.BorderBrush <- Brushes.Black
        b.BorderThickness <- Thickness(2.0)
        b.Margin <- Thickness(10.0)
        b.Child <- canvas
        b

    let dock = DockPanel()

    do
        this.Content <- dock
        this.Width <- 400.0
        this.Height <- 300.0
        this.Title <- "Bouncing Ball (F# + Avalonia)"

        DockPanel.SetDock(resetButton, Dock.Top)
        dock.Children.Add(resetButton) |> ignore
        dock.Children.Add(border) |> ignore

        canvas.Children.Add(ball) |> ignore
        resetPosition()

        let timer = DispatcherTimer(TimeSpan.FromMilliseconds(16.0), DispatcherPriority.Render, EventHandler(fun _ _ ->
            x <- x + vx
            y <- y + vy

            // Use canvas size instead of window bounds
            let canvasWidth = canvas.Bounds.Width
            let canvasHeight = canvas.Bounds.Height

            if x <= 0.0 || x + radius * 2.0 >= canvasWidth then vx <- -vx
            if y <= 0.0 || y + radius * 2.0 >= canvasHeight then vy <- -vy

            Canvas.SetLeft(ball, x)
            Canvas.SetTop(ball, y)
        ))

        timer.Start()
        resetButton.Click.Add(fun _ -> resetPosition())

type App() =
    inherit Application()

    override this.Initialize() =
        ()

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- BouncingBallWindow()
        | _ -> ()

[<EntryPoint>]
let main argv =
    AppBuilder
        .Configure<App>()
        .UsePlatformDetect()
        .LogToTrace()
        .StartWithClassicDesktopLifetime(argv)
