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
    open Avalonia.Controls.Shapes
    open Avalonia.FuncUI.DSL
    open Avalonia.Media

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

    let private windowWidth = 400.0
    let private windowHeight = 400.0
    let private scale = 30.0

    let transformGroup =
        let group = TransformGroup()
        group.Children.Add(
            ScaleTransform(scale, scale))
        group.Children.Add(
            TranslateTransform(
                windowWidth / 2.0,
                windowHeight / 2.0))
        group

    let brushes =
        [
            Brushes.Red
            Brushes.Blue
            Brushes.Green
        ]

    let private (~~)(ecosPt : Ecos.Point) =
        Avalonia.Point(ecosPt.X, ecosPt.Y)

    let view world dispatch =
        Window.create [
            Window.title "Ecos"
            Window.width windowWidth
            Window.height windowHeight
            Window.child (
                Canvas.create [
                    Canvas.renderTransform transformGroup
                    Canvas.renderTransformOrigin RelativePoint.TopLeft   // use world origin as transform origin
                    Canvas.children [

                        for i, atom in Seq.indexed world.Atoms do
                            Rectangle.create [
                                Rectangle.width 2.0
                                Rectangle.height 2.0
                                let pt = atom.Location - Point.create 1 1
                                Rectangle.left pt.X
                                Rectangle.top pt.Y
                                Rectangle.fill brushes[i % brushes.Length]
                                Rectangle.useLayoutRounding false   // don't snap to pixels before scaling
                            ]

                        for a, b in World.getBonds world do
                            Line.create [
                                Line.startPoint ~~a.Location
                                Line.endPoint ~~b.Location
                                Line.stroke Brushes.Black
                                Line.strokeThickness (2.0 / scale)   // undo transform
                            ]
                    ]
                ]
            )
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
