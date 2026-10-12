namespace Ecos

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Controls.Shapes
open Avalonia.Layout
open Avalonia.Media

open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

open Ecos.Experiment

module View =

    let private brushMap =
        [
            AtomType.W, "#1F4E9E"
            AtomType.X, "#2E7D32"
            AtomType.Y, "#B0006B"
            AtomType.Z, "#B26A00"
            AtomType.P, "#000000"
            AtomType.Q, "#808080"
        ]
            |> Seq.map (fun (typ, hex) ->
                typ, Media.Brush.Parse(hex))
            |> Map

    let private (~~)(ecosPt : Ecos.Point) =
        Avalonia.Point(ecosPt.X, ecosPt.Y)

    let renderWalls world =
        [
            for loc in world.Walls do
                Rectangle.create [
                    Rectangle.width 2
                    Rectangle.height 2
                    let pt = loc - Point.create (1, 1)
                    Rectangle.left pt.X
                    Rectangle.top pt.Y
                    Rectangle.fill Brushes.Gray
                    Rectangle.useLayoutRounding false   // don't snap to pixels before scaling
                ] :> IView
        ]

    let renderAtoms model =
        let templateIds = set model.Scenario.Template
        [
            for id, atom in Map.toSeq model.World.AtomMap do
                Ellipse.create [
                    Ellipse.width 2
                    Ellipse.height 2
                    let pt = atom.Location - Point.create (1, 1)   // every atom is 2x2 centered on its location
                    Ellipse.left pt.X
                    Ellipse.top pt.Y
                    if templateIds.Contains(id) then
                        Ellipse.fill brushMap[atom.Type]
                    else
                        Ellipse.stroke brushMap[atom.Type]
                        Ellipse.strokeThickness 0.4
                    Ellipse.useLayoutRounding false   // don't snap to pixels before scaling
                ] :> IView
        ]

    let private getBonds bondType world =
        world.BondTypeMap[bondType]
            |> Map.toSeq
            |> Seq.collect (fun (idA, ids) ->
                Seq.choose (fun idB ->
                    if idA < idB then
                        Some (
                            world.AtomMap[idA],
                            world.AtomMap[idB])
                    else None)
                    ids)

    let renderBonds
        bondType (brush : IImmutableBrush) thickness world =
        [
            for a, b in getBonds bondType world do
                Line.create [
                    Line.startPoint ~~a.Location
                    Line.endPoint ~~b.Location
                    Line.stroke brush
                    Line.strokeThickness thickness
                ] :> IView
        ]

    let renderStrongBonds world =
        renderBonds Strong Brushes.Black 0.2 world

    let renderWeakBonds world =
        renderBonds Weak Brushes.HotPink 0.4 world

    let renderWorld (model : Model) =
        let scale = 6.0
        let background =
            if model.Temperature < 1.0 then Brush.Parse("#CBCDF2")
            else Brush.Parse("#F2CBCB")
        Border.create [
            Border.clipToBounds true   // keep drawing inside the available space
            Border.background background
            Border.child (
                Canvas.create [
                    Canvas.width 0.0
                    Canvas.height 0.0
                    Canvas.horizontalAlignment HorizontalAlignment.Center   // world origin at the center
                    Canvas.verticalAlignment VerticalAlignment.Center
                    Canvas.renderTransform (ScaleTransform(scale, scale))
                    Canvas.renderTransformOrigin RelativePoint.TopLeft
                    Canvas.children [
                        yield! renderWalls model.World
                        yield! renderAtoms model
                        yield! renderStrongBonds model.World
                        yield! renderWeakBonds model.World
                    ]
                ])
        ]

    let margin = 10

    let renderLabel text width =
        TextBlock.create [
            TextBlock.text text
            TextBlock.width width
            TextBlock.foreground Colors.Black
            TextBlock.verticalAlignment VerticalAlignment.Center
            TextBlock.margin margin
        ]

    let labelColumnWidth = 100

    let renderTemperature (model : Model) dispatch =
        let kT = model.Temperature
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                renderLabel "Temperature:" labelColumnWidth
                if model.TemperatureType.IsAdjustable then
                    NumericUpDown.create [
                        NumericUpDown.minimum 0
                        NumericUpDown.maximum 5
                        NumericUpDown.increment 0.05m
                        NumericUpDown.value (decimal kT)
                        NumericUpDown.width 120
                        NumericUpDown.margin margin
                        NumericUpDown.horizontalContentAlignment
                            HorizontalAlignment.Right
                        NumericUpDown.onValueChanged (fun value ->
                            if value.HasValue then
                                float value.Value
                                    |> SetTemperature
                                    |> dispatch)
                    ]
                else
                    renderLabel $"%0.2f{kT}" labelColumnWidth
            ]
        ]

    let renderSweepCount sweepCount =
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                renderLabel "Sweeps:" labelColumnWidth
                renderLabel $"{sweepCount:N0}" labelColumnWidth
            ]
        ]

    let renderSpeed (speed : int) dispatch =
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                renderLabel "Speed:" labelColumnWidth
                NumericUpDown.create [
                    NumericUpDown.minimum 1
                    NumericUpDown.maximum 1000
                    NumericUpDown.increment 1
                    NumericUpDown.value speed
                    NumericUpDown.formatString "F0"
                    NumericUpDown.width 120
                    NumericUpDown.margin margin
                    NumericUpDown.horizontalContentAlignment
                        HorizontalAlignment.Right
                    NumericUpDown.onValueChanged (fun value ->
                        if value.HasValue then
                            int value.Value
                                |> SetSpeed
                                |> dispatch)
                ]
            ]
        ]

    let renderRun run dispatch =
        ToggleButton.create [
            ToggleButton.content "Run"
            ToggleButton.isChecked (run : bool)
            ToggleButton.width 100
            ToggleButton.margin margin
            ToggleButton.foreground Colors.Black
            ToggleButton.background Colors.DarkGray
            ToggleButton.horizontalContentAlignment
                HorizontalAlignment.Center
            ToggleButton.onIsCheckedChanged (fun args ->
                let btn = args.Source :?> ToggleButton
                if btn.IsChecked.HasValue then
                    btn.IsChecked.Value
                        |> SetRun
                        |> dispatch)
        ]

    let renderReset dispatch =
        Button.create [
            Button.content "Reset"
            Button.width 100
            Button.margin margin
            Button.foreground Colors.Black
            Button.background Colors.DarkGray
            Button.horizontalContentAlignment
                HorizontalAlignment.Center
            Button.onClick (fun _ ->
                dispatch Reset)
        ]

    let renderButtons model dispatch =
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                renderRun model.Run dispatch
                renderReset dispatch
            ]
        ]

    let renderControlPanel (model : Model) dispatch =
        DockPanel.create [
            DockPanel.dock Dock.Right
            DockPanel.background Colors.LightGray
            DockPanel.lastChildFill false
            DockPanel.children [
                renderSweepCount model.SweepCount
                renderTemperature model dispatch
                renderSpeed model.Speed dispatch
                renderButtons model dispatch
            ]
        ]

    let render model dispatch =
        Window.create [
            Window.title "Ecos"
            Window.width 1300
            Window.height 1000
            Window.background Brushes.WhiteSmoke
            Window.windowStartupLocation WindowStartupLocation.CenterScreen
            Window.child (
                DockPanel.create [
                    DockPanel.children [
                        renderControlPanel model dispatch
                        renderWorld model   // last child fills
                    ]
                ])
        ]
