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
            AtomType.W, "#1F4E9E"   // blue
            AtomType.X, "#2E7D32"   // green
            AtomType.Y, "#B0006B"   // red
            AtomType.Z, "#B26A00"   // orange
            AtomType.P, "#000000"   // black
            AtomType.Q, "#808080"   // gray
        ]
            |> Seq.map (fun (typ, hex) ->
                typ, Media.Brush.Parse(hex))
            |> Map

    let private (~~)(ecosPt : Ecos.Point) =
        Avalonia.Point(ecosPt.X, ecosPt.Y)

    let renderWalls (world : World) =
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
        let templateIds = set model.Run.Template
        [
            for atom in model.Run.World.Atoms do
                Ellipse.create [
                    Ellipse.width 2
                    Ellipse.height 2
                    let pt = atom.Location - Point.create (1, 1)   // every atom is 2x2 centered on its location
                    Ellipse.left pt.X
                    Ellipse.top pt.Y
                    if templateIds.Contains(atom.Id) then
                        Ellipse.fill brushMap[atom.Type]
                    else
                        Ellipse.stroke brushMap[atom.Type]
                        Ellipse.strokeThickness 0.4
                    Ellipse.useLayoutRounding false   // don't snap to pixels before scaling
                ] :> IView
        ]

    let private getBonds bondType (world : World) =
        world.BondTypeMap[bondType]
            |> Map.toSeq
            |> Seq.collect (fun (idA, ids) ->
                Seq.choose (fun idB ->
                    if idA < idB then
                        Some (
                            world.Atoms[idA],
                            world.Atoms[idB])
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
                        yield! renderWalls model.Run.World
                        yield! renderAtoms model
                        yield! renderStrongBonds model.Run.World
                        yield! renderWeakBonds model.Run.World
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

    let renderSweepCount sweepCount =
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                renderLabel "Sweeps:" labelColumnWidth
                renderLabel $"{sweepCount:N0}" labelColumnWidth
            ]
        ]

    let renderUpdateRate frameRate =
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                renderLabel "Update rate:" labelColumnWidth
                renderLabel $"{frameRate:F1}" labelColumnWidth
            ]
        ]

    /// Names of the scenario groups, and of the scenarios in each. These
    /// are built once, so that the combo boxes see the same items on
    /// every render.
    let private groupNames =
        Registry.groups |> Array.map _.Name

    let private scenarioNames =
        Registry.groups
            |> Array.map (fun group ->
                group.Scenarios |> Array.map _.Name)

    /// A combo box of the given names. Its handler is attached only
    /// once, so it must not capture anything that changes; the model
    /// ignores a selection that is already current.
    let private renderComboBox
        key (names : string[]) (index : int) (onSelect : int -> unit) =
        ComboBox.create [
            ComboBox.dataItems names
            ComboBox.selectedIndex index
            ComboBox.width 260
            ComboBox.margin margin
            ComboBox.onSelectedIndexChanged (fun i ->
                if i >= 0 then onSelect i)   // -1 means the selection was cleared
        ]
            |> View.withKey key   // a new key gives a new box, rather than one whose items change under it

    let renderScenario (model : Model) dispatch =
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.children [
                        renderLabel "Group:" labelColumnWidth
                        renderComboBox "groups"
                            groupNames
                            model.GroupIndex
                            (SelectGroup >> dispatch)
                    ]
                ]
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.children [
                        renderLabel "Scenario:" labelColumnWidth
                        renderComboBox $"scenarios {model.GroupIndex}"
                            scenarioNames[model.GroupIndex]
                            model.ScenarioIndex
                            (SelectScenario >> dispatch)
                    ]
                ]
            ]
        ]

    let renderManual isManual dispatch =
        CheckBox.create [
            CheckBox.content "Manual"
            CheckBox.isChecked (isManual : bool)
            CheckBox.margin margin
            CheckBox.foreground Colors.Black
            CheckBox.onIsCheckedChanged (fun args ->
                let box = args.Source :?> CheckBox
                if box.IsChecked.HasValue then
                    box.IsChecked.Value
                        |> SetManual
                        |> dispatch)
        ]

    let renderTemperature (model : Model) dispatch =
        let kT = model.Temperature
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                renderLabel "Temperature:" labelColumnWidth
                if model.IsManual then
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
                renderManual model.IsManual dispatch
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

    let renderSeed (seed : int) dispatch =
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                renderLabel "Seed:" labelColumnWidth
                NumericUpDown.create [
                    NumericUpDown.increment 1
                    NumericUpDown.value seed
                    NumericUpDown.formatString "F0"
                    NumericUpDown.width 120
                    NumericUpDown.margin margin
                    NumericUpDown.horizontalContentAlignment
                        HorizontalAlignment.Right
                    NumericUpDown.onValueChanged (fun value ->
                        if value.HasValue then
                            int value.Value
                                |> SetSeed
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
                renderRun model.IsRunning dispatch
                renderReset dispatch
            ]
        ]

    let renderControlPanel (model : Model) dispatch =
        DockPanel.create [
            DockPanel.dock Dock.Right
            DockPanel.background Colors.LightGray
            DockPanel.lastChildFill false
            DockPanel.children [
                renderScenario model dispatch
                renderSweepCount model.Run.NumSweeps
                renderUpdateRate model.UpdateRate
                renderTemperature model dispatch
                renderSpeed model.Speed dispatch
                renderSeed model.Seed dispatch
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
