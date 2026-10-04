namespace Ecos

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Layout
open Avalonia.Media

open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

module View =

    let private windowWidth = 1000
    let private windowHeight = 800
    let private scale = 8
    let private strongBondThickness = 0.2
    let private strongBondBrush = Brushes.Black
    let private weakBondThickness = 1.0
    let private weakBondBrush = Brushes.HotPink

    let private transformGroup =
        let group = TransformGroup()
        group.Children.Add(
            ScaleTransform(scale, scale))
        group.Children.Add(
            TranslateTransform(
                float windowWidth / 2.0,
                float windowHeight / 2.0))
        group

    let private brushMap =
        [
            AtomType.W, "#1F4E9E"
            AtomType.X, "#2E7D32"
            AtomType.Y, "#B0006B"
            AtomType.Z, "#B26A00"
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

    let renderAtoms world =
        [
            for atom in world.AtomMap.Values do
                Ellipse.create [
                    Ellipse.width 2
                    Ellipse.height 2
                    let pt = atom.Location - Point.create (1, 1)   // every atom is 2x2 centered on its location
                    Ellipse.left pt.X
                    Ellipse.top pt.Y
                    Ellipse.fill brushMap[atom.Type]
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

    let renderStrongBonds world =
        [
            for a, b in getBonds Strong world do
                Line.create [
                    Line.startPoint ~~a.Location
                    Line.endPoint ~~b.Location
                    Line.stroke strongBondBrush
                    Line.strokeThickness strongBondThickness
                ] :> IView
        ]

    let renderWeakBonds world =
        [
            for a, b in getBonds Weak world do
                Line.create [
                    Line.startPoint ~~a.Location
                    Line.endPoint ~~b.Location
                    Line.stroke weakBondBrush
                    Line.strokeThickness weakBondThickness
                ] :> IView
        ]

    let renderWorld world =
        Canvas.create [
            Canvas.renderTransform transformGroup
            Canvas.renderTransformOrigin RelativePoint.TopLeft   // use world origin as transform origin
            Canvas.children [
                yield! renderWalls world
                yield! renderStrongBonds world
                yield! renderAtoms world   // draw atoms over strong bonds
                yield! renderWeakBonds world
            ]
        ]

    let renderLabel text width =
        TextBlock.create [
            TextBlock.text text
            TextBlock.width width
            TextBlock.foreground Colors.Black
            TextBlock.verticalAlignment VerticalAlignment.Center
            TextBlock.padding 10
        ]

    let labelColumnWidth = 150

    let renderAtomCount world =
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                renderLabel "Atoms:" labelColumnWidth
                renderLabel $"{World.getAtomCount world}" 50
            ]
        ]

    let renderBondCount bondType world =
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                renderLabel $"{bondType} bonds:" labelColumnWidth
                renderLabel $"{World.getBondCount bondType world}" 50
            ]
        ]

    let renderTemperature (kT : float) dispatch =
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                renderLabel "Temperature:" labelColumnWidth
                NumericUpDown.create [
                    NumericUpDown.minimum 0
                    NumericUpDown.maximum 5
                    NumericUpDown.increment 0.1m
                    NumericUpDown.value (decimal kT)
                    NumericUpDown.width 110
                    NumericUpDown.padding 10
                    NumericUpDown.horizontalContentAlignment HorizontalAlignment.Right
                    NumericUpDown.onValueChanged (fun value ->
                        if value.HasValue then
                            float value.Value
                                |> SetTemperature
                                |> dispatch)
                ]
            ]
        ]

    let renderControlPanel model dispatch =
        DockPanel.create [
            DockPanel.dock Dock.Bottom
            DockPanel.background Colors.LightGray
            DockPanel.children [
                renderAtomCount model.World
                renderBondCount Strong model.World
                renderBondCount Weak model.World
                renderTemperature model.Temperature dispatch
            ]
        ]

    let render model dispatch =
        let world = model.World
        Window.create [
            Window.title "Ecos"
            Window.width windowWidth
            Window.height windowHeight
            Window.background Brushes.WhiteSmoke
            Window.child (
                DockPanel.create [
                    DockPanel.children [
                        renderControlPanel model dispatch
                        renderWorld model.World   // last child fills
                    ]
                ])
        ]
