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
            AtomType.A, "#1F4E9E"
            AtomType.B, "#2E7D32"
            AtomType.C, "#B0006B"
            AtomType.D, "#B26A00"
        ]
            |> Seq.map (fun (typ, hex) ->
                typ, Media.Brush.Parse(hex))
            |> Map

    let private (~~)(ecosPt : Ecos.Point) =
        Avalonia.Point(ecosPt.X, ecosPt.Y)

    let renderAtoms world =
        [
            for i, atom in Map.toSeq world.AtomMap do
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

    let renderStrongBonds world =

        let strongBonds =
            world.StrongBondMap
                |> Map.toSeq
                |> Seq.collect (fun (idA, ids) ->
                    Seq.choose (fun idB ->
                        if idA < idB then
                            Some (
                                world.AtomMap[idA],
                                world.AtomMap[idB])
                        else None)
                        ids)

        [
            for a, b in strongBonds do
                Line.create [
                    Line.startPoint ~~a.Location
                    Line.endPoint ~~b.Location
                    Line.stroke strongBondBrush
                    Line.strokeThickness strongBondThickness
                ] :> IView
        ]

    let renderWeakBonds world =

        let weakBonds =
            world.WeakBondMap
                |> Map.toSeq
                |> Seq.choose (fun (idA, idB) ->
                    if idA < idB then
                        Some (
                            world.AtomMap[idA],
                            world.AtomMap[idB])
                    else None)

        [
            for a, b in weakBonds do
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
                yield! renderStrongBonds world
                yield! renderAtoms world   // draw atoms over strong bonds
                yield! renderWeakBonds world
            ]
        ]

    let renderTemperature kT dispatch =
        DockPanel.create [
            DockPanel.dock Dock.Bottom
            DockPanel.children [
                TextBlock.create [
                    TextBlock.dock Dock.Left
                    TextBlock.text $"Temperature: %0.3f{kT}"
                    TextBlock.foreground Colors.Black
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.padding 10
                ]
                Slider.create [
                    Slider.dock Dock.Left
                    Slider.minimum 0.0
                    Slider.maximum 5.0
                    Slider.value kT
                    Slider.onValueChanged (fun value ->
                        SetTemperature value |> dispatch)
                ]
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
                        renderTemperature model.Temperature dispatch
                        renderWorld model.World   // last child fills
                    ]
                ])
        ]
