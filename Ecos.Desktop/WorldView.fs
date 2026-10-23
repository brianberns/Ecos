namespace Ecos

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Controls.Shapes
open Avalonia.Layout
open Avalonia.Media

open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

open Wacton.Unicolour

open Ecos.Experiment

module WorldView =

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

    let renderRectangle location size (color : Color) =
        assert(size % 2 = 0)
        let half = size / 2
        Rectangle.create [
            Rectangle.width size
            Rectangle.height size
            let pt = location - Point.create(half, half)
            Rectangle.left pt.X
            Rectangle.top pt.Y
            Rectangle.fill color
            Rectangle.useLayoutRounding false   // don't snap to pixels before scaling
        ] :> IView

    let private getGradientColor colorA colorB (distance : float) =
        let toUnicolour (color : Color) =
            Unicolour(
                ColourSpace.Rgb255,
                float color.R,
                float color.G,
                float color.B)
        let ucolorA = toUnicolour colorA
        let ucolorB = toUnicolour colorB
        let ucolorMix = ucolorA.Mix(ucolorB, ColourSpace.Oklab, distance)
        let rgb = ucolorMix.Rgb.Byte255
        Color.FromRgb(byte rgb.R, byte rgb.G, byte rgb.B)

    let private getColor temp =
        let distance =
            (temp - float ControlPanel.minTemperature)
                / float (
                    ControlPanel.maxTemperature
                        - ControlPanel.minTemperature)
        getGradientColor Colors.LightBlue Colors.Red distance

    let private renderTemperature model =
        let spacing = 8
        let size = model.Run.World.Size
        [
            for x in 0 .. spacing .. size.X - spacing do
                for y in 0 .. spacing .. size.Y - spacing do
                    let loc =
                        Point.create (x + spacing / 2, y + spacing / 2)
                    loc
                        |> Run.getTemperature model.Run
                        |> getColor
                        |> renderRectangle loc spacing
        ]

    let private renderWalls (world : World) =
        [
            for loc in world.Walls do
                renderRectangle loc 2 Colors.Gray
        ]

    let private renderAtoms model =
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

    let private renderBonds
        bondType (brush : IImmutableBrush) thickness world =

        let drawLine start finish =
            Line.create [
                Line.startPoint ~~start
                Line.endPoint ~~finish
                Line.stroke brush
                Line.strokeThickness thickness
            ] :> IView

        [
            for a, b in getBonds bondType world do
                let d = World.getDisplacement a b world
                yield drawLine a.Location (a.Location + d)
                if a.Location + d <> b.Location then             // bond crosses an edge?
                    yield drawLine b.Location (b.Location - d)   // then draw its other half also
        ]

    let private renderStrongBonds world =
        renderBonds Strong Brushes.Black 0.2 world

    let private renderWeakBonds world =
        renderBonds Weak Brushes.HotPink 0.4 world

    let render (model : Model) =
        let world = model.Run.World
        Viewbox.create [
            Viewbox.stretch Stretch.Uniform   // scale the world to fit, keeping its shape
            Viewbox.child (
                Canvas.create [
                    Canvas.width (float world.Size.X)
                    Canvas.height (float world.Size.Y)
                    Canvas.clipToBounds true   // anything drawn past an edge is cut off there
                    Canvas.children [
                        yield! renderTemperature model
                        yield! renderWalls world
                        yield! renderAtoms model
                        yield! renderStrongBonds world
                        yield! renderWeakBonds world
                    ]
                ])
        ]
