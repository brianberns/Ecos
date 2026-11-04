namespace Ecos

open System

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
            AtomType.U, "#FFFFFF"
            AtomType.V, "#8F8F8F"
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

    /// Renders a rectangle with its top-left corner at the given location.
    let private renderRectangle
        (corner : Ecos.Point)
        (size : Ecos.Point)
        (color : Color) =
        Rectangle.create [
            Rectangle.left corner.X
            Rectangle.top corner.Y
            Rectangle.width size.X
            Rectangle.height size.Y
            Rectangle.fill color
            Rectangle.useLayoutRounding false   // don't snap to pixels before scaling
        ] :> IView

    let private toUnicolour (color : Color) =
        Unicolour(
            ColourSpace.Rgb255,
            float color.R,
            float color.G,
            float color.B)

    let private toColor (ucolor : Unicolour) =
        let rgb = ucolor.Rgb.Byte255
        Color.FromRgb(byte rgb.R, byte rgb.G, byte rgb.B)

    let private coldColor = toUnicolour Colors.LightBlue
    let private hotColor = toUnicolour Colors.Red
    let private hydrolysisColor = toUnicolour Colors.Yellow

    /// Hydrolysis rates shown, on a log scale: at or below the lowest,
    /// a positive rate gets the faintest tint; at or above the highest,
    /// the strongest.
    let private minHydRate, maxHydRate = 1e-6, 1e-3

    /// Faintest and strongest yellow tints, so that any hydrolysis
    /// shows, but the temperature still shows through.
    let private minHydTint, maxHydTint = 0.15, 0.6

    /// Amount of yellow to mix in for the given hydrolysis rate.
    let private getHydrolysisTint hydRate =
        if hydRate <= 0.0 then 0.0
        else
            let x =
                (log10 hydRate - log10 minHydRate)
                    / (log10 maxHydRate - log10 minHydRate)
            minHydTint + (maxHydTint - minHydTint) * Math.Clamp(x, 0.0, 1.0)

    let private getColor temp hydRate =
        let distance =
            (temp - float ControlPanel.minTemperature)
                / float (
                    ControlPanel.maxTemperature
                        - ControlPanel.minTemperature)
        coldColor
            .Mix(hotColor, ColourSpace.Oklab, distance)
            .Mix(hydrolysisColor, ColourSpace.Oklab, getHydrolysisTint hydRate)
            |> toColor

    let private renderEnvironment model =
        let spacing = 8
        let size = model.Run.World.Size
        [
            for x in 0 .. spacing .. size.X - 1 do
                for y in 0 .. spacing .. size.Y - 1 do

                    let width = min spacing (size.X - x)    // the last tile in a row or column may be smaller
                    let height = min spacing (size.Y - y)
                    let size = Point.create (width, height)
                    let corner = Point.create (x, y)
                    let center = Point.create (x + width / 2, y + height / 2)

                    let temp = Run.getTemperature model.Run center
                    let hydRate = Run.getHydrolysisRate model.Run
                    getColor temp hydRate
                        |> renderRectangle corner size
        ]

    let private renderWalls (world : World) =
        [
            for loc in world.Walls do
                let corner = loc - Point.create (1, 1)   // every wall is 2x2 centered on its location
                let size = Point.create(2, 2)
                renderRectangle corner size Colors.Gray
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
                        yield! renderEnvironment model
                        yield! renderWalls world
                        yield! renderAtoms model
                        yield! renderStrongBonds world
                        yield! renderWeakBonds world
                    ]
                ])
        ]
