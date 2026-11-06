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
            AtomType.H, "#A0A0A0"   // gray
            AtomType.W, "#1F4E9E"   // blue
            AtomType.X, "#2E7D32"   // green
            AtomType.Y, "#B0006B"   // red
            AtomType.Z, "#B26A00"   // orange
            AtomType.P, "#000000"   // black
            AtomType.Q, "#707070"   // gray
        ]
            |> Seq.map (fun (typ, hex) ->
                typ, Brush.Parse(hex))
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
    let private stressColor = toUnicolour Colors.Yellow

    /// Cap peel barriers shown: at or above the highest, a finite
    /// barrier gets the faintest tint; at or below the lowest, the
    /// strongest. An infinite barrier (no cap peeling) gets none.
    let private minCapPeelBarrier, maxCapPeelBarrier = 1.0, 4.0

    /// Faintest and strongest yellow tints, so that any cap peeling
    /// shows, but the temperature still shows through.
    let private minStressTint, maxStressTint = 0.15, 0.6

    /// Amount of yellow to mix in for the given cap peel barrier.
    let private getStressTint capPeelBarrier =
        if Double.IsPositiveInfinity capPeelBarrier then 0.0
        else
            let x =
                (maxCapPeelBarrier - capPeelBarrier)
                    / (maxCapPeelBarrier - minCapPeelBarrier)
            minStressTint + (maxStressTint - minStressTint) * Math.Clamp(x, 0.0, 1.0)

    let private getColor temp capPeelBarrier =
        let distance =
            (temp - float ControlPanel.minTemperature)
                / float (
                    ControlPanel.maxTemperature
                        - ControlPanel.minTemperature)
        coldColor
            .Mix(hotColor, ColourSpace.Oklab, distance)
            .Mix(stressColor, ColourSpace.Oklab, getStressTint capPeelBarrier)
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
                    let capPeelBarrier = (Run.getParameters model.Run).CapPeelBarrier
                    getColor temp capPeelBarrier
                        |> renderRectangle corner size
        ]

    let private renderWalls (world : World) =
        [
            for loc in world.Walls do
                let corner = loc - Point.create (1, 1)   // every wall is 2x2 centered on its location
                let size = Point.create(2, 2)
                renderRectangle corner size Colors.Gray
        ]

    let private strokeThickness = 0.4

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
                        Ellipse.strokeThickness strokeThickness
                    Ellipse.useLayoutRounding false   // don't snap to pixels before scaling
                ] :> IView
        ]

    /// Strand backbone colors by family label.
    let private strandBrushMap =
        [
            "former", "#00A000"       // green
            "non-former", "#D00000"   // red
        ]
            |> Seq.map (fun (label, hex) ->
                label, Brush.Parse(hex))
            |> Map

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

    let private renderBonds bondType getBrush world =

        let drawLine (brush : IBrush) start finish =
            Line.create [
                Line.startPoint ~~start
                Line.endPoint ~~finish
                Line.stroke brush
                Line.strokeThickness strokeThickness
            ] :> IView

        [
            for a, b in getBonds bondType world do
                let brush = getBrush a b
                let d = World.getDisplacement a b world
                yield drawLine brush a.Location (a.Location + d)
                if a.Location + d <> b.Location then                   // bond crosses an edge?
                    yield drawLine brush b.Location (b.Location - d)   // then draw its other half also
        ]

    let private renderStrongBonds run =

        let labels = Run.getStrandLabels run

        let getBrush atomA _atomB =
            labels
                |> Map.tryFind atomA.Id   // assume both atoms are in the same strand
                |> Option.bind (fun label ->
                    Map.tryFind label strandBrushMap)
                |> Option.defaultValue Brushes.Black

        renderBonds Strong getBrush run.World

    let private renderWeakBonds world =
        let getBrush _ _ = Brushes.HotPink
        renderBonds Weak getBrush world

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
                        yield! renderStrongBonds model.Run
                        yield! renderWeakBonds world
                    ]
                ])
        ]
