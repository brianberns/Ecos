namespace Ecos.Builder

open System

open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Media

open Wacton.Unicolour

open Ecos
open Ecos.Experiment

/// Draws a run's world: the temperature behind it, its walls, atoms
/// (template atoms filled, fixed atoms square), strong bonds in black and
/// weak bonds in pink.
/// (After the desktop runner's view.)
module WorldView =

    let brushMap =
        [
            AtomType.W, "#1F4E9E"   // blue
            AtomType.X, "#2E7D32"   // green
            AtomType.Y, "#B0006B"   // red
            AtomType.Z, "#B26A00"   // orange
            AtomType.P, "#000000"   // black
            AtomType.Q, "#707070"   // gray
        ]
            |> Seq.map (fun (typ, hex) -> typ, Brush.Parse(hex))
            |> Map

    /// Temperatures shown from light blue (at the lowest) to red (at
    /// the highest).
    let minTemperature, maxTemperature = 0.0, 5.0

    let private renderRectangle (corner : Ecos.Point) (size : Ecos.Point) (color : Color) =
        Rectangle.create [
            Rectangle.left corner.X
            Rectangle.top corner.Y
            Rectangle.width size.X
            Rectangle.height size.Y
            Rectangle.fill color
            Rectangle.useLayoutRounding false
        ] :> IView

    let private toUnicolour (color : Color) =
        Unicolour(ColourSpace.Rgb255, float color.R, float color.G, float color.B)

    let private toColor (ucolor : Unicolour) =
        let rgb = ucolor.Rgb.Byte255
        Color.FromRgb(byte rgb.R, byte rgb.G, byte rgb.B)

    let private coldColor = toUnicolour Colors.LightBlue
    let private hotColor = toUnicolour Colors.Red
    let private stressColor = toUnicolour Colors.Yellow

    /// Amount of yellow to mix in for the given cap peel barrier: none
    /// if there is no cap peeling, more the lower the barrier.
    let private getStressTint capPeelBarrier =
        if Double.IsPositiveInfinity capPeelBarrier then 0.0
        else
            let x = (4.0 - capPeelBarrier) / 3.0
            0.15 + 0.45 * Math.Clamp(x, 0.0, 1.0)

    /// Color of the given temperature and cap peel barrier.
    let getColor temp capPeelBarrier =
        let distance = (temp - minTemperature) / (maxTemperature - minTemperature)
        coldColor
            .Mix(hotColor, ColourSpace.Oklab, Math.Clamp(distance, 0.0, 1.0))
            .Mix(stressColor, ColourSpace.Oklab, getStressTint capPeelBarrier)
            |> toColor

    /// Where to draw things in a world that has been scrolled by the
    /// given shift: each location moves back by the shift, wrapping
    /// around the torus.
    type private Placer(size : Point, shiftX : float, shiftY : float) =
        let wrap (v : float) (n : int) =
            let n = float n
            ((v % n) + n) % n
        member _.Place(loc : Point) =
            Avalonia.Point(wrap (float loc.X - shiftX) size.X, wrap (float loc.Y - shiftY) size.Y)
        /// The world location shown at the given canvas point.
        member _.Unplace(x : float, y : float) =
            Point.create (int (wrap (x + shiftX) size.X), int (wrap (y + shiftY) size.Y))

    let private renderEnvironment (run : Run) (placer : Placer) =
        let spacing = 8
        let size = run.World.Size
        let getTemp = Run.getBaseTemperature run   // zones are drawn over this
        let capPeelBarrier = (Run.getParameters run).CapPeelBarrier
        [
            for x in 0 .. spacing .. size.X - 1 do
                for y in 0 .. spacing .. size.Y - 1 do
                    let width = min spacing (size.X - x)
                    let height = min spacing (size.Y - y)
                    let center = placer.Unplace(float x + float width / 2.0, float y + float height / 2.0)
                    getColor (getTemp center) capPeelBarrier
                        |> renderRectangle (Point.create (x, y)) (Point.create (width, height))
        ]

    /// Zones: each outlined, and filled with the color of its temperature
    /// if it has its own now. (With none, a zone is as warm as the world
    /// around it, so it is shown by its outline alone.) Drawn in small
    /// pieces, so that a zone that crosses an edge of the view wraps.
    let private renderZones (run : Run) (placer : Placer) =
        let size = run.World.Size
        let fillOpt =
            Run.tryGetZoneTemperature run
                |> Option.map (fun temp ->
                    getColor temp (Run.getParameters run).CapPeelBarrier)
        let outline = Color.FromArgb(150uy, 64uy, 64uy, 64uy)
        let thickness = 0.3
        let tile = 2
        let piece (x : int) (y : int) (offsetX : float) (offsetY : float) (width : float) (height : float) (color : Color) =
            let pt = placer.Place(run.World.NormalizeLocation(Point.create (x, y)))
            Rectangle.create [
                Rectangle.left (pt.X + offsetX)
                Rectangle.top (pt.Y + offsetY)
                Rectangle.width width
                Rectangle.height height
                Rectangle.fill color
                Rectangle.useLayoutRounding false
            ] :> IView
        [
            for zone in run.Scenario.Zones do
                let left = size.X / 2 + zone.X - zone.Width / 2
                let top = size.Y / 2 + zone.Y - zone.Height / 2
                for dx in 0 .. tile .. zone.Width - 1 do
                    let width = float (min tile (zone.Width - dx))
                    for dy in 0 .. tile .. zone.Height - 1 do
                        let height = float (min tile (zone.Height - dy))
                        match fillOpt with
                            | Some fill -> yield piece (left + dx) (top + dy) 0.0 0.0 width height fill
                            | None -> ()
                        if dy = 0 then   // top edge
                            yield piece (left + dx) (top + dy) 0.0 0.0 width thickness outline
                        if dy + tile >= zone.Height then   // bottom edge
                            yield piece (left + dx) (top + dy) 0.0 (height - thickness) width thickness outline
                        if dx = 0 then   // left edge
                            yield piece (left + dx) (top + dy) 0.0 0.0 thickness height outline
                        if dx + tile >= zone.Width then   // right edge
                            yield piece (left + dx) (top + dy) (width - thickness) 0.0 thickness height outline
        ]

    let private renderWalls (world : World) (placer : Placer) =
        [
            for loc in world.Walls do
                let pt = placer.Place loc
                Rectangle.create [
                    Rectangle.left (pt.X - 1.0)
                    Rectangle.top (pt.Y - 1.0)
                    Rectangle.width 2.0
                    Rectangle.height 2.0
                    Rectangle.fill Colors.Gray
                    Rectangle.useLayoutRounding false
                ] :> IView
        ]

    let private strokeThickness = 0.4

    let private renderAtoms (run : Run) (placer : Placer) =
        let templateIds = set run.Template
        [
            for atom in run.World.Atoms do
                let pt = placer.Place atom.Location
                let isFilled = templateIds.Contains atom.Id
                if atom.IsMobile then
                    Ellipse.create [
                        Ellipse.width 2.0
                        Ellipse.height 2.0
                        Ellipse.left (pt.X - 1.0)   // every atom is 2x2 centered on its location
                        Ellipse.top (pt.Y - 1.0)
                        if isFilled then
                            Ellipse.fill brushMap[atom.Type]
                        else
                            Ellipse.stroke brushMap[atom.Type]
                            Ellipse.strokeThickness strokeThickness
                        Ellipse.useLayoutRounding false
                    ] :> IView
                else   // a fixed atom is a square
                    Rectangle.create [
                        Rectangle.width 2.0
                        Rectangle.height 2.0
                        Rectangle.left (pt.X - 1.0)
                        Rectangle.top (pt.Y - 1.0)
                        if isFilled then
                            Rectangle.fill brushMap[atom.Type]
                        else
                            Rectangle.stroke brushMap[atom.Type]
                            Rectangle.strokeThickness strokeThickness
                        Rectangle.useLayoutRounding false
                    ] :> IView
        ]

    let private renderBonds bondType (getBrush : AtomId -> IBrush) (world : World) (placer : Placer) =
        let drawLine brush (start : Avalonia.Point) (finish : Avalonia.Point) =
            Line.create [
                Line.startPoint start
                Line.endPoint finish
                Line.stroke (brush : IBrush)
                Line.strokeThickness strokeThickness
            ] :> IView
        [
            for KeyValue (idA, ids) in world.BondTypeMap[bondType] do
                for idB in ids do
                    if idA < idB then
                        let a, b = world.Atoms[idA], world.Atoms[idB]
                        let brush = getBrush idA
                        let d = World.getDisplacement a b world
                        let d = Avalonia.Vector(float d.X, float d.Y)
                        let ptA, ptB = placer.Place a.Location, placer.Place b.Location
                        yield drawLine brush ptA (ptA + d)
                        if Avalonia.Point.Distance(ptA + d, ptB) > 0.5 then   // crosses an edge: draw its other half too
                            yield drawLine brush ptB (ptB - d)
        ]

    /// Brushes of the given colors, made once each.
    let private brushes = System.Collections.Concurrent.ConcurrentDictionary<string, IBrush>()

    /// Strong-bond brush of each atom in a chain of a colored family.
    let private getFamilyBrushes (familyColors : (Text.RegularExpressions.Regex * string)[]) (world : World) =
        if familyColors.Length = 0 then Map.empty
        else
            Experiment3.getStrands world
                |> Seq.where (fun strand -> not strand.IsRing)
                |> Seq.choose (fun strand ->
                    let sequence = strand.AtomIds |> Array.map (fun id -> world.Atoms[id].Type)
                    familyColors
                        |> Array.tryFind (fun (regex, _) -> ScenarioSpec.matchesFamily regex sequence)
                        |> Option.map (fun (_, color) ->
                            let brush = brushes.GetOrAdd(color, fun c -> Brush.Parse(c))
                            strand.AtomIds |> Seq.map (fun id -> id, brush)))
                |> Seq.concat
                |> Map

    /// Renders the given run's world, scrolled around the torus by the
    /// given shift (in world units) and scaled to fit, with the strong
    /// bonds of each colored family in its color.
    let render (run : Run) familyColors (shiftX : float, shiftY : float) =
        let world = run.World
        let placer = Placer(world.Size, shiftX, shiftY)
        let familyBrushes = getFamilyBrushes familyColors world
        let getStrongBrush id =
            familyBrushes |> Map.tryFind id |> Option.defaultValue (Brushes.Black :> IBrush)
        Viewbox.create [
            Viewbox.stretch Stretch.Uniform
            Viewbox.child (
                Canvas.create [
                    Canvas.width (float world.Size.X)
                    Canvas.height (float world.Size.Y)
                    Canvas.clipToBounds true
                    Canvas.children [
                        yield! renderEnvironment run placer
                        yield! renderZones run placer
                        yield! renderWalls world placer
                        yield! renderAtoms run placer
                        yield! renderBonds Strong getStrongBrush world placer
                        yield! renderBonds Weak (fun _ -> Brushes.HotPink) world placer
                    ]
                ])
        ]
