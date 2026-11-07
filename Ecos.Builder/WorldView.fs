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
/// (template atoms filled), strong bonds in black and weak bonds in pink.
/// (After the desktop runner's view.)
module WorldView =

    let brushMap =
        [
            AtomType.H, "#A0A0A0"   // gray
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

    let private (~~)(pt : Ecos.Point) =
        Avalonia.Point(pt.X, pt.Y)

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

    let private renderEnvironment (run : Run) =
        let spacing = 8
        let size = run.World.Size
        let getTemp = Run.getTemperature run
        let capPeelBarrier = (Run.getParameters run).CapPeelBarrier
        [
            for x in 0 .. spacing .. size.X - 1 do
                for y in 0 .. spacing .. size.Y - 1 do
                    let width = min spacing (size.X - x)
                    let height = min spacing (size.Y - y)
                    let center = Point.create (x + width / 2, y + height / 2)
                    getColor (getTemp center) capPeelBarrier
                        |> renderRectangle (Point.create (x, y)) (Point.create (width, height))
        ]

    let private renderWalls (world : World) =
        [
            for loc in world.Walls do
                renderRectangle (loc - Point.create (1, 1)) (Point.create (2, 2)) Colors.Gray
        ]

    let private strokeThickness = 0.4

    let private renderAtoms (run : Run) =
        let templateIds = set run.Template
        [
            for atom in run.World.Atoms do
                Ellipse.create [
                    Ellipse.width 2
                    Ellipse.height 2
                    let pt = atom.Location - Point.create (1, 1)
                    Ellipse.left pt.X
                    Ellipse.top pt.Y
                    if templateIds.Contains atom.Id then
                        Ellipse.fill brushMap[atom.Type]
                    else
                        Ellipse.stroke brushMap[atom.Type]
                        Ellipse.strokeThickness strokeThickness
                    Ellipse.useLayoutRounding false
                ] :> IView
        ]

    let private renderBonds bondType (getBrush : AtomId -> IBrush) (world : World) =
        let drawLine brush start finish =
            Line.create [
                Line.startPoint ~~start
                Line.endPoint ~~finish
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
                        yield drawLine brush a.Location (a.Location + d)
                        if a.Location + d <> b.Location then   // crosses an edge: draw its other half too
                            yield drawLine brush b.Location (b.Location - d)
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

    /// Renders the given run's world, scaled to fit, with the strong
    /// bonds of each colored family in its color.
    let render (run : Run) familyColors =
        let world = run.World
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
                        yield! renderEnvironment run
                        yield! renderWalls world
                        yield! renderAtoms run
                        yield! renderBonds Strong getStrongBrush world
                        yield! renderBonds Weak (fun _ -> Brushes.HotPink) world
                    ]
                ])
        ]
