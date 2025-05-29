namespace Ecos.Desktop

open System

open Avalonia
open Avalonia.Media

open Ecos.Engine

[<AutoOpen>]
module PointExt =
    type Ecos.Engine.Point with
        member this.ToAvalonia() =
            Point(this.X, this.Y)

module AtomType =

    let minValence = 1
    let maxValence = 2

    let private brushes =
        [|
            Brushes.Yellow
            Brushes.Red
        |]

    /// Brush map.
    let brushMap =
        Map [
            for valence = minValence to maxValence do
                let typ = AtomType.create valence
                let brush = brushes[valence - 1]
                typ, brush
        ]

module Atom =

    /// Answers a unit vector pointing in a random direction.
    let randomUnitVector (random : Random) =
        let theta = Math.Tau * random.NextDouble()
        Point.create (cos theta) (sin theta)

    /// Makes the given number of atoms.
    let makeAtoms
        (random : Random)
        typ
        numAtoms
        (scale : Point)
        offset =
        Array.init numAtoms (fun _ ->
            let r = random.NextDouble()
            let location =
                r * scale * randomUnitVector random + offset
            let velocity = Point.Zero
            Atom.create typ location velocity)

    /// Pen to use for atom border.
    let private pen = Pen(Brushes.Black, thickness = 0.05)

    /// Atom radius.
    let radius = World.eqDistance / 2.0

    /// Draws the given atom.
    let draw (ctx : DrawingContext) atom =
        let brush = AtomType.brushMap[atom.Type]
        ctx.DrawEllipse(
            brush, pen,
            atom.Location.ToAvalonia(),
            radius, radius)

    /// Draws a bond between the given atoms.
    let drawBond (ctx : DrawingContext) atomA atomB =
        ctx.DrawLine(
            pen,
            atomA.Location.ToAvalonia(),
            atomB.Location.ToAvalonia())
