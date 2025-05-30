namespace Ecos.Desktop

open Avalonia.Media
open Ecos.Engine

module AtomType =

    /// Brush map.
    let brushMap =
        Map [
            AtomType.create 1 1, Brushes.Yellow
            AtomType.create 2 1, Brushes.Red
        ]

module Atom =

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
