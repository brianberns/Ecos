namespace Ecos.Desktop

open System
open Avalonia.Media
open Ecos.Engine

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

    /// Pen to use for atom border.
    let private pen = Pen(Brushes.Black, thickness = 0.05)

    /// Atom radius.
    let radius = World.repulsionDistance / 2.0

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
