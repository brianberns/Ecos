namespace Ecos.Web

open System

open Browser.Types
open Fable.Core.JsInterop

open Ecos.Engine

module AtomType =

    let minValence = 1
    let maxValence = 2

    let minHue =  60   // yellow
    let maxHue = 360   // red

    /// Gets a color representing the given valence.
    let getColor valence =
        let hue =
            (maxHue - minHue)
                * (valence - minValence)
                / (maxValence - minValence) + minHue
        $"hsl({hue}, 100%%, 50%%)"

    /// Color map.
    let colorMap =
        Map [
            for valence = minValence to maxValence do
                let typ = AtomType.create valence
                let color = getColor valence
                typ, color
        ]

    /// All atom types.
    let all =
        Seq.toArray colorMap.Keys

type DrawingContext = CanvasRenderingContext2D

module Atom =

    /// Atom radius.
    let radius = World.sigma / 1.5

    /// Draws the given atom.
    let draw (ctx : DrawingContext) (atom : Atom) =

        ctx.beginPath()

            // draw each atom as a circle
        ctx.arc(
            atom.Location.X,
            atom.Location.Y,
            radius, 0.0, Math.Tau)

            // draw the circle's border
        ctx.lineWidth <- 0.05
        ctx.stroke()

            // fill the circle
        let color = AtomType.colorMap[atom.Type]
        ctx.fillStyle <- !^color
        ctx.fill()

    /// Draws a bond between the given atoms.
    let drawBond
        (ctx : DrawingContext)
        (atomA : Atom) (atomB : Atom)
        nBonds =
        ctx.beginPath()
        ctx.moveTo(atomA.Location.X, atomA.Location.Y)
        ctx.lineTo(atomB.Location.X, atomB.Location.Y)
        let thickness = float (2 * nBonds - 1) * 0.05
        ctx.lineWidth <- thickness
        ctx.stroke()
