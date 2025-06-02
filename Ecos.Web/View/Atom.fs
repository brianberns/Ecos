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

type DrawingContext = CanvasRenderingContext2D

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

    /// Atom radius.
    let radius = World.repulsionDistance / 2.0

    /// Draws the given atom.
    let draw (ctx : DrawingContext) (atom : Atom) =

        ctx.beginPath()

            // draw each atom as a circle
        ctx.arc(
            atom.Location.X,
            atom.Location.Y,
            radius, 0.0, Math.Tau)

            // draw the circle's border
        ctx.stroke()

            // fill the circle
        let color = AtomType.colorMap[atom.Type]
        ctx.fillStyle <- !^color
        ctx.fill()

    /// Draws a bond between the given atoms.
    let drawBond
        (ctx : DrawingContext) (atomA : Atom) (atomB : Atom) =
        ctx.beginPath()
        ctx.moveTo(atomA.Location.X, atomA.Location.Y)
        ctx.lineTo(atomB.Location.X, atomB.Location.Y)
        ctx.stroke()
