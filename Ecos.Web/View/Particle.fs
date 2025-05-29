namespace Ecos.Web

open System

open Browser.Types
open Fable.Core.JsInterop

open Ecos.Engine

module ParticleType =

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
                let typ = ParticleType.create valence
                let color = getColor valence
                typ, color
        ]

module Particle =

    /// Answers a unit vector pointing in a random direction.
    let randomUnitVector (random : Random) =
        let theta = Math.Tau * random.NextDouble()
        Point.create (cos theta) (sin theta)

    /// Makes the given number of particles.
    let makeParticles
        (random : Random)
        typ
        numParticles
        (scale : Point)
        offset =
        Array.init numParticles (fun _ ->
            let r = random.NextDouble()
            let location =
                r * scale * randomUnitVector random + offset
            let velocity = Point.Zero
            Particle.create typ location velocity)

    /// Particle radius.
    let radius = World.eqDistance / 2.0

    /// Draws the given particle.
    let draw (ctx : CanvasRenderingContext2D) particle =

        ctx.beginPath()

            // draw each particle as a circle
        ctx.arc(
            particle.Location.X,
            particle.Location.Y,
            radius, 0.0, Math.Tau)

            // draw the circle's border
        ctx.stroke()

            // fill the circle
        let color = ParticleType.colorMap[particle.Type]
        ctx.fillStyle <- !^color
        ctx.fill()

    /// Draws a bond between the given particles.
    let drawBond (ctx : CanvasRenderingContext2D) a b =
        ctx.beginPath()
        ctx.moveTo(a.Location.X, a.Location.Y)
        ctx.lineTo(b.Location.X, b.Location.Y)
        ctx.stroke()
