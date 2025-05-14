namespace Ecos.Web

open System

open Browser.Types
open Fable.Core.JsInterop

open Ecos

module Particle =

    let minValence = 1
    let maxValence = 6

    /// Makes the given number of particles.
    let makeParticles
        (random : Random) numParticles (scale : Point) offset =
        Array.init numParticles (fun _ ->
            let valence = random.Next(minValence, maxValence + 1)
            let r = random.NextDouble()
            let location =
                r * scale * random.NextPoint() + offset
            Particle.create valence location)

    let minHue = 60
    let maxHue = 360

    /// Gets a color representing the given particle.
    let getColor particle =
        let hue =
            (maxHue - minHue)
                * (particle.Valence - minValence)
                / (maxValence - minValence) + minHue
        !^($"hsl({hue}, 100%%, 50%%)")

    /// Draws the given particle.
    let draw (ctx : CanvasRenderingContext2D) particle =

        ctx.beginPath()

            // draw each particle as a circle
        let r = 0.4
        ctx.arc(
            particle.Location.X,
            particle.Location.Y,
            r, 0.0, Math.Tau)

            // draw the circle's border
        ctx.stroke()

        ctx.fillStyle <- getColor particle
        ctx.fill()
