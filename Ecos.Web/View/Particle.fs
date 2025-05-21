namespace Ecos.Web

open System

open Browser.Types
open Fable.Core.JsInterop

open Ecos

module ParticleType =

    let minValence = 1
    let maxValence = 3

    let minHue =  60   // yellow
    let maxHue = 360   // red

    /// Gets a color representing the given valence.
    let getColor valence =
        let hue =
            (maxHue - minHue)
                * (valence - minValence)
                / (maxValence - minValence) + minHue
        $"hsl({hue}, 100%%, 50%%)"

    /// All particle types.
    let all =
        [|
            for valence = minValence to maxValence do
                ParticleType.create valence (getColor valence)
        |]

module Particle =

    /// Answers a unit vector pointing in a random direction.
    let randomUnitVector (random : Random) =
        let theta = Math.Tau * random.NextDouble()
        Point.create (cos theta) (sin theta)

    /// Makes the given number of particles.
    let makeParticles
        (random : Random) numParticles (scale : Point) offset =
        Array.init numParticles (fun _ ->
            let typ =
                let idx = random.Next(ParticleType.all.Length)
                ParticleType.all[idx]
            let r = random.NextDouble()
            let location =
                r * scale * randomUnitVector random + offset
            Particle.create typ location)

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

        ctx.fillStyle <- !^particle.Type.Color
        ctx.fill()
