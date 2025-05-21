namespace Ecos.Web

open System

open Browser.Types
open Fable.Core.JsInterop

open Ecos

module ParticleType =

    let colors =
        [|
            for hue in [ 60; 120; 180; 240; 300; 360 ] do
                $"hsl({hue}, 100%%, 50%%)"
        |]

    /// All particle types.
    let all =
        [|
            ParticleType.create 1 (Array.head colors)
            ParticleType.create 1 (Array.last colors)
        |]

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
