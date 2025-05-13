namespace Ecos.Web

open System

open Browser.Types
open Fable.Core.JsInterop

open Ecos

module Particle =

    /// Makes the given number of particles.
    let makeParticles
        (random : Random) numParticles (scale : Point) offset =
        Array.init numParticles (fun _ ->
            let r = random.NextDouble()
            r * scale * random.NextPoint() + offset)

    /// Draws the given particle.
    let draw (ctx : CanvasRenderingContext2D) point =

        ctx.beginPath()

            // draw each particle as a circle
        let r = 0.4
        ctx.arc(point.X, point.Y, r, 0.0, Math.Tau)

            // draw the circle's border
        ctx.stroke()

        ctx.fillStyle <- !^"black"
        ctx.fill()
