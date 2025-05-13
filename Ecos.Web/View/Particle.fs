namespace Ecos.Web

open System
open Browser.Types
open Ecos

module Particle =

    /// Makes the given number of particles.
    let makeParticles (random : Random) numParticles scale offset =
        Array.init numParticles (fun _ ->
            let r = random.NextDouble()
            let theta = 2.0 * Math.PI * random.NextDouble()
            let point = Point.create (r * cos theta) (r * sin theta)
            point * scale + offset)

    /// Full circle.
    let two_pi = 2.0 * Math.PI

    /// Draws the given particle.
    let draw (ctx : CanvasRenderingContext2D) point =

        ctx.beginPath()

            // draw each particle as a circle
        let r = 0.2
        ctx.arc(point.X, point.Y, r, 0.0, two_pi)

            // draw the circle's border
        ctx.stroke()
