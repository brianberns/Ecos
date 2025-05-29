namespace Ecos.Desktop

open System

open Avalonia
open Avalonia.Media

open Ecos.Engine

[<AutoOpen>]
module PointExt =
    type Ecos.Engine.Point with
        member this.ToAvalonia() =
            Point(this.X, this.Y)

module ParticleType =

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
                let typ = ParticleType.create valence
                let brush = brushes[valence - 1]
                typ, brush
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

    /// Pen to use for particle border.
    let private pen = Pen(Brushes.Black, thickness = 0.05)

    /// Particle radius.
    let radius = World.eqDistance / 2.0

    /// Draws the given particle.
    let draw (ctx : DrawingContext) particle =
        let brush = ParticleType.brushMap[particle.Type]
        ctx.DrawEllipse(
            brush, pen,
            particle.Location.ToAvalonia(),
            radius, radius)

    /// Draws a bond between the given particles.
    let drawBond (ctx : DrawingContext) a b =
        ctx.DrawLine(
            pen,
            a.Location.ToAvalonia(),
            b.Location.ToAvalonia())
