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

    let colors =
        [|
            "Yellow"
            "Red"
        |]

    /// Gets a color representing the given valence.
    let getColor valence =
        colors[valence - 1]

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

    /// Draws the given particle.
    let draw (ctx : DrawingContext) particle =
        let r = 0.4
        let brush =
            particle.Type.Color
                |> Color.Parse
                |> SolidColorBrush
        let pen = Pen(Brushes.Black)
        ctx.DrawEllipse(
            brush, pen,
            particle.Location.ToAvalonia(),
            r, r)
