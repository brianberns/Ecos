namespace Ecos.Desktop

open System

open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Media

open Ecos.Engine

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
    let draw (canvas : Canvas) particle =
        let r = 0.4
        let brush =
            particle.Type.Color
                |> Color.Parse
                |> SolidColorBrush
        let shape =
            Ellipse(
                Width = 2.0 * r,
                Height = 2.0 * r,
                Fill = brush)
        canvas.Children.Add(shape)
        Canvas.SetLeft(shape, particle.Location.X)
        Canvas.SetTop(shape, particle.Location.Y)
