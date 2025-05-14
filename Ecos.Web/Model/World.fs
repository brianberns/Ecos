namespace Ecos

open System

type Particle =
    {
        Valence : int
        Location : Point
    }

module Particle =

    let create valence location =
        {
            Valence = valence
            Location = location
        }

/// World of objects to animate.
type World =
    {
        Extent : Point
        Particles : Particle[]
    }

module World =

    /// Creates a world.
    let create extent particles =
        {
            Extent = extent
            Particles = particles
        }

    let repulsionStrength = 1.0
    let repulsionRadius = 1.0

    /// Calculates the repulsion between two particles.
    let getRepulsion particleA particleB =
        assert(particleA <> particleB)
        let vector = particleA.Location - particleB.Location
        let length = vector.Length
        if length < repulsionRadius then
            repulsionStrength * (repulsionRadius - length)
                * (vector / length)
        else Point.Zero

    let getTemperature (extent : Point) (point : Point) =
        extent.Length / (20.0 * point.Length)

    let getBrownian (random : Random) extent particle =
        let temp = getTemperature extent particle.Location
        assert(temp >= 0.0)
        temp * random.NextPoint()

    /// Force that tries to keep particles inside the world.
    let clip (extent : Point) point =
        let x =
            point.X
                - min 0.0 ((2.0 * point.X) + extent.X)   // left edge
                - max 0.0 ((2.0 * point.X) - extent.X)   // right edge
        let y =
            point.Y
                - min 0.0 ((2.0 * point.Y) + extent.Y)   // bottom edge
                - max 0.0 ((2.0 * point.Y) - extent.Y)   // top edge
        Point.create x y

    let dt = 0.05

    /// Moves the particles in the given world one time step
    /// forward.
    let step random world =

            // compute the contents of the lookup table
        let particles = world.Particles
        let nParticles = particles.Length
        let triangle =
            Array.init nParticles (fun i ->
                let particle = particles[i]
                Array.init (i + 1) (fun j ->
                    if j = i then Point.Zero
                    else
                        getRepulsion particle particles[j]))

            // full lookup table
        let lookup i j =
            if j <= i then triangle[i][j]
            else -triangle[j][i]

        let particles =
            Array.init nParticles (fun i ->
                let particle = particles[i]
                let repulsion =
                    Array.init nParticles (lookup i)
                        |> Array.sum
                let brownian =
                    getBrownian random world.Extent particle
                let delta = repulsion + brownian
                let location = particles[i].Location + (delta * dt)

                { particle with
                    Location = clip world.Extent location })

        { world with Particles = particles }
