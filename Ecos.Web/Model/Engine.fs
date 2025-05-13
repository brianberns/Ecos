namespace Ecos

open System

type Particle = Point

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

module Engine =

    let repulsionStrength = 1.0
    let repulsionRadius = 1.0

    /// Calculates the repulsion between two particles.
    let getRepulsion particleA particleB =
        assert(particleA <> particleB)
        let vector : Point = particleA - particleB
        let length = vector.Length
        if length < repulsionRadius then
            repulsionStrength * (repulsionRadius - length)
                * (vector / length)
        else Point.Zero

    let getTemperature (point : Point) =
        point.Length / 1.0

    let getBrownian (random : Random) particle =
        let temp = getTemperature particle
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

            // compute the upper triangle of the lookup table
        let particles = world.Particles
        let nParticles = particles.Length
        let upper =
            Array.init nParticles (fun i ->
                let particle = particles[i]
                Array.init (nParticles - i) (fun offset ->
                    if offset = 0 then Point.Zero
                    else
                        getRepulsion particle particles[i + offset]))

            // full lookup table
        let lookup i j =
            if i <= j then upper[i][j - i]
            else -upper[j][i - j]

        let particles =
            Array.init nParticles (fun i ->
                let repulsion =
                    Array.init nParticles (lookup i)
                        |> Array.sum
                let brownian = getBrownian random particles[i]
                let delta = repulsion + brownian
                let point = particles[i] + (delta * dt)
                clip world.Extent point)

        { world with Particles = particles }
