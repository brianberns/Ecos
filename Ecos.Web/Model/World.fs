namespace Ecos

open System

type Particle =
    {
        Valence : int
        Bonds : int
        Location : Point
    }

module Particle =

    let create valence location =
        {
            Valence = valence
            Bonds = 0
            Location = location
        }

    let bond particleA particleB bonds =
        assert(particleA.Bonds + bonds <= particleA.Valence)
        assert(particleB.Bonds + bonds <= particleB.Valence)
        { particleA with Bonds = particleA.Bonds + bonds },
        { particleB with Bonds = particleB.Bonds + bonds }

/// World of objects to animate.
type World =
    {
        /// Size of the world, with the origin at the center.
        Extent : Point

        /// Particles in the world.
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

    let getBonds (particles : _[]) =

        let tuples =
            [|
                for i = 0 to particles.Length - 1 do
                    let particleA = particles[i]
                    for j = 0 to i - 1 do
                        let particleB = particles[j]
                        let vector = particleA.Location - particleB.Location
                        let length = vector.Length
                        if length <= repulsionRadius then
                            yield i, j, vector, length
            |] |> Array.sortBy (fun (_, _, _, length) -> length)

        let particleMap =
            particles
                |> Seq.mapi (fun i particle -> i, particle)
                |> Map
        (particleMap, tuples)
            ||> Seq.fold (fun particleMap (i, j, _, _) ->
                let particleA = particleMap[i]
                let particleB = particleMap[j]
                let bonds =
                    min
                        (particleA.Valence - particleA.Bonds)
                        (particleB.Valence - particleB.Bonds)
                let particleA, particleB =
                    Particle.bond particleA particleB bonds
                particleMap
                    |> Map.add i particleA
                    |> Map.add j particleB)
            |> Map.values
            |> Seq.toArray

    /// Gets the temperature at the given point.
    let getTemperature extent (point : Point) =
        let stdDev = (min extent.X extent.Y) / 4.0
        exp -((point.X * point.X + point.Y * point.Y) / (stdDev * stdDev))

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
