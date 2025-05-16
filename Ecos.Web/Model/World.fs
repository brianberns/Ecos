namespace Ecos

open System

type Particle =
    {
        Valence : int
        NumBonds : int
        Location : Point
    }

module Particle =

    let create valence location =
        {
            Valence = valence
            NumBonds = 0
            Location = location
        }

    let resetBonds particle =
        { particle with NumBonds = 0 }

    let bond particleA particleB =
        assert(particleA.NumBonds < particleA.Valence)
        assert(particleB.NumBonds < particleB.Valence)
        { particleA with
            NumBonds = particleA.NumBonds + 1 },
        { particleB with
            NumBonds = particleB.NumBonds + 1 }

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
    let repulsionRadius = 2.0

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
            let zero =
                {|
                    Vector = Point.Zero
                    Length = 0.0
                |}
            Array.init nParticles (fun i ->
                let particle = particles[i]
                Array.init (i + 1) (fun j ->
                    if j = i then zero
                    else
                        let vector =
                            particle.Location - particles[j].Location
                        let length = vector.Length
                        {|
                            Vector = vector
                            Length = length
                        |}))

        let particleMap =
            particles
                |> Seq.mapi (fun i particle ->
                    i, Particle.resetBonds particle)
                |> Map

        let tuples =
            [|
                for i = 0 to nParticles - 1 do
                    for j = 0 to i - 1 do
                        let entry = triangle[i][j]
                        if entry.Length <= repulsionRadius then
                            i, j, entry
            |] |> Array.sortBy (fun (_, _, entry) -> entry.Length)

        let _, bondSet =
            ((particleMap, Set.empty), tuples)
                ||> Seq.fold (fun (particleMap, bondSet) (i, j, _) ->
                    let particleA = particleMap[i]
                    let particleB = particleMap[j]
                    let canBond =
                        particleA.Valence > particleA.NumBonds
                            && particleB.Valence > particleB.NumBonds
                    if canBond then
                        let particleA, particleB =
                            Particle.bond particleA particleB
                        let particleMap =
                            particleMap
                                |> Map.add i particleA
                                |> Map.add j particleB
                        let bondSet = bondSet.Add(i, j).Add(j, i)
                        particleMap, bondSet
                    else particleMap, bondSet)

        let particles =
            Array.init nParticles (fun i ->
                let particle = particles[i]
                let repulsion =
                    Array.init nParticles (fun j ->
                        let entry, sign =
                            if j <= i then triangle[i][j], 1.0
                            else triangle[j][i], -1.0
                        let repulsion =
                            if i = j then Point.Zero
                            elif entry.Length < repulsionRadius then
                                let strength =
                                    repulsionStrength
                                        * (repulsionRadius - entry.Length)
                                let strength =
                                    if bondSet.Contains(i, j) then
                                        strength - repulsionRadius / 2.0
                                    else strength
                                strength
                                    * (entry.Vector / entry.Length)
                                    * sign
                            else Point.Zero
                        repulsion)
                        |> Array.sum
                let brownian =
                    getBrownian random world.Extent particle
                let delta = repulsion + brownian
                let location = particles[i].Location + (delta * dt)

                { particle with
                    Location = clip world.Extent location })

        { world with Particles = particles }
