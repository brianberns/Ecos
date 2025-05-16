namespace Ecos

open System

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

    /// Calculates Brownian motion of the given particle.
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

    /// Time step.
    let dt = 0.05

    /// Information about a vector.
    type private VectorEntry =
        {
            /// Vector between two points.
            Vector : Point

            /// Length of the vector.
            Length : float
        }

    module private VectorEntry =

        /// Creates a vector entry.
        let create vector =
            {
                Vector = vector
                Length = vector.Length
            }

        /// Zero vector.
        let zero = create Point.Zero

    /// Calculates vector between every pair of particles. The
    /// result is the lower half of a symmetric lookup table
    //// (up to sign).
    let private getVectors (particles : _[]) =
        Array.init particles.Length (fun i ->
            let particle = particles[i]
            Array.init (i + 1) (fun j ->
                assert(j <= i)   // lower half of table only
                if j = i then VectorEntry.zero
                else
                    let vector =
                        particle.Location - particles[j].Location
                    VectorEntry.create vector))

    /// Resets particle bonds to zero and maps particles by index.
    let private resetBonds particles =
        particles
            |> Seq.mapi (fun i particle ->
                i, Particle.resetBonds particle)
            |> Map

    /// Sorts interacting particles by distance.
    let private sortInteractions (triangle : _[][]) =
        seq {
            for i = 0 to triangle.Length - 1 do
                for j = 0 to i - 1 do
                    let entry = triangle[i][j]
                    if entry.Length <= repulsionRadius then
                        i, j, entry
        } |> Seq.sortBy (fun (_, _, entry) -> entry.Length)

    /// Creates bonds between closest particles.
    let private createBonds indexes (particleMap : Map<_, _>) =
        ((particleMap, Set.empty), indexes)
            ||> Seq.fold (fun (particleMap, bondSet) (i : int, j : int, _) ->
                let a = particleMap[i]
                let b = particleMap[j]
                let canBond =
                    a.Valence > a.NumBonds
                        && b.Valence > b.NumBonds
                if canBond then
                    let a, b = Particle.bond a b
                    let particleMap =
                        particleMap
                            |> Map.add i a
                            |> Map.add j b
                    let bondSet = bondSet.Add(i, j).Add(j, i)
                    particleMap, bondSet
                else particleMap, bondSet)
            |> snd

    /// Moves a single particle one time step forward.
    let private stepParticle random world (triangle : _[][]) bondSet i =
        let particle = world.Particles[i]
        let repulsion =
            Array.init world.Particles.Length (fun j ->
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
                            if Set.contains (i, j) bondSet then
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
        let location = particle.Location + (delta * dt)

        { particle with
            Location = clip world.Extent location }

    /// Moves the particles in the given world one time step
    /// forward.
    let step random world =
        let triangle = getVectors world.Particles
        let bondSet =
            let tuples = sortInteractions triangle
            resetBonds world.Particles
                |> createBonds tuples
        let particles =
            Array.init world.Particles.Length (
                stepParticle random world triangle bondSet)
        { world with Particles = particles }
