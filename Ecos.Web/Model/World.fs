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
        2.0

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

    /// Relationship between two points.
    type private VectorEntry =
        {
            /// Vector between the points.
            Vector : Point

            /// Length of the vector.
            Length : float

            /// Repulsion between the points.
            Repulsion : float
        }

    module private VectorEntry =

        /// Creates a vector entry.
        let create (vector : Point) =
            let length = vector.Length
            let repulsion =
                repulsionStrength
                    * (repulsionRadius - length)
            {
                Vector = vector
                Length = length
                Repulsion = repulsion
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

    /// Sorts interacting particles by distance.
    let private sortInteractions (entries : _[][]) =
        seq {
            for i = 0 to entries.Length - 1 do
                for j = 0 to i - 1 do
                    let entry = entries[i][j]
                    if entry.Length <= repulsionRadius then
                        i, j, entry
        } |> Seq.sortBy (fun (_, _, entry) -> entry.Length)

    /// Creates bonds between closest particles.
    let private createBonds indexes particles =

            // reset bonds to zero
        let particles =
            particles
                |> Seq.map Particle.resetBonds
                |> ImmutableArray.Create<_>

        ((particles, Set.empty), indexes)
            ||> Seq.fold (fun (particles, bondSet) (i : int, j : int, _) ->
                let a = particles[i]
                let b = particles[j]
                let canBond =
                    a.Valence > a.NumBonds
                        && b.Valence > b.NumBonds
                if canBond then
                    let a, b = Particle.bond a b
                    let particles =
                        particles
                            .SetItem(i, a)
                            .SetItem(j, b)
                    let bondSet = bondSet.Add(i, j).Add(j, i)
                    particles, bondSet
                else particles, bondSet)
            |> snd

    /// Calculates the force between two particles.
    let private getForce entry sign bonded =

            // repulsion
        let strength = entry.Repulsion

            // attraction between bonded particles?
        let strength =
            if bonded then
                strength - repulsionRadius / 2.0
            else strength

            // align to vector
        strength
            * (entry.Vector / entry.Length)
            * sign

    /// Moves a single particle one time step forward.
    let private stepParticle random world (entries : _[][]) bondSet i =
        let particle = world.Particles[i]
        let force =
            Array.init world.Particles.Length (fun j ->
                let entry, sign =
                    if j <= i then entries[i][j], 1.0
                    else entries[j][i], -1.0
                if i = j then Point.Zero
                elif entry.Length < repulsionRadius then
                    let bonded = Set.contains (i, j) bondSet
                    getForce entry sign bonded
                else Point.Zero)
                |> Array.sum
        let brownian =
            getBrownian random world.Extent particle
        let delta = force + brownian
        let location = particle.Location + (delta * dt)

        { particle with
            Location = clip world.Extent location }

    /// Moves the particles in the given world one time step
    /// forward.
    let step random world =
        let entries = getVectors world.Particles
        let bondSet =
            let tuples = sortInteractions entries
            createBonds tuples world.Particles
        let particles =
            Array.init world.Particles.Length (
                stepParticle random world entries bondSet)
        { world with Particles = particles }
