namespace Ecos

open System

/// World of objects to animate.
type World =
    {
        /// Size of the world, with the origin at the center.
        Extent : Point

        /// Particles in the world.
        Particles : Particle[]

        BondTypePriorityMap : Map<ParticleType * ParticleType, int>
    }

module World =

    /// Repulsion strength.
    let repulsionStrength = 1.0

    /// Maximum distance at which repulsion occurs.
    let repulsionRadius = 2.0

    /// Brownian strength.
    let brownianStrength = 0.1

    /// Brownian friction.
    let brownianFriction = 0.1

    /// Time step.
    let dt = 0.05

    /// Creates a world.
    let create extent particles bondTypePriorityMap =
        {
            Extent = extent
            Particles = particles
            BondTypePriorityMap = bondTypePriorityMap
        }

    /// Standard normal distribution using Box-Muller transform.
    let gaussianNoise (random : Random) =
        let u1 = random.NextDouble()
        let u2 = random.NextDouble()
        sqrt (-2.0 * log u1) * cos (Math.Tau * u2)

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

    /// Relationship between two points.
    type private VectorEntry =
        {
            /// Vector between the points.
            Vector : Point

            /// Length of the vector.
            Length : float

            /// Repulsion between the points.
            Repulsion : float

            BondTypePriority : int
        }

    module private VectorEntry =

        /// Creates a vector entry.
        let create (vector : Point) bondTypePriority =
            let length = vector.Length
            let repulsion =
                repulsionStrength
                    * (repulsionRadius - length)
            {
                Vector = vector
                Length = length
                Repulsion = repulsion
                BondTypePriority = bondTypePriority
            }

        /// Zero vector.
        let zero = create Point.Zero 0

    /// Calculates vector between every pair of particles. The
    /// result is the lower half of a symmetric lookup table
    //// (up to sign).
    let private getVectors bondTypePriorityMap (particles : _[]) =
        Array.init particles.Length (fun i ->
            let particle = particles[i]
            Array.init (i + 1) (fun j ->
                assert(j <= i)   // lower half of table only
                if j = i then VectorEntry.zero
                else
                    let other = particles[j]
                    let vector = particle.Location - other.Location
                    let priority =
                        bondTypePriorityMap
                            |> Map.tryFind (particle.Type, other.Type)
                            |> Option.defaultValue 0
                    VectorEntry.create vector priority))

    /// Sorts interacting particles by distance.
    let private sortInteractions (entries : _[][]) =
        seq {
            for i = 0 to entries.Length - 1 do
                let row = entries[i]
                assert(row.Length = i + 1)
                for j = 0 to i - 1 do
                    let entry = row[j]
                    if entry.Length <= repulsionRadius then
                        i, j, entry
        } |> Seq.sortBy (fun (_, _, entry) ->
            -entry.BondTypePriority, entry.Length)

    /// Creates bonds between closest particles.
    let private createBonds indexes particles =

            // reset bonds to zero
        let particles =
            particles
                |> Seq.map Particle.resetBonds
                |> ImmutableArray.Create<_>

        ((particles, Set.empty), indexes)
            ||> Seq.fold (fun (particles, bondSet) (i, j, _) ->
                let a = particles[i]
                let b = particles[j]
                let canBond =
                    a.NumBonds < a.Type.Valence
                        && b.NumBonds < b.Type.Valence
                if canBond then
                    let a, b = Particle.bond a b
                    let particles =
                        particles
                            .SetItem(i, a)
                            .SetItem(j, b)
                    assert(i > j)
                    let bondSet = bondSet.Add(i, j)
                    particles, bondSet
                else particles, bondSet)
            |> snd

    /// Calculates the force between two particles.
    let private getForce entry bonded =

            // repulsion
        let strength = entry.Repulsion

            // attraction between bonded particles?
        let strength =
            if bonded then
                strength - repulsionRadius / 2.0
            else strength

            // align to vector
        strength * (entry.Vector / entry.Length)

    /// Calculates the forces acting on a particle.
    let private getForces world (entries : _[][]) bondSet i =
        let row = entries[i]
        assert(row.Length = i + 1)
        Array.init world.Particles.Length (fun j ->
            if i = j then Point.Zero
            elif j < i then
                let entry = row[j]
                if entry.Length < repulsionRadius then
                    let bonded =
                        Set.contains (i, j) bondSet
                    getForce entry bonded
                else Point.Zero
            else
                let entry = entries[j][i]
                if entry.Length < repulsionRadius then
                    let bonded =
                        Set.contains (j, i) bondSet
                    -getForce entry bonded
                else Point.Zero)

    /// Gets the momentum of the given particle due to Brownian
    /// motion (using the Ornstein-Uhlenbeck process for
    /// smoothness.)
    let getMomentum random particle =
        let noise =
            sqrt dt
                * Point.create
                    (gaussianNoise random)
                    (gaussianNoise random)
        let delta = -brownianFriction * particle.Momentum * dt + brownianStrength * noise
        particle.Momentum + delta

    /// Moves a single particle one time step forward.
    let private stepParticle random world entries bondSet i =
        let particle = world.Particles[i]

            // force acting directly on particle's location
        let force =
            Array.sum (getForces world entries bondSet i)

            // update background momentum
        let momentum = getMomentum random particle

            // update location
        let location =
            let delta = force + momentum
            particle.Location + (delta * dt)
                |> clip world.Extent

        { particle with
            Location = location
            Momentum = momentum }

    /// Moves the particles in the given world one time step
    /// forward.
    let step random world =
        let entries =
            getVectors world.BondTypePriorityMap world.Particles
        let bondSet =
            let tuples = sortInteractions entries
            createBonds tuples world.Particles
        let particles =
            Array.init world.Particles.Length (
                stepParticle random world entries bondSet)
        { world with Particles = particles }
