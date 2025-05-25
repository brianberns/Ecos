namespace Ecos

/// World of objects to animate.
type World =
    {
        /// Minimum extent point.
        ExtentMin : Point

        /// Maximum extent point.
        ExtentMax : Point

        /// Particles in the world.
        Particles : Particle[]
    }

module World =

    /// Repulsion strength.
    let repulsionStrength = 1.0

    /// Maximum distance at which repulsion occurs.
    let repulsionRadius = 1.0

    /// Attraction strength.
    let attractionStrength = 0.5

    /// Maximum distance at which attraction occurs.
    let attractionRadius = 2.0

    /// Friction.
    let friction = 0.99

    /// Time step.
    let dt = 0.05

    /// Creates a world.
    let create extentMin extentMax particles =
        assert(extentMax.X >= extentMin.X)
        assert(extentMax.Y >= extentMin.Y)
        {
            ExtentMin = extentMin
            ExtentMax = extentMax
            Particles = particles
        }

    /// Relationship between two particles.
    type private VectorEntry =
        {
            /// Vector between the particles.
            Vector : Point

            /// Length of the vector.
            Length : float

            /// Repulsion between the particles.
            Repulsion : float

            /// Possible attraction between the particles.
            Attraction : float
        }

    module private VectorEntry =

        /// Creates a vector entry.
        let create (vector : Point) =

            let length = vector.Length

            let repulsion =
                if length < repulsionRadius then
                    repulsionStrength
                        * (repulsionRadius - length)
                        / repulsionRadius
                else 0.0

            let attraction =
                if length < attractionRadius then
                    attractionStrength
                        * (attractionRadius - length)
                        / attractionRadius
                else 0.0

            {
                Vector = vector
                Length = length
                Repulsion = repulsion
                Attraction = attraction
            }

        /// Zero vector entry.
        let zero =
            {
                Vector = Point.Zero
                Length = 0.0
                Repulsion = 0.0
                Attraction = 0.0
            }

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
                let row = entries[i]
                assert(row.Length = i + 1)
                for j = 0 to i - 1 do
                    let entry = row[j]
                    assert(attractionRadius >= repulsionRadius)
                    if entry.Length <= attractionRadius then
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
            ||> Seq.fold (fun (particles, bondSet) (i, j, _) ->
                let a = particles[i]
                let b = particles[j]
                let canBond =
                    a.Type.Valence > a.NumBonds
                        && b.Type.Valence > b.NumBonds
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

            // compute strength of force between the particles
        let strength =
            if bonded then
                entry.Repulsion - entry.Attraction
            else
                entry.Repulsion

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
                let bonded =
                    Set.contains (i, j) bondSet
                getForce entry bonded
            else
                let entry = entries[j][i]
                let bonded =
                    Set.contains (j, i) bondSet
                -getForce entry bonded)

    let private bounce world location velocity =
        let vx =
            if location.X < world.ExtentMin.X then
                abs velocity.X
            elif location.X > world.ExtentMax.X then
                -(abs velocity.X)
            else
                velocity.X
        let vy =
            if location.Y < world.ExtentMin.Y then
                abs velocity.Y
            elif location.Y > world.ExtentMax.Y then
                -(abs velocity.Y)
            else
                velocity.Y
        Point.create vx vy

    /// Moves a single particle one time step forward.
    let private stepParticle random world entries bondSet i =
        let particle = world.Particles[i]
        let force =
            Array.sum (getForces world entries bondSet i)
        let velocity = (particle.Velocity + force) * friction
        let location = particle.Location + (velocity * dt)
        let velocity = bounce world location velocity
        { particle with
            Location = location
            Velocity = velocity }

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
