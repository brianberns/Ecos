namespace Ecos.Engine

/// World of objects to animate.
type World =
    {
        /// Minimum extent point.
        ExtentMin : Point

        /// Maximum extent point.
        ExtentMax : Point

        /// Particles in the world.
        Particles : Particle[]

        /// Indexes of bound particles.
        Bonds : Set<int * int>
    }

module World =

    /// Repulsion strength.
    let repulsionStrength = 3.0

    /// Maximum distance at which repulsion occurs.
    let repulsionRadius = 0.9

    /// Attraction strength.
    let attractionStrength = 1.0

    /// Maximum distance at which attraction occurs.
    let attractionRadius = 1.0

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
            Bonds = Set.empty
        }

    /// Relationship between two particles.
    type private VectorEntry =
        {
            /// Distance between the particles.
            Distance : float

            /// Normalized vector between the particles.
            Vector : Point

            /// Repulsion between the particles.
            Repulsion : float

            /// Attraction between the particles.
            Attraction : float
        }

    module private VectorEntry =

        /// Creates a vector entry.
        let create (vector : Point) =
            let distance = vector.Length
            let norm = vector / distance

            let repulsion =
                if distance < repulsionRadius then
                    repulsionStrength
                        * (repulsionRadius - distance)
                        / repulsionRadius
                else 0.0

            let attraction =
                if distance < attractionRadius then
                    attractionStrength
                        * (attractionRadius - distance)
                        / attractionRadius
                else 0.0

            {
                Distance = distance
                Vector = norm
                Repulsion = repulsion
                Attraction = attraction
            }

        /// Zero vector entry.
        let zero =
            {
                Distance = 0.0
                Vector = Point.Zero
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
                    let other = particles[j]
                    let vector = particle.Location - other.Location
                    VectorEntry.create vector))

    /// Sorts attracted particles by distance.
    let private sortAttracted world (entries : _[][]) =
        seq {
            for i = 0 to entries.Length - 1 do
                let row = entries[i]
                assert(row.Length = i + 1)
                for j = 0 to i - 1 do
                    let entry = row[j]
                    if entry.Distance <= attractionRadius then
                        let bound = world.Bonds.Contains (i, j)
                        i, j, entry.Distance, bound
        } |> Seq.sortBy (fun (_, _, distance, bound) ->
            (if bound then 0 else 1), distance)

    /// Creates bonds between closest particles.
    let private createBonds world indexes =

            // reset bonds to zero
        let particles =
            world.Particles
                |> Seq.map Particle.resetBonds
                |> ImmutableArray.Create<_>

        let particles, bonds =
            ((particles, Set.empty), indexes)
                ||> Seq.fold (fun (particles, bonds) (i, j, _, bound) ->
                    let a = particles[i]
                    let b = particles[j]
                    let canBond =
                        a.NumBonds < a.Type.Valence
                            && b.NumBonds < b.Type.Valence
                    if canBond then
                        let a, b = Particle.bond a b (not bound)
                        let particles =
                            particles
                                .SetItem(i, a)
                                .SetItem(j, b)
                        assert(i > j)
                        let bonds = bonds.Add(i, j)
                        particles, bonds
                    else particles, bonds)

        { world with
            Particles = particles.Items
            Bonds = bonds }

    /// Calculates the force between two particles.
    let private getForce entry =

            // compute strength of force between the particles
        let strength =
            entry.Repulsion - entry.Attraction

            // align to normalized vector
        strength * entry.Vector

    /// Calculates the forces acting on a particle.
    let private getForces world (entries : _[][]) i =
        let row = entries[i]
        assert(row.Length = i + 1)
        Array.init world.Particles.Length (fun j ->
            if i = j then Point.Zero
            elif j < i then
                let entry = row[j]
                getForce entry
            else
                let entry = entries[j][i]
                -getForce entry)

    /// Bounces the given trajectory off a wall, if necessary.
    let private bounce world location velocity =
        let vx =
            if location.X < world.ExtentMin.X then
                abs velocity.X
            elif location.X > world.ExtentMax.X then
                -abs velocity.X
            else
                velocity.X
        let vy =
            if location.Y < world.ExtentMin.Y then
                abs velocity.Y
            elif location.Y > world.ExtentMax.Y then
                -abs velocity.Y
            else
                velocity.Y
        Point.create vx vy

    /// Moves a single particle one time step forward.
    let private stepParticle world entries i =
        let particle = world.Particles[i]
        let force =
            Array.sum (getForces world entries i)
        let velocity = particle.Velocity + force
        let location = particle.Location + (velocity * dt)
        let velocity = bounce world location velocity
        { particle with
            Location = location
            Velocity = velocity }

    /// Moves the particles in the given world one time step
    /// forward.
    let step world =
        let entries =
            getVectors world.Particles
        let world =
            let tuples = sortAttracted world entries
            createBonds world tuples
        let particles =
            Array.init world.Particles.Length (
                stepParticle world entries)
        { world with Particles = particles }
