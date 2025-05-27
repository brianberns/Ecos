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

        /// Indexes of bonded particles.
        Bonds : Set<int * int>
    }

module World =

    /// Repulsion strength.
    let repulsionStrength = 2.0

    /// Maximum distance at which repulsion occurs.
    let repulsionRadius = 1.0

    /// Attraction strength.
    let attractionStrength = 1.0

    /// Maximum distance at which attraction occurs.
    let attractionRadius = 2.0

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
            /// Normalized vector between the particles.
            Vector : Point

            /// Repulsion between the particles.
            Repulsion : float

            /// Possible attraction between the particles.
            Attraction : float
        }

    module private VectorEntry =

        /// Creates a vector entry.
        let create (vector : Point) =
            let length = vector.Length
            let norm = vector / length

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
                Vector = norm
                Repulsion = repulsion
                Attraction = attraction
            }

        /// Zero vector entry.
        let zero =
            {
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

    /// Sorts interacting particles by distance.
    let private sortInteractions (entries : _[][]) =
        seq {
            for i = 0 to entries.Length - 1 do
                let row = entries[i]
                assert(row.Length = i + 1)
                for j = 0 to i - 1 do
                    let entry = row[j]
                    assert(attractionRadius >= repulsionRadius)
                    if entry.Attraction > 0.0 then
                        i, j, entry
        } |> Seq.sortByDescending (fun (_, _, entry) ->
            entry.Attraction)

    /// Creates bonds between closest particles.
    let private createBonds indexes world =

            // reset bonds to zero
        let particles =
            world.Particles
                |> Seq.map Particle.resetBonds
                |> ImmutableArray.Create<_>

        let particles, bonds =
            ((particles, Set.empty), indexes)
                ||> Seq.fold (fun (particles, bonds) (i, j, _) ->
                    let a = particles[i]
                    let b = particles[j]
                    let canBond =
                        a.NumBonds < a.Type.Valence
                            && b.NumBonds < b.Type.Valence
                    if canBond then
                        let bonded = world.Bonds.Contains (i, j)
                        let a, b = Particle.bond a b (not bonded)
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
    let private getForce entry bonded =

            // compute strength of force between the particles
        let strength =
            if bonded then
                entry.Repulsion - entry.Attraction
            else
                entry.Repulsion

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
                let bonded = world.Bonds.Contains (i, j)
                getForce entry bonded
            else
                let entry = entries[j][i]
                let bonded = world.Bonds.Contains (j, i)
                -getForce entry bonded)

    /// Wraps a location around the edges of the given world.
    let private wrap world location =
        let x =
            if location.X < world.ExtentMin.X then
                world.ExtentMax.X - (world.ExtentMin.X - location.X)
            elif location.X > world.ExtentMax.X then
                world.ExtentMin.X + (location.X - world.ExtentMax.X)
            else
                location.X
        let y =
            if location.Y < world.ExtentMin.Y then
                world.ExtentMax.Y - (world.ExtentMin.Y - location.Y)
            elif location.Y > world.ExtentMax.Y then
                world.ExtentMin.Y + (location.Y - world.ExtentMax.Y)
            else
                location.Y
        Point.create x y

    /// Moves a single particle one time step forward.
    let private stepParticle world entries i =
        let particle = world.Particles[i]
        let force =
            Array.sum (getForces world entries i)
        let velocity = particle.Velocity + force
        let location = particle.Location + (velocity * dt)
        let location = wrap world location
        { particle with
            Location = location
            Velocity = velocity }

    /// Moves the particles in the given world one time step
    /// forward.
    let step world =
        let entries =
            getVectors world.Particles
        let world =
            let tuples = sortInteractions entries
            createBonds tuples world
        let particles =
            Array.init world.Particles.Length (
                stepParticle world entries)
        { world with Particles = particles }
