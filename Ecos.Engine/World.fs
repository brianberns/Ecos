namespace Ecos.Engine

/// World of interacting entities.
type World =
    {
        /// Minimum extent point.
        ExtentMin : Point

        /// Maximum extent point.
        ExtentMax : Point

        /// Atoms in the world.
        Atoms : Atom[]

        /// Indexes of bound atoms.
        Bonds : bool[(*i*)][(*j*)]   // i > j

        /// Photons in the world.
        Photons : Photon[]
    }

module World =

    /// Repulsion strength.
    let repulsionStrength = 2.0

    /// Maximum distance at which repulsion occurs.
    let repulsionDistance = 0.9

    /// Attraction strength.
    let attractionStrength = 1.0

    /// Maximum distance at which attraction occurs
    /// for bound atoms.
    let attractionDistance = 1.2

    /// Maximum distance at which bonding occurs.
    let bondDistance = 1.0

    /// Attraction/repulsion equilibrium distance.
    let eqDistance =
        (repulsionStrength - attractionStrength)
            / ((repulsionStrength / repulsionDistance)
                - (attractionStrength / attractionDistance))

    /// Time step.
    let dt = 0.05

    /// Initializes empty symmetrical bond array.
    let private initBonds numAtoms =
        Array.init numAtoms (fun i ->
            Array.replicate i false)

    /// Creates a world.
    let create extentMin extentMax atoms =
        assert(extentMax.X >= extentMin.X)
        assert(extentMax.Y >= extentMin.Y)
        {
            ExtentMin = extentMin
            ExtentMax = extentMax
            Atoms = atoms
            Bonds = initBonds atoms.Length
            Photons = Array.empty
        }

    /// Relationship between two atoms.
    type private VectorEntry =
        {
            /// Distance between the atoms.
            Distance : float

            /// Normalized vector between the atoms.
            Vector : Point

            /// Repulsion between the atoms.
            Repulsion : float

            /// Possible attraction between the atoms.
            Attraction : float
        }

    module private VectorEntry =

        /// Creates a vector entry.
        let create (vector : Point) =
            let distance = vector.Length
            let norm = vector / distance

            let repulsion =
                if distance < repulsionDistance then
                    repulsionStrength
                        * (repulsionDistance - distance)
                        / repulsionDistance
                else 0.0

            let attraction =
                if distance < attractionDistance then
                    attractionStrength
                        * (attractionDistance - distance)
                        / attractionDistance
                else 0.0

            {
                Distance = distance
                Vector = norm
                Repulsion = repulsion
                Attraction = attraction
            }

    /// Calculates vector between every pair of atoms. The
    /// result is the lower half of a symmetric lookup table
    //// (up to sign).
    let private getVectors (atoms : Atom[]) =
        Array.init atoms.Length (fun i ->
            let atom = atoms[i]
            Array.init i (fun j ->
                assert(i >= j)   // lower half of table only
                let other = atoms[j]
                let vector = atom.Location - other.Location
                VectorEntry.create vector))

    /// Sorts attracted atoms by distance.
    let private sortAttracted world (entries : _[][]) =
        seq {
            for i = 0 to entries.Length - 1 do

                let entryRow = entries[i]
                assert(entryRow.Length = i)

                let bondRow = world.Bonds[i]
                assert(bondRow.Length = i)

                for j = 0 to i - 1 do
                    let entry = entryRow[j]
                    let bound = bondRow[j]
                    if (bound && entry.Distance <= attractionDistance)
                        || entry.Distance <= bondDistance then
                        let key =
                            (if bound then 0 else 1), entry.Distance
                        key, (i, j, bound)
        }
            |> Seq.sortBy fst
            |> Seq.map snd

    /// Creates bonds between closest atoms.
    let private createBonds world tuples =

            // reset bonds to zero
        let atoms =
            world.Atoms
                |> Array.map Atom.resetBonds
        let bonds = initBonds atoms.Length
        let photons = ResizeArray()

            // examine each candidate bound pair
        for i, j, bound in tuples do

            let atomA = atoms[i]
            let atomB = atoms[j]

            let canBond =
                atomA.NumBonds < atomA.Type.Valence
                    && atomB.NumBonds < atomB.Type.Valence
            if canBond then

                    // radiate a photon when creating a new bond
                let radiate = not bound
                let atomA, atomB =
                    Atom.bond atomA atomB radiate
                atoms[i] <- atomA
                atoms[j] <- atomB

                    // mark pair as bound
                assert(i > j)
                bonds[i][j] <- true

                    // create photon?
                if radiate then
                    let location =
                        (atomA.Location + atomB.Location)
                             / 2.0
                    let direction =
                        atomA.Velocity + atomB.Velocity
                    let photon =
                        Photon.create location direction
                    photons.Add(photon)

        { world with
            Atoms = atoms
            Bonds = bonds
            Photons =
                [|
                    yield! world.Photons
                    yield! photons
                |] }

    /// Calculates the force between two atoms.
    let private getForce entry bound =

            // compute strength of force between the atoms
        let strength =
            if bound then
                assert(entry.Attraction > 0.0)
                entry.Repulsion - entry.Attraction
            else
                entry.Repulsion

            // align to normalized vector
        strength * entry.Vector

    /// Calculates the forces acting on an atom.
    let private getForces world (entries : _[][]) i =

        let entryRow = entries[i]
        assert(entryRow.Length = i)

        let bondRow = world.Bonds[i]
        assert(bondRow.Length = i)

        Array.init world.Atoms.Length (fun j ->
            if i = j then Point.Zero
            elif i > j then
                let entry = entryRow[j]
                let bound = bondRow[j]
                getForce entry bound
            else
                let entry = entries[j][i]
                let bound = world.Bonds[j][i]
                -getForce entry bound)

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

    /// Moves a single atom one time step forward.
    let private stepAtom world entries i =
        let atom = world.Atoms[i]
        let force =
            Array.sum (getForces world entries i)
        let velocity = atom.Velocity + force
        let location = atom.Location + (velocity * dt)
        let velocity = bounce world location velocity
        { atom with
            Location = location
            Velocity = velocity }

    /// Moves a single photon one time step forward.
    let private stepPhoton world (photon : Photon) =
        let location =
            photon.Location + photon.Velocity * dt
        let valid =
            location.X >= world.ExtentMin.X
                && location.X <= world.ExtentMax.X
                && location.Y >= world.ExtentMin.Y
                && location.Y <= world.ExtentMax.Y
        if valid then
            Some { photon with Location = location }
        else None

    /// Moves the atoms in the given world one time step
    /// forward.
    let step world =
        let entries = getVectors world.Atoms
        let world =
            entries
                |> sortAttracted world
                |> createBonds world
        let atoms =
            Array.init world.Atoms.Length (
                stepAtom world entries)
        let photons =
            world.Photons
                |> Array.choose (stepPhoton world)
        { world with
            Atoms = atoms
            Photons = photons }
