namespace Ecos.Engine

/// World of interacting atoms.
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
    }

module World =

    /// Depth of potential energy well.
    let epsilon = 1.0

    /// Equilibrium distance.
    let sigma = 1.0

    /// Maximum distance at which bonding occurs.
    let bondDistance = 2.0 * sigma

    /// Time step.
    let dt = 0.01

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
        }

    /// Relationship between two atoms.
    type private VectorEntry =
        {
            /// Distance between the atoms.
            Distance : float

            /// Repulsion between the atoms.
            Repulsion : Point

            /// Possible attraction between the atoms.
            Attraction : Point
        }

    module private VectorEntry =

        let private cRepulsion = 48.0 * epsilon * pown sigma 12
        let private cAttraction = -24.0 * epsilon * pown sigma 6

        /// Repulsion and attraction magnitudes for the given
        /// distance. (Lennard-Jones potential.)
        let getForce distance =
            let six = pown distance 6
            let seven = distance * six
            let thirteen = six * seven
            cRepulsion / thirteen,
            cAttraction / seven

        /// Creates a vector entry.
        let create atomA atomB =
            let vector = atomA.Location - atomB.Location
            let distance = vector.Length
            let norm = vector / distance
            let repulsion, attraction = getForce distance
            {
                Distance = distance
                Repulsion = norm * repulsion
                Attraction = norm * attraction
            }

    /// Calculates vector between every pair of atoms. The
    /// result is the lower half of a symmetric lookup table
    //// (up to sign).
    let private getVectors (atoms : Atom[]) =
        Array.init atoms.Length (fun i ->
            let atom = atoms[i]
            Array.init i (fun j ->
                assert(i >= j)   // lower half of table only
                VectorEntry.create atom atoms[j]))

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
                    if entry.Distance <= bondDistance then
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

            // examine each candidate bound pair
        for i, j, bound in tuples do

            let atomA = atoms[i]
            let atomB = atoms[j]

            let canBond =
                atomA.NumBonds < atomA.Type.Valence
                    && atomB.NumBonds < atomB.Type.Valence
            if canBond then

                    // bind atoms
                let atomA, atomB =
                    Atom.bond atomA atomB (not bound)
                atoms[i] <- atomA
                atoms[j] <- atomB

                    // mark pair as bound
                assert(i > j)
                bonds[i][j] <- true

        { world with
            Atoms = atoms
            Bonds = bonds }

    /// Calculates the force between two atoms.
    let private getForce entry bound =
        if bound then
            entry.Repulsion + entry.Attraction
        else
            entry.Repulsion

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

    /// Bounces the given atom off a wall, if necessary.
    let private bounce world atom =

        let extentMin = world.ExtentMin
        let extentMax = world.ExtentMax
        let location = atom.Location
        let velocity = atom.Velocity

        let vx =
            if location.X < extentMin.X then
                abs velocity.X
            elif location.X > extentMax.X then
                -abs velocity.X
            else
                velocity.X

        let vy =
            if location.Y < extentMin.Y then
                abs velocity.Y
            elif location.Y > extentMax.Y then
                -abs velocity.Y
            else
                velocity.Y

        let velocity = Point.create vx vy
        { atom with Velocity = velocity }

    let private stepAtom world entries i =
        let atom = world.Atoms[i]
        let force =
            Array.sum (getForces world entries i)
        { atom with Acceleration = force }   // assume mass of atom = 1.0
            |> Atom.updateHalfStepVelocity dt
            |> bounce world

    /// Moves the atoms in the given world one time step
    /// forward.
    let step world =

            // update atom locations using half-step velocities
        let atoms =
            Array.map (
                Atom.updateHalfStepVelocity dt
                    >> Atom.updateLocation dt)
                world.Atoms
        let world = { world with Atoms = atoms }

            // create bonds between atoms
        let entries = getVectors world.Atoms
        let world =
             entries
                |> sortAttracted world
                |> createBonds world

            // update atom accelerations
        let atoms =
            Array.init world.Atoms.Length (
                stepAtom world entries)
        { world with Atoms = atoms }
