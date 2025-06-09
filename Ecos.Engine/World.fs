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
        Bonds : int[(*i*)][(*j*)]   // i > j

        /// Photons in the world.
        Photons : Photon[]
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
            Array.replicate i 0)

    /// Creates a world.
    let createBound
        (extentMin : Point)
        (extentMax : Point)
        atoms
        bonds =
        assert(extentMax.X >= extentMin.X)
        assert(extentMax.Y >= extentMin.Y)
        {
            ExtentMin = extentMin
            ExtentMax = extentMax
            Atoms = atoms
            Bonds = bonds
            Photons = Array.empty
        }

    /// Creates a world.
    let create
        (extentMin : Point)
        (extentMax : Point)
        atoms =
        assert(
            Array.forall (fun atom ->
                atom.NumBonds = 0) atoms)
        createBound
            extentMin
            extentMax
            atoms
            (initBonds atoms.Length)

    /// Relationship between two atoms.
    [<Struct>]
    type private VectorEntry =

        /// Distance between the atoms.
        val Distance : float

        /// Repulsion between the atoms.
        val Repulsion : Point

        /// Possible attraction between the atoms.
        val Attraction : Point

        new(distance, repulsion, attraction) =
            {
                Distance = distance
                Repulsion = repulsion
                Attraction = attraction
            }

    module private VectorEntry =

        /// Repulsion numerator.
        let private cRepulsion = 48.0 * epsilon * pown sigma 12

        /// Attraction numerator.
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
        let create (atomA : Atom) (atomB : Atom) =
            let vector = atomA.Location - atomB.Location
            let distance = vector.Length
            if distance <= bondDistance then
                let norm = vector / distance
                let magRep, magAttr = getForce distance
                VectorEntry(distance, norm * magRep, norm * magAttr)
            else
                VectorEntry(distance, Point.zero, Point.zero)

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
        [|
            for i = 0 to entries.Length - 1 do

                let entryRow = entries[i]
                assert(entryRow.Length = i)

                let bondRow = world.Bonds[i]
                assert(bondRow.Length = i)

                for j = 0 to i - 1 do
                    let entry : VectorEntry = entryRow[j]
                    if entry.Distance <= bondDistance then
                        let bound = bondRow[j] > 0
                        let key =
                            (if bound then 0 else 1), entry.Distance
                        key, struct (i, j, bound)
        |]
            |> Array.sortBy fst
            |> Array.map snd

    /// Emits a photon for the given atoms.
    let private emitPhoton (atomA : Atom) (atomB : Atom) =

            // photon continues in same direction as atoms
        let direction =
            atomA.Velocity + atomB.Velocity

            // initial location
        let location =
            (atomA.Location + atomB.Location) / 2.0
                + direction   // prevent photon from being absorbed by these atoms

        Photon.create location direction

    /// Creates bonds between closest atoms.
    let private createBonds world tuples =

            // reset bonds to zero
        let atoms =
            world.Atoms
                |> Array.map Atom.resetBonds
        let bonds = initBonds atoms.Length
        let photons = ResizeArray()

            // examine each candidate bound pair
        for struct (i, j, bound) in tuples do

            let atomA = atoms[i]
            let atomB = atoms[j]

            let canBond =
                atomA.NumBonds < atomA.Type.Valence
                    && atomB.NumBonds < atomB.Type.Valence
            if canBond then

                    // bind atoms
                let radiate = not bound
                let atomA, atomB, nBonds =
                    Atom.bond atomA atomB radiate
                atoms[i] <- atomA
                atoms[j] <- atomB

                    // mark pair as bound
                assert(i > j)
                assert(bonds[i][j] = 0)
                bonds[i][j] <- nBonds

                    // emit photon?
                if radiate then
                    emitPhoton atomA atomB
                        |> photons.Add

        { world with
            Atoms = atoms
            Bonds = bonds
            Photons =
                [|
                    yield! world.Photons
                    yield! photons
                |] }

    /// Calculates the force between two atoms.
    let private getForce (entry : VectorEntry) bound =
        if bound then
            entry.Repulsion + entry.Attraction
        else
            entry.Repulsion

    /// Calculates the total force acting on an atom.
    let private getTotalForce world (entries : _[][]) i =

        let entryRow = entries[i]
        assert(entryRow.Length = i)

        let bondRow = world.Bonds[i]
        assert(bondRow.Length = i)

        let mutable total = Point.zero
        for j = 0 to world.Atoms.Length - 1 do
            let force =
                if i = j then Point.zero
                elif i > j then
                    let entry = entryRow[j]
                    let bound = bondRow[j] > 0
                    getForce entry bound
                else
                    let entry = entries[j][i]
                    let bound = world.Bonds[j][i] > 0
                    -getForce entry bound
            total <- total + force
        total

    /// Bounces the given atom off a wall, if necessary.
    let private bounce world (atom : Atom) =

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

    /// Starts a half-step atom update.
    let private startAtomUpdate atom =
        atom
            |> Atom.updateHalfStepVelocity dt
            |> Atom.updateLocation dt

    /// Finishes a half-step atom update.
    let private finishAtomUpdate world entries i =
        let atom = world.Atoms[i]
        let force = getTotalForce world entries i
        { atom with
            Acceleration = force / atom.Type.Mass }
            |> Atom.updateHalfStepVelocity dt
            |> bounce world

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

    /// Moves the photons in the given world one time step
    /// forward. Atoms are updated using the Velocity Verlet
    /// algorithm.
    let step world =

            // start atom updates
        let atoms =
            Array.map startAtomUpdate world.Atoms
        let world = { world with Atoms = atoms }

            // create bonds between atoms
        let entries = getVectors world.Atoms
        let world =
             entries
                |> sortAttracted world
                |> createBonds world

            // finish atom updates
        let atoms =
            Array.init world.Atoms.Length (
                finishAtomUpdate world entries)

            // update photons
        let photons =
            world.Photons
                |> Array.choose (stepPhoton world)

        { world with
            Atoms = atoms
            Photons = photons }
