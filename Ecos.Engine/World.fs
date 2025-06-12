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
    }

module World =

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
        atoms bonds =
        assert(extentMax.X >= extentMin.X)
        assert(extentMax.Y >= extentMin.Y)
        {
            ExtentMin = extentMin
            ExtentMax = extentMax
            Atoms = atoms
            Bonds = bonds
        }

    /// Creates a world.
    let create extentMin extentMax atoms =
        assert(
            Array.forall (fun atom ->
                atom.NumBonds = 0) atoms)
        createBound
            extentMin
            extentMax
            atoms
            (initBonds atoms.Length)

    /// Sorts attracted atoms.
    let private sortAttracted (interactions : _[][]) =
        [|
            for i = 0 to interactions.Length - 1 do

                let iaRow = interactions[i]
                assert(iaRow.Length = i)

                for j = 0 to i - 1 do
                    let ia : Interaction = iaRow[j]
                    if ia.DistanceSquared <= Interaction.bondDistanceSquared then
                        let key = ia.DistanceSquared
                        key, struct (i, j)
        |]
            |> Array.sortBy fst
            |> Array.map snd

    /// Creates bonds between closest atoms.
    let private createBonds world pairs =

            // reset bonds to zero
        let atoms =
            world.Atoms
                |> Array.map Atom.resetBonds
        let bonds = initBonds atoms.Length

            // examine each candidate bound pair
        for struct (i, j) in pairs do

            let atomA = atoms[i]
            let atomB = atoms[j]

                // bind atoms?
            let atomA, atomB, nBonds =
                Atom.tryBond atomA atomB
            if nBonds > 0 then

                atoms[i] <- atomA
                atoms[j] <- atomB

                    // mark pair as bound
                assert(i > j)
                assert(bonds[i][j] = 0)
                bonds[i][j] <- nBonds

        { world with
            Atoms = atoms
            Bonds = bonds }

    /// Calculates the total force acting on an atom.
    let private getForce world (interactions : _[][]) i =

        let iaRow = interactions[i]
        assert(iaRow.Length = i)

        let bondRow = world.Bonds[i]
        assert(bondRow.Length = i)

        let mutable total = Point.zero
        for j = 0 to world.Atoms.Length - 1 do
            let force =
                if i = j then Point.zero
                elif i > j then
                    let ia = iaRow[j]
                    let bound = bondRow[j] > 0
                    Interaction.getForce ia bound
                else
                    let ia = interactions[j][i]
                    let bound = world.Bonds[j][i] > 0
                    -Interaction.getForce ia bound
            total <- total + force
        total

    /// Bounces the given trajectory off a wall, if necessary.
    let private bounce
        world (location : Point) (velocity : Point) =

        let extentMin = world.ExtentMin
        let extentMax = world.ExtentMax

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

        Point.create vx vy

    module private Atom =

        /// Bounces the given atom off a wall, if necessary.
        let bounce world (atom : Atom) =
            let velocity =
                bounce world atom.Location atom.Velocity
            { atom with Velocity = velocity }

        /// Starts a half-step atom update.
        let startUpdate atom =
            atom
                |> Atom.updateHalfStepVelocity dt
                |> Atom.updateLocation dt

        /// Finishes a half-step atom update.
        let finishUpdate world interactions i =
            let atom = world.Atoms[i]
            let force = getForce world interactions i
            { atom with
                Acceleration = force / atom.Type.Mass }
                |> Atom.updateHalfStepVelocity dt
                |> bounce world

    /// Moves the atoms in the given world one time step
    /// forward using the Velocity Verlet algorithm.
    let step world =

            // start atom updates
        let atoms =
            Array.map Atom.startUpdate world.Atoms
        let world = { world with Atoms = atoms }

            // create bonds between atoms
        let ias =
            Interaction.getInteractions world.Atoms
        let world =
             ias
                |> sortAttracted
                |> createBonds world

            // finish atom updates
        let atoms =
            Array.init world.Atoms.Length (
                Atom.finishUpdate world ias)

        { world with Atoms = atoms }
