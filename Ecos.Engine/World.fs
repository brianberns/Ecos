namespace Ecos.Engine

/// World of interacting atoms.
type World =
   {
        /// Minimum extent point.
        ExtentMin : Point

        /// Maximum extent point.
        ExtentMax : Point

        /// Bond potentials, indexed by atom type.
        Potentials : Potential[(*i*)][(*j*)]   // i > j

        /// Atoms in the world.
        Atoms : Atom[]

        /// Indexes of bound atoms.
        Bonds : int[(*i*)][(*j*)]   // i > j

        /// Photons in the world.
        Photons : Photon[]    }

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
        potentials atoms bonds =
        assert(extentMax.X >= extentMin.X)
        assert(extentMax.Y >= extentMin.Y)
        {
            ExtentMin = extentMin
            ExtentMax = extentMax
            Potentials = potentials
            Atoms = atoms
            Bonds = bonds
            Photons = Array.empty
        }

    /// Creates a world.
    let create extentMin extentMax potentials atoms =
        assert(
            Array.forall (fun atom ->
                atom.NumBonds = 0) atoms)
        createBound
            extentMin
            extentMax
            potentials
            atoms
            (initBonds atoms.Length)

    /// Sorts attracted atoms.
    let private sortAttracted world (interactions : _[][]) =
        [|
            for i = 0 to interactions.Length - 1 do

                let iaRow = interactions[i]
                assert(iaRow.Length = i)

                let bondRow = world.Bonds[i]
                assert(bondRow.Length = i)

                for j = 0 to i - 1 do
                    let ia : Interaction = iaRow[j]
                    let bondDistSquared =
                        pown ia.Potential.BondDistance 2
                    if ia.DistanceSquared <= bondDistSquared then
                        let key = ia.DistanceSquared
                        let bound = bondRow[j] > 0
                        key, struct (i, j, bound)
        |]
            |> Array.sortBy fst
            |> Array.map snd

    /// Emits a photon for the given atoms.Add commentMore actions
    let private emitPhoton
        (atomA : Atom) (atomB : Atom) energy =

            // photon continues in same direction as atoms
        let direction =
            atomA.Velocity + atomB.Velocity

            // initial location
        let location =
            (atomA.Location + atomB.Location) / 2.0
                + direction   // try to avoid photon being absorbed by these atoms

        Photon.create location direction energy

    /// Creates bonds between closest atoms.
    let private createBonds world tuples =

            // reset bonds to zero
        let atoms =
            world.Atoms
                |> Array.map Atom.resetBonds
        let bonds = initBonds atoms.Length
        let newPhotons = ResizeArray()

            // examine each candidate bound pair
        for struct (i, j, bound) in tuples do

            let atomA = atoms[i]
            let atomB = atoms[j]

                // bind atoms?
            let atomA, atomB, nBonds =
                Atom.tryBond atomA atomB
            if nBonds > 0 then

                    // emit photon?
                let atomA, atomB =
                    if bound then atomA, atomB
                    else
                        let atomA, atomB, energy =
                            Atom.radiate atomA atomB
                        emitPhoton atomA atomB energy
                            |> newPhotons.Add
                        atomA, atomB

                atoms[i] <- atomA
                atoms[j] <- atomB

                    // mark pair as bound
                assert(i > j)
                assert(bonds[i][j] = 0)
                bonds[i][j] <- nBonds

        { world with
            Atoms = atoms
            Bonds = bonds
            Photons =
                [|
                    yield! world.Photons
                    yield! newPhotons
                |] }

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

        /// The given atom absorbs the given photon.
        let absorb atom photon =
            let momentum =
                (photon.Energy / Photon.speed)
                    * (photon.Velocity / photon.Velocity.Length)
            let dVel = momentum / atom.Type.Mass
            { atom with Velocity = atom.Velocity + dVel }


    module private Photon =

        /// Bounces the given photon off a wall, if necessary.
        let bounce world (photon : Photon) =
            let velocity =
                bounce world photon.Location photon.Velocity
            { photon with Velocity = velocity }

        /// Moves a single photon one time step forward.
        let step world (photon : Photon) =
            let location =
                photon.Location + photon.Velocity * dt
            { photon with Location = location }
                |> bounce world

    /// Moves the atoms in the given world one time step
    /// forward using the Velocity Verlet algorithm.
    let stepAtoms world =

            // start atom updates
        let atoms =
            Array.map Atom.startUpdate world.Atoms
        let world = { world with Atoms = atoms }

            // create bonds between atoms
        let ias =
            Interaction.getInteractions
                world.Potentials
                world.Atoms
        let world =
             ias
                |> sortAttracted world
                |> createBonds world

            // finish atom updates
        let atoms =
            Array.init world.Atoms.Length (
                Atom.finishUpdate world ias)

        { world with Atoms = atoms }

    /// Moves the photons in the given world one time step
    /// forward.
    let private stepPhotons world =
        let photons =
            world.Photons
                |> Array.map (Photon.step world)
        { world with Photons = photons }

    module private Choice =

        /// Unzips an array of choices.
        let unzip choices =
            let opts =
                choices
                    |> Array.map (function
                        | Choice1Of2 ch -> Some ch, None
                        | Choice2Of2 ch -> None, Some ch)
            Array.choose fst opts,
            Array.choose snd opts

    let private absorptionRadius = 2.0 / 3.0

    /// Tries to find an atom near the given location.
    let private tryFindAtom radius world location =
        world.Atoms
            |> Array.tryFindIndex (fun atom ->
                let distance =
                    (location - atom.Location).Length
                distance <= radius)

    /// Absorbs photons in the given world.
    let private absorbPhotons world =

            // find atoms absorbing photons
        let pairs, photons =
            world.Photons
                |> Array.map (fun photon ->
                    let iAtomOpt =
                        tryFindAtom
                            absorptionRadius
                            world
                            photon.Location
                    match iAtomOpt with
                        | Some iAtom -> Choice1Of2 (iAtom, photon)
                        | None -> Choice2Of2 photon)
                |> Choice.unzip

            // absorb photons
        let atomMap =
            pairs
                |> Seq.groupBy fst
                |> Seq.map (fun (iAtom, group) ->
                    let atom = world.Atoms[iAtom]
                    let photons = Seq.map snd group
                    let atom = Seq.fold Atom.absorb atom photons
                    iAtom, atom)
                |> Map

            // update world
        let atoms =
            Array.init world.Atoms.Length (fun iAtom ->
                atomMap
                    |> Map.tryFind iAtom
                    |> Option.defaultValue world.Atoms[iAtom])
        { world with
            Atoms = atoms
            Photons = photons }

    /// Moves the objects in the given world one time step
    /// forward.
    let step world =
        world
            |> stepAtoms
            |> stepPhotons
            |> absorbPhotons
