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
            Photons = Array.empty
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
    let private sortAttracted world (interactions : _[][]) =
        [|
            for i = 0 to interactions.Length - 1 do

                let iaRow = interactions[i]
                assert(iaRow.Length = i)

                let bondRow = world.Bonds[i]
                assert(bondRow.Length = i)

                for j = 0 to i - 1 do
                    let ia : Interaction = iaRow[j]
                    if ia.Distance <= Interaction.bondDistance then
                        let bound = bondRow[j] > 0
                        let key =
                            (if bound then 0 else 1), ia.Distance
                        key, struct (i, j, bound)
        |]
            |> Array.sortBy fst
            |> Array.map snd

    /// Emits a photon for the given atoms.
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

    /// Moves the photons in the given world one time step
    /// forward using the Velocity Verlet algorithm.
    let private stepAtoms world =

            // start atom updates
        let atoms =
            Array.map Atom.startUpdate world.Atoms
        let world = { world with Atoms = atoms }

            // create bonds between atoms
        let ias =
            Interaction.getInteractions world.Atoms
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

    module Choice =

        /// Unzips an array of choices.
        let unzip choices =
            let opts =
                choices
                    |> Array.map (function
                        | Choice1Of2 ch -> Some ch, None
                        | Choice2Of2 ch -> None, Some ch)
            Array.choose fst opts,
            Array.choose snd opts

    let private absorbPhotons world =

        let radius = Interaction.sigma / 1.5
        let pairs, photons =
            world.Photons
                |> Array.map (fun photon ->
                    let iAtomOpt =
                        world.Atoms
                            |> Array.tryFindIndex (fun atom ->
                                let distance =
                                    (photon.Location - atom.Location).Length
                                distance <= radius)
                    match iAtomOpt with
                        | Some iAtom -> Choice1Of2 (iAtom, photon)
                        | None -> Choice2Of2 photon)
                |> Choice.unzip

        let atomMap =
            pairs
                |> Seq.groupBy fst
                |> Seq.map (fun (iAtom, group) ->
                    let atom = world.Atoms[iAtom]
                    let photons = Seq.map snd group
                    iAtom, atom)
                |> Map

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
