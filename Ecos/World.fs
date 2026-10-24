namespace Ecos

open System

/// A lattice world containing atoms bonded into molecules.
/// Every atom occupies a 2x2 square around its central
/// location on the lattice, so in the drawing below, the
/// ╋ symbol indicates the location of the atom:
///
///    ┏━┳━┓
///    ┣━╋━┫
///    ┗━┻━┛
///
/// Behavior is based on the Bond Fluctuation Model with:
/// * Metropolis dynamics
/// * Pairing of complementary strands
/// * Stacking of bonded pairs (a la DNA)
///
/// Walls are immovable obstacles, each the size of an atom.
type World =
    private {

        /// Atoms in the world, indexed by their IDs.
        _Atoms : Atom[]

        /// Locations of walls in the world.
        _Walls : Point[]

        /// Bonds between atoms in the world, by type.
        _BondTypeMap : BondTypeMap

        /// Occupant of each cell in the world.
        _Cells : Grid
    }

    /// Atoms in the world, indexed by their IDs.
    member this.Atoms = this._Atoms

    /// Locations of walls in the world.
    member this.Walls = this._Walls

    /// Bonds between atoms in the world, by type.
    member this.BondTypeMap = this._BondTypeMap

    /// Occupant of each cell in the world.
    member this.Cells = this._Cells

    /// Size of this world.
    member this.Size = this.Cells.Size

    /// Gets the occupant of the given location in this world.
    member this.Item(x, y) = this.Cells[x, y]

    /// Gets the occupant of the given location in this world.
    member this.Item(location) = this.Cells[location]

    /// Normalizes the given location onto this world.
    member this.NormalizeLocation(location) =
        this.Cells.NormalizeLocation(location)

/// An event in the world.
type Event =

    /// An atom moved.
    | MoveAtom of AtomId

    /// A bond was created between two atoms.
    | CreateBond of BondType * AtomId * AtomId

    /// A bond was broken between two atoms.
    | BreakBond of BondType * AtomId * AtomId

module World =

    /// Creates an empty world of the given size.
    let create size =
        assert(size.X % 2 = 0)   // must be even because atoms are 2x2
        assert(size.Y % 2 = 0)
        {
            _Atoms = Array.empty
            _Walls = Array.empty
            _BondTypeMap = BondTypeMap.empty
            _Cells = Grid.create size
        }

    /// Creates an empty square world of the given size.
    let createSquare size =
        create (Point.create(size, size))

    /// Creates a copy of the given world with its own mutable
    /// data structures. This is used to protect against mutating
    /// shared data unexpectedly.
    let private copy world =
        {
            world with
                _Atoms = Array.copy world._Atoms
                _Cells = Grid.copy world.Cells
        }

    /// Moore neighborhood deltas, including center.
    let private mooreDeltas =
        [|
            for dx = -1 to 1 do
                for dy = -1 to 1 do
                    Point.create (dx, dy)
        |]

    /// Determines whether the given location is available.
    let private isAvailable loc idOpt (world : World) =
        mooreDeltas
            |> Seq.forall (fun delta ->
                match world[loc + delta] with
                    | Cell.Atom id -> Some id = idOpt   // an atom can occupy its own location
                    | Cell.Wall -> false
                    | Cell.Empty -> true)

    /// Adds an atom with the given attributes to the given world
    /// if it is valid.
    let tryAddAtom atomType location (world : World) =
        let location = world.NormalizeLocation(location)
        if isAvailable location None world then
            let world = copy world
            let id = world.Atoms.Length
            let atom = Atom.create id atomType location
            let world =
                { world with
                    _Atoms = [| yield! world.Atoms; atom |]
                    _Cells = Grid.setAtom location id world.Cells }   // MUTATION
            Some (world, atom)
        else None

    /// Adds an atom with the given attributes to the given world.
    let addAtom atomType location world =
        match tryAddAtom atomType location world with
            | Some (world, _) -> world
            | None -> failwith "Invalid location"

    /// Adds a wall to the given world if it is valid.
    let tryAddWall location (world : World) =
        let location = world.NormalizeLocation(location)
        if isAvailable location None world then
            let world = copy world
            Some
                { world with
                    _Walls = [| yield! world.Walls; location |]
                    _Cells = Grid.setWall location world.Cells }    // MUTATION
        else None

    /// Adds a wall to the given world.
    let addWall location world =
        match tryAddWall location world with
            | Some world -> world
            | None -> failwith "Invalid location"

    /// Answers the IDs of all atoms bonded with the given atom.
    let private getAllBonds id (world : World) =
        world.BondTypeMap.BondMaps
            |> Seq.collect (BondMap.getBonds id)

    /// Indicates whether the atoms with the given IDs are already
    /// bonded in the given world.
    let private areBonded idA idB (world : World) =
        world.BondTypeMap.BondMaps
            |> Seq.exists (BondMap.areBonded idA idB)

    /// Behavior of each bond type.
    let private behaviorMap =
        Map [
            Strong, StrongBond.behavior
            Weak, WeakBond.behavior
        ]

    /// The shortest vector from atom A to atom B in the given
    /// world, accounting for possible edge wrapping.
    let getDisplacement atomA atomB (world : World) =
        Grid.getDisplacement
            atomA.Location
            atomB.Location
            world.Cells

    /// Determines whether bonds between the given atoms cross.
    /// Since the atoms live on a torus, we need to view them
    /// from a common reference point.
    let private testCrossBond (atomA, atomB) (atomC, atomD) world =

            // shift the given atom to atom A's frame
        let shift atom =
            getDisplacement atomA atom world

        assert(shift atomA = Point.zero)
        BondBehavior.testCrossBond
            (Point.zero, shift atomB)
            (shift atomC, shift atomD)

    /// Indicates whether a bond of the given type between the
    /// given atoms crosses any other bonds. This only needs to
    /// check the local neighborhood of one of the atoms.
    let private isCrossBond behavior atomA atomB (world : World) =
        behavior.CrossBondDeltas
            |> Seq.exists (fun delta ->
                match world[atomA.Location + delta] with  // check A's neighborhood
                    | Cell.Atom idC ->
                        if idC = atomA.Id || idC = atomB.Id then
                            false
                        else
                            let atomC = world.Atoms[idC]
                            getAllBonds idC world
                                |> Seq.exists (fun idD ->
                                    if idD = atomA.Id
                                        || idD = atomB.Id then false
                                    else
                                        let atomD = world.Atoms[idD]
                                        testCrossBond
                                            (atomA, atomB) (atomC, atomD) world)
                    | _ -> false)

    /// A -- B
    ///  \
    ///   \
    ///    C
    let private testBend behavior bendLimit atomA atomB atomC world =
        let deltaAB = getDisplacement atomA atomB world   // vector from A to B
        let deltaAC = getDisplacement atomA atomC world   // vector from A to C
        behavior.TestBend bendLimit deltaAB deltaAC

    /// A -?- B
    ///  \
    ///   \
    ///    C
    let private testBends
        behavior bendLimit atomA atomB idsC (world : World) =
        Seq.forall (fun idC ->
            let atomC = world.Atoms[idC]
            testBend behavior bendLimit atomA atomB atomC world)
            idsC

    /// Energy of a bend between the given bond vectors, which start at
    /// the same atom: zero when they point in opposite directions (a
    /// straight strand).
    let private getBendEnergy kappa (u : Point) (v : Point) =
        let dot = float (u .* v)
        kappa * (1.0 + dot / sqrt (float (u .* u) * float (v .* v)))

    /// Bend energy that a bond of the given type between atoms A and B
    /// adds to the given world, at A and at B, whether or not the bond
    /// exists yet.
    let private getBondBendEnergy kappa bondType atomA atomB (world : World) =
        let bondMap = world.BondTypeMap[bondType]
        let at (atomA : Atom) (atomB : Atom) =
            let deltaAB = getDisplacement atomA atomB world
            BondMap.getBonds atomA.Id bondMap
                |> Seq.sumBy (fun idC ->
                    if idC = atomB.Id then 0.0
                    else
                        let deltaAC =
                            getDisplacement atomA world.Atoms[idC] world
                        getBendEnergy kappa deltaAB deltaAC)
        at atomA atomB + at atomB atomA

    /// Bend energy between bonds of the given type at the given atom
    /// and its partners of that type, with the given atom at its given
    /// location.
    let private getLocalBendEnergy kappa bondType (atom : Atom) (world : World) =
        let bondMap = world.BondTypeMap[bondType]
        let getAtom id = if id = atom.Id then atom else world.Atoms[id]
        let at id =
            let atomM = getAtom id
            let deltas =
                BondMap.getBonds id bondMap
                    |> Seq.map (fun idC ->
                        getDisplacement atomM (getAtom idC) world)
                    |> Seq.toArray
            seq {
                for i in 0 .. deltas.Length - 1 do
                    for j in i + 1 .. deltas.Length - 1 do
                        getBendEnergy kappa deltas[i] deltas[j]
            } |> Seq.sum
        at atom.Id
            + (BondMap.getBonds atom.Id bondMap |> Seq.sumBy at)

    /// Adds a bond of the given type between the given atoms in
    /// the given world if it is valid.
    let private tryAddBond bondType bondParms atomA atomB (world : World) =

            // get existing bond map
        let bondMap = world.BondTypeMap[bondType]

            // create bond provisionally                
        let bondTypeMap =
            let bondMap =
                BondMap.createBond atomA.Id atomB.Id bondMap
            BondTypeMap.setBondMap bondType bondMap world.BondTypeMap

            // new bond is valid?
        let isValid =
            let behavior = behaviorMap[bondType]
            let idsA = BondMap.getBonds atomA.Id bondMap
            let idsB = BondMap.getBonds atomB.Id bondMap
            let delta = getDisplacement atomA atomB world
            let bendLimit = bondParms.BendLimit
            behavior.AreCompatible atomA.Type atomB.Type
                && not (areBonded atomA.Id atomB.Id world)
                && Neighborhood.testBond delta behavior.Neighborhood
                && behavior.TestValence atomA bondTypeMap
                && behavior.TestValence atomB bondTypeMap
                && not (isCrossBond behavior atomA atomB world)
                && testBends behavior bendLimit atomA atomB idsA world
                && testBends behavior bendLimit atomB atomA idsB world

        if isValid then
            Some { world with _BondTypeMap = bondTypeMap }
        else None

    /// Adds a strong bond between the given atoms in the given
    /// world if it is valid.
    let tryAddStrongBond parms idA idB (world : World) =
        tryAddBond Strong parms.BondParameterMap[Strong]
            world.Atoms[idA] world.Atoms[idB] world

    /// Adds a strong bond between the given atoms in the given
    /// world.
    let addStrongBond parms idA idB world =
        match tryAddStrongBond parms idA idB world with
            | Some world -> world
            | None -> failwith "Invalid bond"

    /// Adds a weak bond between the given atoms in the given
    /// world if it is valid.
    let tryAddWeakBond parms idA idB (world : World) =
        tryAddBond Weak parms.BondParameterMap[Weak]
            world.Atoms[idA] world.Atoms[idB] world

    /// Adds a weak bond between the given atoms in the given
    /// world.
    let addWeakBond parms idA idB world =
        match tryAddWeakBond parms idA idB world with
            | Some world -> world
            | None -> failwith "Invalid bond"

    /// Cardinal directions.
    let private cardinalDirections =
        [|
            ( 1,  0)
            ( 0, -1)
            (-1,  0)
            ( 0,  1)
        |] |> Array.map Point.create

    /// Chooses a random cardinal direction.
    let private getRandomDirection (rng : Random) =
        cardinalDirections[
            rng.Next(cardinalDirections.Length)]

    /// Determines whether the given atom, which has been moved
    /// by the given unit delta, is now in an unoccupied 
    /// location. Only the three cells that the delta brings into
    /// the atom's neighborhood need checking, since (we assume)
    /// the world was valid before the move.
    let private canMove atom delta (world : World) =
        assert(world[atom.Location - delta] = Cell.Atom atom.Id)
        assert(Array.contains delta cardinalDirections)

        let inline check location =
            world[location].IsEmpty

        let result =
            let ahead = atom.Location + delta
            if check ahead then
                let perpendicular = Point.create (delta.Y, delta.X)   // (1, 0) <-> (0, 1)
                check (ahead + perpendicular)
                    && check (ahead - perpendicular)
            else false
        assert(
            result =
                isAvailable atom.Location (Some atom.Id) world)
        result

    /// Tests bond angles in which the atom is central: C-A-B.
    let private testCentralBonds
        behavior bendLimit atom (partnerIds : _[]) (world : World) =
        seq {
            for i = 0 to partnerIds.Length - 2 do
                for j = i + 1 to partnerIds.Length - 1 do
                    partnerIds[i], partnerIds[j]
        }
            |> Seq.forall (fun (idB, idC) ->
                let atomB = world.Atoms[idB]
                let atomC = world.Atoms[idC]
                testBend behavior bendLimit atom atomB atomC world)

    /// Tests angles in which the atom isn't central: A-B-C.
    let private testPeripheralBonds
        behavior bendLimit atom bondMap partnerIds (world : World) =
        partnerIds
            |> Array.forall (fun idB ->
                let atomB = world.Atoms[idB]
                BondMap.getBonds idB bondMap
                    |> Set.forall (fun idC ->
                        idC = atom.Id
                            || testBend behavior bendLimit atomB atom (world.Atoms[idC]) world))

    /// Determines whether the given atom's bonds are valid.
    let private testBonds parms atom (world : World) =
        behaviorMap
            |> Map.forall (fun bondType behavior ->
                let bendLimit = parms.BondParameterMap[bondType].BendLimit

                    // test the atom's bonds
                let bondMap = world.BondTypeMap[bondType]
                let partnerIds = BondMap.getBonds atom.Id bondMap
                if partnerIds.IsEmpty then true   // performance optimization: early exit if unbonded
                else
                        // test bond deltas
                    let isValid =
                        partnerIds
                            |> Set.forall (fun idB ->
                                let atomB = world.Atoms[idB]
                                let delta = getDisplacement atom atomB world
                                Neighborhood.testBond
                                    delta behavior.Neighborhood)

                        // test bond angles?
                    if isValid then
                        let partnerIds = Seq.toArray partnerIds
                        testCentralBonds
                            behavior bendLimit atom partnerIds world
                            && testPeripheralBonds
                                behavior bendLimit atom bondMap partnerIds world
                    else false)

    /// Applies the Metropolis rule:
    /// * A move that lowers the energy, or leaves it unchanged,
    ///   is always accepted.
    /// * An uphill move is accepted with probability exp(s − ΔE/kT),
    ///   where kT is temperature scaled by Boltzmann's constant
    ///   and s is entropy.
    let private testMetropolis
        (rng : Random)
        (kT : float)
        (entropy : float)
        (energyDelta : float) =
        let x = entropy - energyDelta / kT
        x >= 0 || rng.NextDouble() < exp x

    /// Updates the given atom's location in the given world.
    let private moveAtom atomBefore atomAfter (world : World) =
        assert(atomBefore.Id = atomAfter.Id)
        assert(world.Atoms[atomBefore.Id] = atomBefore)
        assert(world[atomBefore.Location] = Cell.Atom atomBefore.Id)

        let id = atomBefore.Id

            // MUTATION
        world._Atoms[id] <- atomAfter   // replaces `atomBefore`

            // MUTATION
        let cells =
            world.Cells
                |> Grid.setEmpty atomBefore.Location
                |> Grid.setAtom atomAfter.Location id

        { world with _Cells = cells }

    /// Applies the Metropolis rule to the change in bend energy from
    /// moving the given atom, if bends cost energy.
    let private testBendEnergy parms rng atomBefore atomAfter (world : World) =
        let stiffBondTypes =
            BondType.all
                |> Array.choose (fun bondType ->
                    let kappa = parms.BondParameterMap[bondType].BendStiffness
                    if kappa > 0.0 then Some (bondType, kappa) else None)
        if stiffBondTypes.Length = 0 then true
        else
            let energyDelta =
                stiffBondTypes
                    |> Array.sumBy (fun (bondType, kappa) ->
                        getLocalBendEnergy kappa bondType atomAfter world
                            - getLocalBendEnergy kappa bondType atomBefore world)
            let temp =
                Parameters.getAvgTemperature atomBefore atomAfter parms
            testMetropolis rng temp 0.0 energyDelta

    /// Attempts to move an atom in the given world.
    let private tryUpdateAtom parms (rng : Random) (world : World) =

            // pick atom to move
        let id = rng.Next(world.Atoms.Length)
        let atom = world.Atoms[id]

            // pick direction to move in
        let delta = getRandomDirection rng

            // move atom in that direction provisionally
        let atom' =
            let loc = world.NormalizeLocation(atom.Location + delta)
            { atom with Location = loc }

            // move the atom permanently?
        let isValid =
            canMove atom' delta world                          // avoids overlap with other atoms?
                && testBonds parms atom' world                 // all of the atom's bonds are still valid?
                && testBendEnergy parms rng atom atom' world   // bends, if they cost energy, accepted?
        if isValid then
            let world = moveAtom atom atom' world
            let event = MoveAtom id
            Some (world, event)
        else None

    /// Additional energy from a stacked weak bond.
    let private stackEnergy = 1.0

    /// Applies Metropolis tests to the given bond energy.
    let private testBondEnergy
        rng kT energyBarrier reduction startBarrier nStacks entropy energy =
        let barrier =
            (max 0.0 (energyBarrier - reduction * float nStacks))
                + startBarrier
        testMetropolis rng kT 0 barrier
            && testMetropolis rng kT entropy energy

    /// Determines whether to peel the given bond.
    let private testPeel
        (rng : Random) parms temp behavior atomA atomB bondMap =
        if behavior.TestPeel atomA atomB bondMap then
            rng.NextDouble() <= exp (-parms.PeelBarrier / temp)
        else false

    /// Determines whether to break a bond by hydrolysis.
    let private testHydrolysis (rng : Random) bondParms =
        rng.NextDouble() < bondParms.HydrolysisRate

    /// Attempts to break the bond of the given type between the
    /// given atoms in the given world.
    let private tryBreakBond bondType atomA atomB (world : World) =

            // remove the bond provisionally
        let bondTypeMap =
            let bondMap =
                world.BondTypeMap[bondType]
                    |> BondMap.remove atomA.Id atomB.Id
                    |> BondMap.remove atomB.Id atomA.Id
            BondTypeMap.setBondMap bondType bondMap world.BondTypeMap

            // enforce valence rules
        let isValid =
            BondType.all
                |> Seq.forall (fun bondType ->
                    let behavior = behaviorMap[bondType]
                    behavior.TestValence atomA bondTypeMap
                        && behavior.TestValence atomB bondTypeMap)

        if isValid then
            Some { world with _BondTypeMap = bondTypeMap }
        else None

    /// Attempts to toggle a bond of the given type between
    /// the atoms with the given IDs in the given world.
    let private tryToggleBond
        parms (rng : Random) bondType atomA atomB (world : World) =

            // count adjacent stacks
        let behavior = behaviorMap[bondType]
        let nStacks =
            behavior.CountStacks atomA.Id atomB.Id world.BondTypeMap

            // compute bond energy
        let bondParms = parms.BondParameterMap[bondType]
        let bondEnergy =
            BondTypeParameters.getEnergy
                atomA.Type atomB.Type bondParms
                + stackEnergy * float nStacks
                - (if bondParms.BendStiffness > 0.0 then
                    getBondBendEnergy
                        bondParms.BendStiffness bondType atomA atomB world
                   else 0.0)

            // compute start barrier
        let bondMap = world.BondTypeMap[bondType]
        let startBarrier =
            if bondParms.StartBarrier > 0
                && BondBehavior.testStrandStart atomA atomB bondMap then
                bondParms.StartBarrier
            else 0.0

            // create or break bond?
        let temp = Parameters.getAvgTemperature atomA atomB parms
        let testBond =
            testBondEnergy rng temp
                bondParms.Barrier
                bondParms.BarrierStackReduction
                startBarrier
                nStacks
        if BondMap.areBonded atomA.Id atomB.Id bondMap then
            if testBond bondParms.Entropy bondEnergy
                || testPeel rng parms temp behavior atomA atomB bondMap
                || testHydrolysis rng bondParms
                then
                tryBreakBond bondType atomA atomB world
                    |> Option.map (fun world ->
                        let event =
                            BreakBond (bondType, atomA.Id, atomB.Id)
                        world, event)
            else None
        else
            if testBond -bondParms.Entropy -bondEnergy then
                tryAddBond bondType bondParms atomA atomB world
                    |> Option.map (fun world ->
                        let event =
                            CreateBond (bondType, atomA.Id, atomB.Id)
                        world, event)
            else None

    /// Attempts to toggle a bond of the given type in the given
    /// world.
    let private tryUpdateBond
        parms (rng : Random) bondType (world : World) =

            // pick atom to bond
        let idA = rng.Next(world.Atoms.Length)
        let atomA = world.Atoms[idA]

            // pick a direction to bond in
        let delta =
            let deltas = behaviorMap[bondType].Deltas
            deltas[rng.Next(deltas.Length)]

            // toggle bond with atom at that location?
        match world[atomA.Location + delta] with
            | Cell.Atom idB ->
                let atomB = world.Atoms[idB]
                tryToggleBond parms rng bondType atomA atomB world
            | _ -> None

    /// Fraction of updates that attempt to move an atom (aka "hop").
    let moveAtomRate = 0.5

    /// Fraction of updates that attempt to toggle a strong bond.
    let toggleStrongBondRate = 0.1

    /// Fraction of updates that attempt to toggle a weak bond.
    let toggleWeakBondRate = 0.4

    /// Attempts to cause an event in the given world without
    /// cloning it.
    let private tryUpdateImpl parms (rng : Random) world =
        assert(
            moveAtomRate
                + toggleStrongBondRate
                + toggleWeakBondRate = 1.0)
        let x = rng.NextDouble()
        if x <= moveAtomRate then
            tryUpdateAtom parms rng world
        elif x <= moveAtomRate + toggleStrongBondRate then
            tryUpdateBond parms rng Strong world
        else
            tryUpdateBond parms rng Weak world

    /// Attempts to cause an event in the given world.
    let tryUpdate parms rng world =
        copy world
            |> tryUpdateImpl parms rng

    /// Causes an event in the given world without cloning it,
    /// or is a no-op if the event fails.
    let private updateImpl parms rng world =
        match tryUpdateImpl parms rng world with
            | Some (world, event) -> world, Some event
            | None -> world, None

    /// Causes an event in the given world, or is a no-op if
    /// the event fails.
    let update parms rng world =
        copy world
            |> updateImpl parms rng

    /// Performs the given number of sweeps, where each sweep
    /// is one update per atom in the world, on average. Some
    /// atoms might be updated more than once, or not at all.
    let sweepWith parms rng numSweeps onEvent (world : World) =
        let nUpdates = numSweeps * world.Atoms.Length
        (copy world, seq { 1 .. nUpdates })
            ||> Seq.fold (fun world _ ->
                let world, eventOpt = updateImpl parms rng world
                match eventOpt with
                    | Some event ->
                        onEvent event world.BondTypeMap
                    | None -> ()
                world)

    /// Performs the given number of sweeps, where each sweep
    /// is one update per atom in the world, on average. Some
    /// atoms might be updated more than once, or not at all.
    let sweep parms rng numSweeps world =
        let onEvent _ _ = ()
        sweepWith parms rng numSweeps onEvent world
