namespace Ecos

open System
open System.Collections.Immutable

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
type World =
    {
        /// Atoms in the world, indexed by their IDs.
        Atoms : ImmutableList<Atom>

        /// Bonds between atoms in the world, by type.
        BondTypeMap : Map<BondType, BondMap>

        /// Maps each atom's location to its ID.
        OccupancyMap : ImmutableSortedDictionary<Point, AtomId>

        /// Immovable obstacles, each the size of an atom.
        Walls : ImmutableSortedSet<Point>
    }

/// An event in the world.
type Event =

    /// An atom moved.
    | MoveAtom of AtomId

    /// A bond was created between two atoms.
    | CreateBond of BondType * AtomId * AtomId

    /// A bond was broken between two atoms.
    | BreakBond of BondType * AtomId * AtomId

module World =

    /// An empty world.
    let empty =
        {
            Atoms = ImmutableList.Empty
            BondTypeMap =
                BondType.all
                    |> Seq.map (fun bondType ->
                        bondType, Map.empty)
                    |> Map
            OccupancyMap = ImmutableSortedDictionary.Empty
            Walls = ImmutableSortedSet.Empty
        }

    /// Moore neighborhood deltas, including center.
    let private mooreDeltas =
        [|
            for dx = -1 to 1 do
                for dy = -1 to 1 do
                    Point.create (dx, dy)
        |]

    /// Determines whether the given location is available.
    let private isAvailable loc idOpt world =
        mooreDeltas
            |> Seq.forall (fun delta ->
                let neighbor = loc + delta
                not (world.Walls.Contains(neighbor))
                    && world.OccupancyMap
                        |> Dictionary.tryFind neighbor
                        |> Option.map (fun id -> Some id = idOpt)   // an atom can occupy its own location
                        |> Option.defaultValue true)

    /// Adds an atom with the given attributes to the given world
    /// if it is valid.
    let tryAddAtom atomType location world =
        if isAvailable location None world then
            let id = world.Atoms.Count
            let atom = Atom.create id atomType location
            let world =
                {
                    world with
                        Atoms = world.Atoms.Add(atom)
                        OccupancyMap =
                            Dictionary.add atom.Location id world.OccupancyMap
                }
            Some (world, atom)
        else None

    /// Adds an atom with the given attributes to the given world.
    let addAtom atomType location world =
        match tryAddAtom atomType location world with
            | Some (world, _) -> world
            | None -> failwith "Invalid location"

    /// Adds a wall to the given world if it is valid.
    let tryAddWall loc world =
        if isAvailable loc None world then
            Some {
                world with
                    Walls = world.Walls.Add(loc)
            }
        else None

    /// Adds a wall to the given world.
    let addWall loc world =
        match tryAddWall loc world with
            | Some world -> world
            | None -> failwith "Invalid location"

    /// Answers the IDs of all atoms bonded with the given atom.
    let private getAllBonds id world =
        world.BondTypeMap.Values
            |> Seq.collect (BondMap.getBonds id)

    /// Indicates whether the atoms with the given IDs are already
    /// bonded in the given world.
    let private areBonded idA idB world =
        world.BondTypeMap.Values
            |> Seq.exists (BondMap.areBonded idA idB)

    /// Behavior of each bond type.
    let private behaviorMap =
        Map [
            Strong, StrongBond.behavior
            Weak, WeakBond.behavior
        ]

    /// Indicates whether a bond of the given type between the
    /// given atoms crosses any other bonds. This only needs to
    /// check the local neighborhood of one of the atoms.
    let private isCrossBond behavior atomA atomB world =
        behavior.CrossBondDeltas
            |> Seq.exists (fun delta ->
                world.OccupancyMap
                    |> Dictionary.tryFind (atomA.Location + delta)   // check A's neighborhood
                    |> Option.map (fun idC ->
                        if idC = atomA.Id || idC = atomB.Id then false
                        else
                            let atomC = world.Atoms[idC]
                            getAllBonds idC world
                                |> Seq.exists (fun idD ->
                                    if idD = atomA.Id || idD = atomB.Id then false
                                    else
                                        let atomD = world.Atoms[idD]
                                        BondBehavior.testCrossBond
                                            (atomA, atomB)
                                            (atomC, atomD)))
                    |> Option.defaultValue false)

    /// A -- B
    ///  \
    ///   \
    ///    C
    let private testBondAngle behavior atomA atomB atomC =
        let deltaAB = atomB.Location - atomA.Location   // vector from A to B
        let deltaAC = atomC.Location - atomA.Location   // vector from A to C
        behavior.TestBondAngle deltaAB deltaAC

    /// A -?- B
    ///  \
    ///   \
    ///    C
    let private testBondAngles behavior atomA atomB idsC world =
        Seq.forall (fun idC ->
            let atomC = world.Atoms[idC]
            testBondAngle behavior atomA atomB atomC)
            idsC

    /// Adds a bond of the given type between the given atoms in
    /// the given world if it is valid.
    let private tryAddBond bondType atomA atomB world =

            // get existing bond map
        let bondMap = world.BondTypeMap[bondType]

            // create bond provisionally                
        let bondTypeMap =
            let bondMap =
                BondMap.createBond atomA.Id atomB.Id bondMap
            Map.add bondType bondMap world.BondTypeMap

            // new bond is valid?
        let isValid =
            let behavior = behaviorMap[bondType]
            let idsA = BondMap.getBonds atomA.Id bondMap
            let idsB = BondMap.getBonds atomB.Id bondMap
            behavior.AreCompatible atomA.Type atomB.Type
                && not (areBonded atomA.Id atomB.Id world)
                && Neighborhood.testBond behavior.Neighborhood atomA atomB
                && behavior.TestValence atomA bondTypeMap
                && behavior.TestValence atomB bondTypeMap
                && not (isCrossBond behavior atomA atomB world)
                && testBondAngles behavior atomA atomB idsA world
                && testBondAngles behavior atomB atomA idsB world

        if isValid then
            Some { world with BondTypeMap = bondTypeMap }
        else None

    /// Adds a strong bond between the given atoms in the given
    /// world if it is valid.
    let tryAddStrongBond idA idB world =
        tryAddBond
            Strong world.Atoms[idA] world.Atoms[idB] world

    /// Adds a strong bond between the given atoms in the given
    /// world.
    let addStrongBond idA idB world =
        match tryAddStrongBond idA idB world with
            | Some world -> world
            | None -> failwith "Invalid bond"

    /// Adds a weak bond between the given atoms in the given
    /// world if it is valid.
    let tryAddWeakBond idA idB world =
        tryAddBond
            Weak world.Atoms[idA] world.Atoms[idB] world

    /// Adds a weak bond between the given atoms in the given
    /// world.
    let addWeakBond idA idB world =
        match tryAddWeakBond idA idB world with
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
    let private canMove atom delta world =
        assert(Array.contains delta cardinalDirections)

        let ahead = atom.Location + delta
        let perpendicular = Point.create (delta.Y, delta.X)   // (1, 0) <-> (0, 1)

        let check (cell : Point) =
            not (world.Walls.Contains(cell))
                && not (world.OccupancyMap.ContainsKey(cell))

        let result =
            check ahead
                && check (ahead + perpendicular)
                && check (ahead - perpendicular)
        assert(
            result =
                isAvailable atom.Location (Some atom.Id) world)
        result

    /// Tests bond angles in which the atom is central: C-A-B.
    let private testCentralBonds
        behavior atom (partnerIds : _[]) world =
        seq {
            for i = 0 to partnerIds.Length - 2 do
                for j = i + 1 to partnerIds.Length - 1 do
                    partnerIds[i], partnerIds[j]
        }
            |> Seq.forall (fun (idB, idC) ->
                let atomB = world.Atoms[idB]
                let atomC = world.Atoms[idC]
                testBondAngle behavior atom atomB atomC)

    /// Tests angles in which the atom isn't central: A-B-C.
    let private testPeripheralBonds
        behavior atom bondMap partnerIds world =
        partnerIds
            |> Seq.forall (fun idB ->
                let atomB = world.Atoms[idB]
                BondMap.getBonds idB bondMap
                    |> Seq.where ((<>) atom.Id)
                    |> Seq.forall (fun idC ->
                        world.Atoms[idC]
                            |> testBondAngle behavior atomB atom))

    /// Determines whether the given atom's bonds are valid.
    let private testBonds atom world =
        behaviorMap
            |> Map.toSeq
            |> Seq.forall (fun (bondType, behavior) ->

                    // test the atom's bonds
                let bondMap = world.BondTypeMap[bondType]
                let partnerIds =
                    BondMap.getBonds atom.Id bondMap
                        |> Seq.toArray
                let isValid =
                    partnerIds
                        |> Seq.forall (fun idB ->
                            let atomB = world.Atoms[idB]
                            Neighborhood.testBond
                                behavior.Neighborhood atom atomB)

                    // test bond angles?
                if isValid then
                    testCentralBonds
                        behavior atom partnerIds world
                        && testPeripheralBonds
                            behavior atom bondMap partnerIds world
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

    /// Updates the given atom's location in the given
    /// world.
    let private moveAtom atomBefore atomAfter world =
        assert(atomBefore.Id = atomAfter.Id)
        assert(world.Atoms[atomBefore.Id] = atomBefore)
        assert(world.OccupancyMap[atomBefore.Location] = atomBefore.Id)
        let id = atomBefore.Id
        {
            world with
                Atoms =
                    world.Atoms.SetItem(id, atomAfter)   // replaces `atomBefore`
                OccupancyMap =
                    world.OccupancyMap
                        |> Dictionary.remove atomBefore.Location
                        |> Dictionary.add atomAfter.Location id
        }

    /// Attempts to move an atom in the given world.
    let private tryUpdateAtom parms (rng : Random) world =

            // pick atom to move
        let id = rng.Next(world.Atoms.Count)
        let atom = world.Atoms[id]

            // pick direction to move in
        let delta = getRandomDirection rng

            // move atom in that direction provisionally
        let atom' =
            { atom with
                Location = atom.Location + delta }

            // move the atom permanently?
        let isValid =
            canMove atom' delta world                         // avoids overlap with other atoms?
                && testBonds atom' world                      // all of the atom's bonds are still valid?
                && testMetropolis rng parms.Temperature 0 0   // possible to-do: add position-dependent energy term
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
    let private testPeel (rng : Random) parms behavior atomA atomB bondMap =

        let impl capPeel barrier =
            let isExposed =
                behavior.TestPeel capPeel atomA atomB bondMap
            if isExposed then
                rng.NextDouble() <=
                    exp (-barrier / parms.Temperature)
            else false

        impl false parms.PeelBarrier
            || impl true parms.CapPeelBarrier

    /// Attempts to break the bond of the given type between the
    /// given atoms in the given world.
    let private tryBreakBond bondType atomA atomB world =

            // remove the bond provisionally
        let bondTypeMap =
            let bondMap =
                world.BondTypeMap[bondType]
                    |> BondMap.remove atomA.Id atomB.Id
                    |> BondMap.remove atomB.Id atomA.Id
            Map.add bondType bondMap world.BondTypeMap

            // enforce valence rules
        let isValid =
            BondType.all
                |> Seq.forall (fun bondType ->
                    let behavior = behaviorMap[bondType]
                    behavior.TestValence atomA bondTypeMap
                        && behavior.TestValence atomB bondTypeMap)

        if isValid then
            Some { world with BondTypeMap = bondTypeMap }
        else None

    /// Attempts to toggle a bond of the given type between
    /// the atoms with the given IDs in the given world.
    let private tryToggleBond parms (rng : Random) bondType atomA atomB world =

            // count adjacent stacks
        let behavior = behaviorMap[bondType]
        let nStacks =
            behavior.CountStacks atomA.Id atomB.Id world.BondTypeMap

            // compute bond energy
        let bondParms = parms.BondParameterMap[bondType]
        let bondEnergy =
            bondParms.Energy + stackEnergy * float nStacks

            // compute start barrier
        let bondMap = world.BondTypeMap[bondType]
        let startBarrier =
            if bondParms.StartBarrier > 0
                && BondBehavior.testStrandStart atomA atomB bondMap then
                bondParms.StartBarrier
            else 0.0

            // create or break bond?
        let testBond =
            testBondEnergy
                rng
                parms.Temperature
                bondParms.Barrier
                bondParms.BarrierStackReduction
                startBarrier
                nStacks
        if BondMap.areBonded atomA.Id atomB.Id bondMap then
            if testBond bondParms.Entropy bondEnergy
                || testPeel rng parms behavior atomA atomB bondMap then
                tryBreakBond bondType atomA atomB world
                    |> Option.map (fun world ->
                        let event =
                            BreakBond (bondType, atomA.Id, atomB.Id)
                        world, event)
            else None
        else
            if testBond -bondParms.Entropy -bondEnergy then
                tryAddBond bondType atomA atomB world
                    |> Option.map (fun world ->
                        let event =
                            CreateBond (bondType, atomA.Id, atomB.Id)
                        world, event)
            else None

    /// Attempts to toggle a bond of the given type in the given
    /// world.
    let private tryUpdateBond parms (rng : Random) bondType world =

            // pick atom to bond
        let idA = rng.Next(world.Atoms.Count)
        let atomA = world.Atoms[idA]

            // pick a direction to bond in
        let delta =
            let deltas = behaviorMap[bondType].Deltas
            deltas[rng.Next(deltas.Length)]

            // toggle bond with atom at that location?
        world.OccupancyMap
            |> Dictionary.tryFind (atomA.Location + delta)
            |> Option.bind (fun idB ->
                let atomB = world.Atoms[idB]
                tryToggleBond parms rng bondType atomA atomB world)

    /// Fraction of updates that attempt to move an atom (aka "hop").
    let moveAtomRate = 0.5

    /// Fraction of updates that attempt to toggle a strong bond.
    let toggleStrongBondRate = 0.1

    /// Fraction of updates that attempt to toggle a weak bond.
    let toggleWeakBondRate = 0.4

    /// Attempts to cause an event in the given world.
    let tryUpdate parms (rng : Random) world =
        assert(
            moveAtomRate
                + toggleStrongBondRate
                + toggleWeakBondRate= 1.0)
        let x = rng.NextDouble()
        if x <= moveAtomRate then
            tryUpdateAtom parms rng world
        elif x <= moveAtomRate + toggleStrongBondRate then
            tryUpdateBond parms rng Strong world
        else
            tryUpdateBond parms rng Weak world

    /// Causes an event in the given world, or is a no-op if
    /// the event fails.
    let update parms rng world =
        match tryUpdate parms rng world with
            | Some (world, event) -> world, Some event
            | None -> world, None

    /// Performs one update per atom in the world, on average.
    /// Some atoms might be updated more than once, or not at
    /// all.
    let sweep parms rng world =
        (world, seq { 1 .. world.Atoms.Count })
            ||> Seq.fold (fun world _ ->
                update parms rng world |> fst)
