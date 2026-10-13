namespace Ecos

open System
open System.Collections.Immutable

/// Bond type-specific parameters.
/// 
/// Energy
///   |            ----  <- barrier: in-between state
///   |           /    \
///   |   E_b    /      \
///   |  --------        \
///   |  unbonded         \      E_b + S
///   |                    \
///   |                     -------- bonded (lower by S)
///   |
///    ----------------------------------
/// 
/// The barrier affects how fast the system moves between the
/// states, not which one it favors.
/// 
/// (Drawing by Claude, with my edits.)
type BondTypeParameters =
    {
        /// Energy delta of this bond type (S).
        Energy : float

        /// Energy barrier to bond formation (E_b).
        Barrier : float

        /// Barrier reduction for completed stacks (δ).
        BarrierStackReduction : float

        /// Implicit entropy in this bond type (s).
        Entropy : float
    }

/// Configurable update parameters.
type Parameters =
    {
        /// Temperature scaled by Boltzmann's constant (kT).
        Temperature : float

        /// Bond type-specific parameters.
        BondParameterMap : Map<BondType, BondTypeParameters>

        /// Determines how often peeling happens. +Infinity
        /// prevents peeling altogether.
        PeelBarrier : float
    }

/// An event in the world.
type Event =

    /// An atom moved.
    | MoveAtom of AtomId

    /// A bond was created between two atoms.
    | CreateBond of BondType * AtomId * AtomId

    /// A bond was broken between two atoms.
    | BreakBond of BondType * AtomId * AtomId

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

module Dictionary =

    /// Tries to find the given key in the given dictionary.
    let inline tryFind key (dict : ImmutableSortedDictionary<_, _>) =
        match dict.TryGetValue(key) with
            | true, value -> Some value
            | false, _ -> None

    /// Adds the given key to the given value in the given
    /// dictionary.
    let inline add key value (dict : ImmutableSortedDictionary<_, _>) =
        dict.Add(key, value)

    /// Removes the given key from the given dictionary.
    let inline remove key (dict : ImmutableSortedDictionary<_, _>) =
        dict.Remove(key)

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

    /// Chooses a random cardinal direction.
    let private getRandomDirection (rng : Random) =
        Point.create <|
            match rng.Next(4) with
                | 0 ->  1,  0
                | 1 ->  0, -1
                | 2 -> -1,  0
                | 3 ->  0,  1
                | _ -> failwith "Unexpected"

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
            isAvailable atom'.Location (Some id) world        // avoids overlap with other atoms? (to-do: check only the leading edge)
                && testBonds atom' world                      // all of the atom's bonds are still valid?
                && testMetropolis rng parms.Temperature 0 0   // possible to-do: add position-dependent energy term
        if isValid then
            let world = moveAtom atom atom' world
            let event = MoveAtom id
            Some (world, event)
        else None

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

    /// Additional energy from a stacked weak bond.
    let private stackEnergy = 1.0

    /// Attempts to toggle a bond of the given type between
    /// the atoms with the given IDs in the given world.
    let private tryToggleBond parms rng bondType atomA atomB world =

            // prepare to work with this bond type
        let bondParms = parms.BondParameterMap[bondType]
        let bondMap = world.BondTypeMap[bondType]
        let behavior = behaviorMap[bondType]

            // count adjacent stacks
        let nStacks =
            behavior.CountStacks atomA.Id atomB.Id world.BondTypeMap

        /// Applies Metropolis tests to the given bond energy.
        let testBond energy =
            let barrier =
                bondParms.Barrier
                    - bondParms.BarrierStackReduction * float nStacks
            testMetropolis rng parms.Temperature 0 barrier
                && testMetropolis
                    rng parms.Temperature bondParms.Entropy energy

            // compute bond energy
        let bondEnergy =
            bondParms.Energy + stackEnergy * float nStacks

        /// Peel this bond?
        let testPeel () =
            let isExposed =
                behavior.TestBondPeel atomA atomB bondMap
            if isExposed then
                rng.NextDouble() <=
                    exp (-parms.PeelBarrier / parms.Temperature)
            else false

            // create or break bond?
        if BondMap.areBonded atomA.Id atomB.Id bondMap then
            if testBond bondEnergy || testPeel () then
                tryBreakBond bondType atomA atomB world
                    |> Option.map (fun world ->
                        let event =
                            BreakBond (bondType, atomA.Id, atomB.Id)
                        world, event)
            else None
        else
            if testBond -bondEnergy then
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
