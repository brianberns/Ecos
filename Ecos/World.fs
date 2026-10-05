namespace Ecos

open System

/// Bond type.
type BondType =

    /// Long-term, long-range bonds.
    | Strong

    /// Short-term, short-range bonds.
    | Weak

module BondType =

    /// All bond types.
    let all = [| Strong; Weak |]

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
    }

/// Configurable update parameters.
type Parameters =
    {
        /// Temperature scaled by Boltzmann's constant (kT).
        Temperature : float

        /// Bond type-specific parameters.
        BondParameterMap : Map<BondType, BondTypeParameters>
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
        /// Atoms in the world, mapped by their IDs.
        AtomMap : Map<AtomId, Atom>

        /// Bonds between atoms in the world, by type.
        BondTypeMap : Map<BondType, BondMap>

        /// Maps each atom's location to its ID.
        OccupancyMap : Map<Point, AtomId>

        /// Immovable obstacles, each the size of an atom.
        Walls : Set<Point>
    }

module World =

    /// An empty world.
    let empty =
        {
            AtomMap = Map.empty
            BondTypeMap =
                BondType.all
                    |> Seq.map (fun bondType ->
                        bondType, Map.empty)
                    |> Map
            OccupancyMap = Map.empty
            Walls = Set.empty
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
                        |> Map.tryFind neighbor
                        |> Option.map (fun id -> Some id = idOpt)   // an atom can occupy its own location
                        |> Option.defaultValue true)

    /// Adds the given atom to the given world if it is valid.
    let tryAddAtom atom world =
        if isAvailable atom.Location None world then
            let id = world.AtomMap.Count   // assume for now that atoms are never removed
            Some {
                world with
                    AtomMap =
                        Map.add id atom world.AtomMap
                    OccupancyMap =
                        Map.add atom.Location id world.OccupancyMap
            }
        else None

    /// Adds the given atom to the given world.
    let addAtom atom world =
        match tryAddAtom atom world with
            | Some world -> world
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
    let private isCrossBond bondType idA atomA idB atomB world =
        behaviorMap[bondType].CrossBondDeltas
            |> Seq.exists (fun delta ->
                world.OccupancyMap
                    |> Map.tryFind (atomA.Location + delta)   // check A's neighborhood
                    |> Option.map (fun idC ->
                        if idC = idA || idC = idB then false
                        else
                            let atomC = world.AtomMap[idC]
                            getAllBonds idC world
                                |> Seq.exists (fun idD ->
                                    if idD = idA || idD = idB then false
                                    else
                                        let atomD = world.AtomMap[idD]
                                        BondBehavior.testCrossBond
                                            (atomA, atomB)
                                            (atomC, atomD)))
                    |> Option.defaultValue false)

    /// Adds a bond of the given type between the given atoms in
    /// the given world if it is valid.
    let private tryAddBond bondType idA idB world =

        let bondMap = world.BondTypeMap[bondType]

        let canAdd =
            let atomA = world.AtomMap[idA]
            let atomB = world.AtomMap[idB]
            let behavior = behaviorMap[bondType]
            behavior.CanBond atomA.Type atomB.Type
                && BondMap.testBond behavior.Neighborhood atomA atomB
                && (BondMap.getBonds idA bondMap).Count
                    < behavior.Valence
                && (BondMap.getBonds idB bondMap).Count
                    < behavior.Valence
                && not (areBonded idA idB world)
                && not (isCrossBond bondType idA atomA idB atomB world)

        if canAdd then
            let bondMap = BondMap.createBond idB idA bondMap
            Some {
                world with
                    BondTypeMap =
                        world.BondTypeMap
                            |> Map.add bondType bondMap
            }
        else None

    /// Adds a strong bond between the given atoms in the given
    /// world if it is valid. Note that this currently allows
    /// crossed bonds. (Moves cannot create crossings, so a world
    /// created without crossed bonds will stay that way.)
    let tryAddStrongBond idA idB world =
        tryAddBond Strong idA idB world

    /// Adds a strong bond between the given atoms in the given
    /// world.
    let addStrongBond idA idB world =
        match tryAddStrongBond idA idB world with
            | Some world -> world
            | None -> failwith "Invalid bond"

    /// Adds a weak bond between the given atoms in the given
    /// world if it is valid.
    let tryAddWeakBond idA idB world =
        tryAddBond Weak idA idB world

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

    /// Determines whether the given atom's bonds are valid.
    let private testBonds id atom world =
        behaviorMap
            |> Map.toSeq
            |> Seq.forall (fun (bondType, behavior) ->
                BondMap.testBonds behavior.Neighborhood
                    id atom world.AtomMap
                    world.BondTypeMap[bondType])

    /// Applies the Metropolis rule:
    /// * A move that lowers the energy, or leaves it unchanged,
    ///   is always accepted.
    /// * An uphill move is accepted with probability exp(−ΔE/kT),
    ///   where kT is temperature scaled by Boltzmann's constant.
    let private testMetropolis
        (kT : float) (rng : Random) (energyDelta : float) =
        energyDelta <= 0
            || rng.NextDouble() < exp (-energyDelta / kT)

    /// Updates the given atom's location in the given
    /// world.
    let private moveAtom id atomBefore atomAfter world =
        assert(world.AtomMap[id] = atomBefore)
        assert(world.OccupancyMap[atomBefore.Location] = id)
        {
            world with
                AtomMap =
                    world.AtomMap
                        |> Map.add id atomAfter   // replaces `atomBefore`
                OccupancyMap =
                    world.OccupancyMap
                        |> Map.remove atomBefore.Location
                        |> Map.add atomAfter.Location id
        }

    /// Attempts to move an atom in the given world.
    let private tryUpdateAtom parms (rng : Random) world =

            // pick atom to move
        let idA = rng.Next(world.AtomMap.Count)   // assume for now that atoms are never removed
        let atom = world.AtomMap[idA]

            // pick direction to move in
        let delta = getRandomDirection rng

            // move atom in that direction provisionally
        let atom' =
            { atom with
                Location = atom.Location + delta }

            // move the atom permanently?
        let isValid =
            isAvailable atom'.Location (Some idA) world     // avoids overlap with other atoms? (to-do: check only the leading edge)
                && testBonds idA atom' world                // all of the atom's bonds are still valid?
                && testMetropolis parms.Temperature rng 0   // possible to-do: add position-dependent energy term
        if isValid then
            let world = moveAtom idA atom atom' world
            let event = MoveAtom idA
            Some (world, event)
        else None

    /// Counts stacks adjacent to the given (existing or 
    /// proposed) weak bond:
    ///    A' ══ A ══ A'   ══ strong bond
    ///    ┆     ┆    ┆    ┆  weak bond
    ///    ┆     ?    ┆
    ///    ┆     ┆    ┆
    ///    B' ══ B ══ B'
    let private countStacksWeak idA idB world =
        let strongBondMap = world.BondTypeMap[Strong]
        let weakBondMap = world.BondTypeMap[Weak]
        let bPrimes = BondMap.getBonds idB strongBondMap
        BondMap.getBonds idA strongBondMap
            |> Seq.where (fun idA' ->
                BondMap.getBonds idA' weakBondMap
                    |> Seq.exists (fun idB' ->
                        bPrimes.Contains(idB')))
            |> Seq.length

    /// Counts stacks affected by the given (existing or 
    /// proposed) strong bond:
    ///    A  ═?═ B    ══ strong bond
    ///    ┆      ┆    ┆  weak bond
    ///    A' ═══ B'
    let private countStacksStrong idA idB world =
        let isStack =
            let strongBondMap = world.BondTypeMap[Strong]
            let weakBondMap = world.BondTypeMap[Weak]
            let bPrimes = BondMap.getBonds idB weakBondMap
            BondMap.getBonds idA weakBondMap
                |> Seq.exists (fun idA' ->
                    BondMap.getBonds idA' strongBondMap
                        |> Seq.exists (fun idB' ->
                            bPrimes.Contains(idB')))
        if isStack then 1 else 0

    /// Counts weak bonds stacked by a bond of the given
    /// type between the given atoms.
    let private countStacks bondType idA idB world =
        match bondType with
            | Weak -> countStacksWeak idA idB world
            | Strong -> countStacksStrong idA idB world

    /// Breaks a bond of the given type between the atoms
    /// with the given IDs in the given world.
    let private breakBond bondType idA idB world =

        /// Removes atom B from atom A's bond set.
        let remove idA idB (bondMap : BondMap) : BondMap =
            let ids = bondMap[idA].Remove(idB)
            if ids.IsEmpty then
                Map.remove idA bondMap
            else
                Map.add idA ids bondMap

        let bondMap =
            world.BondTypeMap[bondType]
                |> remove idA idB
                |> remove idB idA
        { world with
            BondTypeMap =
                Map.add bondType bondMap world.BondTypeMap }

    /// Additional energy from a stacked weak bond.
    let private stackEnergy = 1.0

    /// Attempts to toggle a bond of the given type between
    /// the atoms with the given IDs in the given world.
    let private tryToggleBond parms rng bondType idA idB world =

        let bondParms = parms.BondParameterMap[bondType]
        let nStacks = countStacks bondType idA idB world

        let test energy =
            let barrier =
                bondParms.Barrier
                    - bondParms.BarrierStackReduction * float nStacks
            testMetropolis parms.Temperature rng barrier
                && testMetropolis parms.Temperature rng energy

        let bondEnergy =
            bondParms.Energy + stackEnergy * float nStacks

        if BondMap.areBonded idA idB world.BondTypeMap[bondType] then
            if test bondEnergy then
                let world = breakBond bondType idA idB world
                let event = BreakBond (bondType, idA, idB)
                Some (world, event)
            else None
        else
            if test -bondEnergy then
                tryAddBond bondType idA idB world
                    |> Option.map (fun world ->
                        let event = CreateBond (bondType, idA, idB)
                        world, event)
            else None

    /// Attempts to toggle a bond of the given type in the given
    /// world.
    let private tryUpdateBond parms (rng : Random) bondType world =

            // pick atom to bond
        let idA = rng.Next(world.AtomMap.Count)   // assume for now that atoms are never removed
        let atom = world.AtomMap[idA]

            // pick a direction to bond in
        let delta =
            let deltas = behaviorMap[bondType].Deltas
            deltas[rng.Next(deltas.Length)]

            // toggle bond with atom at that location?
        world.OccupancyMap
            |> Map.tryFind (atom.Location + delta)
            |> Option.bind (fun idB ->
                tryToggleBond parms rng bondType idA idB world)

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
        (world, seq { 1 .. world.AtomMap.Count })
            ||> Seq.fold (fun world _ ->
                update parms rng world |> fst)
