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

    /// Moore neighborhood, including center.
    let private mooreNeighborhood =
        [|
            for dx = -1 to 1 do
                for dy = -1 to 1 do
                    Point.create (dx, dy)
        |]

    /// Determines whether the given location is available.
    let private isAvailable loc idOpt world =
        mooreNeighborhood
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
    let getAllBonds id world =
        world.BondTypeMap.Values
            |> Seq.collect (BondMap.getBonds id)

    /// Indicates whether the atoms with the given IDs are already
    /// bonded in the given world.
    let private areBonded idA idB world =
        world.BondTypeMap.Values
            |> Seq.exists (BondMap.areBonded idA idB)

    /// Denormalized weak bond neighborhood.
    let private weakBondDeltas =
        WeakBond.neighborhood
            |> Seq.collect (fun (a, b) ->
                [|
                    (a, b)                            // e.g.  2,  1
                    if a <> 0 then (-a, b)            // e.g. -2,  1
                    if b <> 0 then
                        (a, -b)                       // e.g.  2, -1
                        if a <> 0 then (-a, -b)       // e.g. -2, -1
                    if a <> b then
                        (b, a)                        // e.g.  1,  2
                        if b <> 0 then (-b, a)        // e.g. -1,  2
                        if a <> 0 then
                            (b, -a)                   // e.g.  1, -2
                            if b <> 0 then (-b, -a)   // e.g. -1, -2
                |])
            |> Seq.map Point.create
            |> Seq.toArray

    /// Determines orientation of the path connecting
    /// the given points (a-b-c):
    /// * Positive: left turn
    /// * Negative: right turn
    let private orient (a : Point) (b : Point) (c : Point) =
        sign ((b - a) .* (c - a))

    /// Indicates whether bonds between the given pairs of atoms
    /// cross.
    let private testCrossBond (atomA, atomB) (atomC, atomD) =
        let locA = atomA.Location
        let locB = atomB.Location
        let locC = atomC.Location
        let locD = atomD.Location
        let cdOpp =   // C and D are on opposite sides of line AB
            orient locA locB locC * orient locA locB locD < 0
        let abOpp =   // A and B are on opposite sides of line CD
            orient locC locD locA * orient locC locD locB < 0
        cdOpp && abOpp

    /// Indicates whether a bond between the given atoms
    /// crosses any other bonds. This only needs to check the
    /// local neighborhood of one of the atoms. (In fact, Claude
    /// says, for weak bonds, it would suffice to check only
    /// four deltas: (±2, 0) and (0, ±2)).
    let isCrossBond idA atomA idB atomB world =
        weakBondDeltas
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
                                        testCrossBond
                                            (atomA, atomB)
                                            (atomC, atomD)))
                    |> Option.defaultValue false)

    /// Behavior of each bond type.
    let private behaviorMap =
        Map [
            Strong, StrongBond.behavior
            Weak, WeakBond.behavior
        ]

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
                &&
                    (not behavior.CheckCrossBonds
                        || not (isCrossBond idA atomA idB atomB world))

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
    /// * An uphill move is accepted with probability exp(−ΔE/kT).
    let private testMetropolis kT (rng : Random) energyDelta =
        energyDelta <= 0
            || rng.NextDouble() < exp (-float energyDelta / kT)   // kT: temperature scaled by Boltzmann's constant.

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
    let private updateAtom kT (rng : Random) world =

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
            isAvailable atom'.Location (Some idA) world   // avoids overlap with other atoms? (to-do: check only the leading edge)
                && testBonds idA atom' world              // all of the atom's bonds are still valid?
                && testMetropolis kT rng 0                // acceptable energy change?
        if isValid then
            moveAtom idA atom atom' world
        else world

    /// Energy delta of a weak bond.
    let private weakBondEnergy = 1

    /// Additional energy from a stacked weak bond.
    let private stackEnergy = weakBondEnergy

    /// Counts stacks adjacent to the given bond:
    ///    A ══ A′   ══ strong bond
    ///    ┆    ┆    ┆  weak bond
    ///    B ══ B′
    let private countStacks idA idB world =
        let bNeighbors =
            BondMap.getBonds idB world.BondTypeMap[Strong]
        BondMap.getBonds idA world.BondTypeMap[Strong]
            |> Seq.where (fun idA' ->
                BondMap.getBonds idA' world.BondTypeMap[Weak]
                    |> Seq.exists (fun idB' ->
                        bNeighbors.Contains(idB')))
            |> Seq.length

    /// Breaks a weak bond between the atoms with the given IDs
    /// in the given world.
    let private breakWeakBond idA idB world =
        let weakBondMap =
            world.BondTypeMap[Weak]
                |> Map.remove idA
                |> Map.remove idB
        { world with
            BondTypeMap =
                Map.add Weak weakBondMap world.BondTypeMap }

    /// Attempts to toggle a weak bond between the atoms with the
    /// given IDs in the given world.
    let private tryToggleWeakBond kT rng idA idB world =
        let bondEnergy =
            let nStacks = countStacks idA idB world
            weakBondEnergy + stackEnergy * nStacks
        if BondMap.areBonded idA idB world.BondTypeMap[Weak] then
            if testMetropolis kT rng bondEnergy then
                Some (breakWeakBond idA idB world)
            else None
        else
            if testMetropolis kT rng -bondEnergy then
                tryAddWeakBond idA idB world
            else None

    /// Attempts to toggle a weak bond in the given world.
    let private updateWeakBond kT (rng : Random) world =

            // pick atom to bond
        let idA = rng.Next(world.AtomMap.Count)   // assume for now that atoms are never removed
        let atom = world.AtomMap[idA]

            // pick a direction to bond in
        let delta =
            weakBondDeltas[rng.Next(weakBondDeltas.Length)]

            // toggle bond with atom at that location?
        world.OccupancyMap
            |> Map.tryFind (atom.Location + delta)
            |> Option.bind (fun idB ->
                tryToggleWeakBond kT rng idA idB world)
            |> Option.defaultValue world

    /// Attempts to cause an event in the given world.
    let update kT (rng : Random) world =
        if rng.NextDouble() <= 0.5 then
            updateAtom kT rng world
        else updateWeakBond kT rng world
