namespace Ecos

open System

(*
 * Based on the Bond Fluctuation Model with:
 *    - Metropolis dynamics
 *    - Pairing of complementary strands
 *    - Stacking of bonded pairs (a la DNA)
 *)

/// Unique ID of an atom in a world.
type AtomId = int

/// Bidirectional bonds between atoms.
type BondMap = Map<AtomId, Set<AtomId>>

module private BondMap =

    /// Answers the IDs of all atoms bonded with the given atom.
    let getBonds id bondMap =
        let ids =
            bondMap
                |> Map.tryFind id
                |> Option.defaultValue Set.empty
        assert(
            Seq.forall (fun idB ->
                bondMap[idB].Contains(id))
                ids)
        ids

    /// Indicates whether the atoms with the given IDs are already
    /// bonded.
    let areBonded idA idB bondMap =
        assert(
            (getBonds idA bondMap).Contains(idB) =
                (getBonds idB bondMap).Contains(idA))
        (getBonds idA bondMap).Contains(idB)

    /// Determines whether a bond between the given atoms is valid.
    let testBond neighborhood atomA atomB =
        assert(
            Seq.forall (fun (x, y) ->
                x >= 0 && y >= 0)
                neighborhood)
        let diff = abs (atomA.Location - atomB.Location)
        Set.contains
            (max diff.X diff.Y, min diff.X diff.Y)
            neighborhood

    /// Tests validity of the given atom's bonds.
    let testBonds
        neighborhood id atom (atomMap : Map<_, _>) bondMap =
        getBonds id bondMap
            |> Seq.forall (fun idB ->
                let atomB = atomMap[idB]
                testBond neighborhood atom atomB)

    /// Creates a unidirectional bond between the given atoms.
    let createBond idA idB bondMap =
        let ids =
            Map.tryFind idA bondMap
                |> Option.defaultValue Set.empty
                |> Set.add idB
        Map.add idA ids bondMap

/// A lattice world containing atoms bonded into molecules.
/// Every atom occupies a 2x2 square around its central
/// location on the lattice, so in the drawing below, the
/// ╋ symbol indicates the location of the atom:
///    ┏━┳━┓
///    ┣━╋━┫
///    ┗━┻━┛
type World =
    {
        /// Atoms in the world, mapped by their IDs.
        AtomMap : Map<AtomId, Atom>

        /// Strong bonds between atoms in the world.
        StrongBondMap : BondMap

        /// Weak bonds between atoms in the world.
        WeakBondMap : BondMap

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
            StrongBondMap = Map.empty
            WeakBondMap = Map.empty
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
    let private getAllBonds id world =
        Seq.append
            (BondMap.getBonds id world.StrongBondMap)
            (BondMap.getBonds id world.WeakBondMap)

    /// All possible normalized strong bond vectors.
    let private strongBondNeighborhood =
        set [
            2, 0
            2, 1
            2, 2
            3, 0
            3, 1
            3, 2
        ]

    /// Maximum number of strong bonds an atom can have.
    let private strongBondValence = 2

    /// Indicates whether the atoms with the given IDs are already
    /// bonded in the given world.
    let private areBonded idA idB world =
        BondMap.areBonded idA idB world.StrongBondMap
            || BondMap.areBonded idA idB world.WeakBondMap
   
    /// Adds a strong bond between the given atoms in the given
    /// world if it is valid. Note that this currently allows
    /// crossed bonds. (Moves cannot create crossings, so a world
    /// created without crossed bonds will stay that way.)
    let tryAddStrongBond idA idB world =
        let atomA = world.AtomMap[idA]
        let atomB = world.AtomMap[idB]
        let canAdd =
            let bondMap = world.StrongBondMap
            BondMap.testBond strongBondNeighborhood atomA atomB
                && (BondMap.getBonds idA bondMap).Count < strongBondValence
                && (BondMap.getBonds idB bondMap).Count < strongBondValence
                && not (areBonded idA idB world)
        if canAdd then
            Some {
                world with
                    StrongBondMap =
                        world.StrongBondMap
                            |> BondMap.createBond idA idB
                            |> BondMap.createBond idB idA
            }
        else None

    /// Adds a strong bond between the given atoms in the given
    /// world.
    let addStrongBond idA idB world =
        match tryAddStrongBond idA idB world with
            | Some world -> world
            | None -> failwith "Invalid bond"

    /// All possible normalized weak bond vectors.
    let private weakBondNeighborhood =
        set [
            2, 0
            2, 1
            2, 2
        ]

    /// Denormalized weak bond neighborhood.
    let private weakBondDeltas =
        weakBondNeighborhood
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

    /// Maximum number of weak bonds an atom can have.
    let private weakBondValence = 1

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
    let private isCrossBond idA atomA idB atomB world =
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

    /// Adds a weak bond between the given atoms in the given
    /// world if it is valid.
    let tryAddWeakBond idA idB world =
        let atomA = world.AtomMap[idA]
        let atomB = world.AtomMap[idB]
        let canAdd =
            let bondMap = world.WeakBondMap
            AtomType.canPair atomA.Type atomB.Type
                && BondMap.testBond weakBondNeighborhood atomA atomB
                && (BondMap.getBonds idA bondMap).Count < weakBondValence
                && (BondMap.getBonds idB bondMap).Count < weakBondValence
                && not (areBonded idA idB world)
                && not (isCrossBond idA atomA idB atomB world)
        if canAdd then
            Some {
                world with
                    WeakBondMap =
                        world.WeakBondMap
                            |> BondMap.createBond idA idB
                            |> BondMap.createBond idB idA
            }
        else None

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
            isAvailable atom'.Location (Some idA) world       // avoids overlap with other atoms? (to-do: check only the leading edge)
                && BondMap.testBonds strongBondNeighborhood   // all of the atom's strong bonds are still valid?
                    idA atom' world.AtomMap world.StrongBondMap
                && BondMap.testBonds weakBondNeighborhood     // all of the atom's weak bonds are still valid?
                    idA atom' world.AtomMap world.WeakBondMap
                && testMetropolis kT rng 0                    // acceptable energy change?
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
            BondMap.getBonds idB world.StrongBondMap
        BondMap.getBonds idA world.StrongBondMap
            |> Seq.where (fun idA' ->
                BondMap.getBonds idA' world.WeakBondMap
                    |> Seq.exists (fun idB' ->
                        bNeighbors.Contains(idB')))
            |> Seq.length

    /// Breaks a weak bond between the atoms with the given IDs
    /// in the given world.
    let private breakWeakBond idA idB world =
        let weakBondMap =
            world.WeakBondMap
                |> Map.remove idA
                |> Map.remove idB
        { world with WeakBondMap = weakBondMap }

    /// Attempts to toggle a weak bond between the atoms with the
    /// given IDs in the given world.
    let private tryToggleWeakBond kT rng idA idB world =
        let bondEnergy =
            let nStacks = countStacks idA idB world
            weakBondEnergy + stackEnergy * nStacks
        if BondMap.areBonded idA idB world.WeakBondMap then
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
