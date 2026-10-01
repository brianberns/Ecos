namespace Ecos

open System

(*
 * 2D bond fluctuation model.
 * https://en.wikipedia.org/wiki/Bond_fluctuation_model
 *)

/// Unique ID of an atom in a world.
type AtomId = int

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

        /// Undirected strong bonds between atoms in the world.
        StrongBondMap : Map<AtomId, Set<AtomId>>

        /// Undirected weak bonds between atoms in the world.
        WeakBondMap : Map<AtomId, AtomId>   // valence = 1

        /// Maps each atom's location to its ID.
        OccupancyMap : Map<Point, AtomId>
    }

module World =

    /// An empty world.
    let empty =
        {
            AtomMap = Map.empty
            StrongBondMap = Map.empty
            WeakBondMap = Map.empty
            OccupancyMap = Map.empty
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
                world.OccupancyMap
                    |> Map.tryFind (loc + delta)
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

    /// Answers the IDs of all atoms strongly bonded with the
    /// given atom.
    let private getStrongBonds id world =
        let ids =
            world.StrongBondMap
                |> Map.tryFind id
                |> Option.defaultValue Set.empty
        assert(
            Seq.forall (fun idB ->
                world.StrongBondMap[idB].Contains(id))
                ids)
        ids

    /// Answers the IDs of all atoms weakly bonded with the
    /// given atom.
    let private getWeakBonds id world =
        let ids =
            world.WeakBondMap
                |> Map.tryFind id
                |> Option.toArray
                |> set
        assert(
            Seq.forall (fun idB ->
                world.WeakBondMap[idB] = id)
                ids)
        ids

    /// Answers the IDs of all atoms bonded with the given atom.
    let private getAllBonds id world =
        Seq.append
            (getStrongBonds id world)
            (getWeakBonds id world)

    /// Determines whether a strong bond between the given atoms
    /// is valid.
    let private testStrongBond atomA atomB =
        let diff = abs (atomA.Location - atomB.Location)
        match max diff.X diff.Y, min diff.X diff.Y with
            | 2, 0
            | 2, 1
            | 2, 2
            | 3, 0
            | 3, 1
            | 3, 2 -> true
            | _ -> false

    /// Maximum number of strong bonds an atom can have.
    let private strongBondValence = 2

    /// Indicates whether the atoms with the given IDs are already
    /// strongly bonded in the given world.
    let private areStronglyBonded idA idB world =
        assert(
            (getStrongBonds idA world).Contains(idB) =
                (getStrongBonds idB world).Contains(idA))
        (getStrongBonds idA world).Contains(idB)

    /// Indicates whether the atoms with the given IDs are already
    /// weakly bonded in the given world.
    let private areWeaklyBonded idA idB world =
        assert(
            (getWeakBonds idA world).Contains(idB) =
                (getWeakBonds idB world).Contains(idA))
        (getWeakBonds idA world).Contains(idB)

    /// Indicates whether the atoms with the given IDs are already
    /// bonded in the given world.
    let private areBonded idA idB world =
        areStronglyBonded idA idB world
            || areWeaklyBonded idA idB world

    /// Creates a unidirectional strong bond between the given
    /// atoms.
    let private createStrongBond idA idB bondMap =
        let ids =
            Map.tryFind idA bondMap
                |> Option.defaultValue Set.empty
                |> Set.add idB
        Map.add idA ids bondMap
   
    /// Adds a strong bond between the given atoms in the given
    /// world if it is valid. Note that this currently allows
    /// crossed bonds. (Moves cannot create crossings, so a world
    /// created without crossed bonds will stay that way.)
    let tryAddStrongBond idA idB world =
        let atomA = world.AtomMap[idA]
        let atomB = world.AtomMap[idB]
        let canAdd =
            testStrongBond atomA atomB
                && (getStrongBonds idA world).Count < strongBondValence
                && (getStrongBonds idB world).Count < strongBondValence
                && not (areBonded idA idB world)
        if canAdd then
            Some {
                world with
                    StrongBondMap =
                        world.StrongBondMap
                            |> createStrongBond idA idB
                            |> createStrongBond idB idA
            }
        else None

    /// Adds a strong bond between the given atoms in the given
    /// world.
    let addStrongBond idA idB world =
        match tryAddStrongBond idA idB world with
            | Some world -> world
            | None -> failwith "Invalid bond"

    /// Determines whether a weak bond between the given locations
    /// is valid.
    let private testWeakBondLocation locA locB =
        let diff = abs (locA - locB)
        match max diff.X diff.Y, min diff.X diff.Y with
            | 2, 0
            | 2, 1
            | 2, 2 -> true
            | _ -> false

    /// Determines whether a weak bond between the given atoms
    /// is valid.
    let private testWeakBond atomA atomB =
        testWeakBondLocation
            atomA.Location
            atomB.Location

    /// Maximum number of weak bonds an atom can have.
    let private weakBondValence = 1

    /// Weak bond neighborhood. These are neighbors that
    /// are in short-range contact.
    let private weakBondNeighborhood =
        let points =
            [| (2, 0); (2, 1); (2, 2) |]
                |> Array.collect (fun (a, b) ->
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
                |> Array.map Point.create
        assert(
            Seq.forall
                (testWeakBondLocation Point.zero)
                points)
        points

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
    /// local neighborhood of one of the atoms.
    let private isCrossBond idA atomA idB atomB world =
        weakBondNeighborhood
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
            AtomType.canPair atomA.Type atomB.Type
                && testWeakBond atomA atomB
                && (getWeakBonds idA world).Count < weakBondValence
                && (getWeakBonds idB world).Count < weakBondValence
                && not (areBonded idA idB world)
                && not (isCrossBond idA atomA idB atomB world)
        if canAdd then
            Some {
                world with
                    WeakBondMap =
                        world.WeakBondMap
                            |> Map.add idA idB
                            |> Map.add idB idA
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

    /// Tests validity of the given atom's strong bonds.
    let private testStrongBonds id atom world =
        getStrongBonds id world
            |> Seq.forall (fun idB ->
                let atomB = world.AtomMap[idB]
                testStrongBond atom atomB)

    /// Tests validity of the given atom's weak bonds.
    let private testWeakBonds id atom world =
        getWeakBonds id world
            |> Seq.forall (fun idB ->
                let atomB = world.AtomMap[idB]
                testWeakBond atom atomB)

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
                && testStrongBonds idA atom' world        // all of the atom's strong bonds are still valid?
                && testWeakBonds idA atom' world          // all of the atom's weak bonds are still valid?
                && testMetropolis kT rng 0                // acceptable energy change?
        if isValid then
            moveAtom idA atom atom' world
        else world

    /// Energy delta of a weak bond.
    let private weakBondEnergy = 1

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
        if areWeaklyBonded idA idB world then
            if testMetropolis kT rng weakBondEnergy then
                Some (breakWeakBond idA idB world)
            else None
        else
            tryAddWeakBond idA idB world
                |> Option.filter (fun _ ->
                    testMetropolis kT rng -weakBondEnergy)

    /// Attempts to toggle a weak bond in the given world.
    let private updateWeakBond kT (rng : Random) world =

            // pick atom to bond
        let idA = rng.Next(world.AtomMap.Count)   // assume for now that atoms are never removed
        let atom = world.AtomMap[idA]

            // pick a direction to bond in
        let delta =
            weakBondNeighborhood[
                rng.Next(weakBondNeighborhood.Length)]

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
