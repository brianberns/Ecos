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

    /// Determines whether a strong bond between the given
    /// atoms is valid.
    let private testStrongBond atomA atomB =
        let diff = abs (atomA.Location - atomB.Location)   // get non-negative location difference
        match max diff.X diff.Y, min diff.X diff.Y with
            | 2, 0
            | 2, 1
            | 2, 2
            | 3, 0
            | 3, 1
            | 3, 2 -> true
            | _ -> false

    /// Creates a unidirectional strong bond between the given
    /// atoms.
    let private createStrongBond idA idB bonds =
        let ids =
            Map.tryFind idA bonds
                |> Option.defaultValue Set.empty
                |> Set.add idB
        Map.add idA ids bonds
   
    /// Adds a strong bond between the given atoms in the given
    /// world if it is valid. Note that this currently allows
    /// crossed bonds. (Moves cannot create crossings, so a world
    /// created without crossed bonds will stay that way.)
    let tryAddStrongBond idA idB world =
        let atomA = world.AtomMap[idA]
        let atomB = world.AtomMap[idB]
        if testStrongBond atomA atomB then
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

    /// Chooses a random cardinal direction.
    let private getRandomDirection (rng : Random) =
        Point.create <|
            match rng.Next(4) with
                | 0 ->  1,  0
                | 1 ->  0, -1
                | 2 -> -1,  0
                | 3 ->  0,  1
                | _ -> failwith "Unexpected"

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

    /// Tests validity of the given atom's strong bonds.
    let private testStrongBonds id atom world =
        getStrongBonds id world
            |> Seq.forall (fun idB ->
                let atomB = world.AtomMap[idB]
                testStrongBond atom atomB)

    /// Answers the IDs of all atoms weakly bonded with the
    /// given atom.
    let private getWeakBonds id world =
        let ids =
            world.WeakBondMap
                |> Map.tryFind id
                |> Option.toArray
        assert(
            Seq.forall (fun idB ->
                world.StrongBondMap[idB].Contains(id))
                ids)
        ids

    /// Short-range contact neighborhood.
    let private contactNeighborhood =
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

    /// Determines whether a weak bond between the given
    /// atoms is valid.
    let private testWeakBond atomA atomB =
        let diff = abs (atomA.Location - atomB.Location)   // get non-negative location difference
        match max diff.X diff.Y, min diff.X diff.Y with
            | 2, 0
            | 2, 1
            | 2, 2 -> true
            | _ -> false

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

    let private updateWeakBond kT (rng : Random) world =
        world

    /// Attempts to cause an event in the given world.
    let update kT (rng : Random) world =
        if rng.NextDouble() <= 0.5 then
            updateAtom kT rng world
        else updateWeakBond kT rng world
