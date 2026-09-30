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

        /// Undirected bonds between atoms in the world.
        BondMap : Map<AtomId, Set<AtomId>>

        /// Maps each atom's location to its ID.
        OccupancyMap : Map<Point, AtomId>
    }

module World =

    /// An empty world.
    let empty =
        {
            AtomMap = Map.empty
            BondMap = Map.empty
            OccupancyMap = Map.empty
        }

    /// Determines whether the given location is available.
    let private isAvailable loc idOpt world =
        seq {
            for dx = -1 to 1 do
                for dy = -1 to 1 do
                    Point.create (dx, dy)
        }
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

    /// Determines whether a bond between the given atoms
    /// is valid.
    let private testBond atomA atomB =
        let diff = abs (atomA.Location - atomB.Location)   // get non-negative location difference
        match max diff.X diff.Y, min diff.X diff.Y with
            | 2, 0
            | 2, 1
            | 2, 2
            | 3, 0
            | 3, 1
            | 3, 2 -> true
            | _ -> false

    /// Creates a unidirectional bond between the given atoms.
    let private createBond idA idB bonds =
        let ids =
            Map.tryFind idA bonds
                |> Option.defaultValue Set.empty
                |> Set.add idB
        Map.add idA ids bonds
   
    /// Adds a bond between the given atoms in the given world
    /// if it is valid. Note that this currently allows crossed
    /// bonds. (Moves cannot create crossings, so a world
    /// created without crossed bonds will stay that way.)
    let tryAddBond idA idB world =
        let atomA = world.AtomMap[idA]
        let atomB = world.AtomMap[idB]
        if testBond atomA atomB then
            Some {
                world with
                    BondMap =
                        world.BondMap
                            |> createBond idA idB
                            |> createBond idB idA
            }
        else None

    /// Adds a bond between the given atoms in the given world.
    let addBond idA idB world =
        match tryAddBond idA idB world with
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

    /// Answers the IDs of all atoms bonded with the given atom.
    let private getBonds id world =
        let ids =
            world.BondMap
                |> Map.tryFind id
                |> Option.defaultValue Set.empty
        assert(
            Seq.forall (fun idB ->
                world.BondMap[idB].Contains(id))
                ids)
        ids

    /// Interactions between atom types.
    /// * Negative: attraction
    /// * Positive: repulsion
    /// * Zero: no interaction
    let private interactionTable =
        Map [
            (Hydrophobic, Hydrophobic), -1
            (Hydrophobic, Polar), 0
            (Polar, Hydrophobic), 0
            (Polar, Polar), 0
        ]

    /// Computes the local energy for the given atom. This is
    /// determined by interactions with other atoms that are
    /// in contact (i.e. nearby, but not bonded).
    let private getLocalEnergy id atom world =

        let ids = getBonds id world

        seq { (2, 0); (2, 1); (2, 2) }
            |> Seq.collect (fun (a, b) ->
                seq {
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
                })
            |> Seq.map Point.create
            |> Seq.sumBy (fun delta ->
                world.OccupancyMap
                    |> Map.tryFind (atom.Location + delta)
                    |> Option.map (fun idB ->
                        let atomB = world.AtomMap[idB]
                        if ids.Contains(idB) then 0   // exclude bonded neighbors
                        else interactionTable[atom.Type, atomB.Type])
                    |> Option.defaultValue 0)

    /// Gets the energy level of the given world.
    let getEnergy world =
        world.AtomMap
            |> Map.toSeq
            |> Seq.sumBy (fun (id, atom) ->
                getLocalEnergy id atom world)
            |> fun energy -> energy / 2   // local energy counts each contact twice

    /// Applies the Metropolis rule to the given atom:
    /// * A move that lowers the energy, or leaves it unchanged,
    ///   is always accepted.
    /// * An uphill move is accepted with probability exp(−ΔE/kT).
    let private testMetropolis
        kT (rng : Random) id atomBefore atomAfter world =
        let energyBefore = getLocalEnergy id atomBefore world
        let energyAfter = getLocalEnergy id atomAfter world
        let delta = energyAfter - energyBefore
        delta <= 0 || rng.NextDouble() < exp (-float delta / kT)   // kT: temperature scaled by Boltzmann's constant.

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

    /// Allows one atom in the given world to move.
    let update kT (rng : Random) world =

            // pick atom to move
        let idA = rng.Next(world.AtomMap.Count)   // assume for now that atoms are never removed
        let atom = world.AtomMap[idA]

            // pick direction to move in
        let delta = getRandomDirection rng

            // move atom in that direction provisionally
        let atom' =
            { atom with
                Location = atom.Location + delta }

            // avoids overlap with other atoms? (to-do: check only the leading edge)
        if isAvailable atom'.Location (Some idA) world then

                // all of the atom's bonds are still valid?
            let isValid =
                getBonds idA world
                    |> Seq.forall (fun idB ->
                        let atomB = world.AtomMap[idB]
                        testBond atom' atomB)
            if isValid then

                    // check energy
                if testMetropolis kT rng idA atom atom' world then

                        // move the atom permanently
                    moveAtom idA atom atom' world

                else world
            else world
        else world
