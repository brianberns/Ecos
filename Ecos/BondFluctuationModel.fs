namespace Ecos

open System

/// Unique ID of an atom in a world.
type AtomId = int

/// An indivisible atom.
type Atom =
    {
        /// Location of the atom in the world.
        Location : Point
    }

module Atom =

    /// Creates an atom.
    let create location =
        {
            Location = location
        }

/// A lattice world containing atoms bonded into molecules.
/// Every atom occupies a 2x2 square around its central
/// location on the lattice.
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

    /// Gets a normalized point representing the difference
    /// in location of the given atoms.
    let private getDiff atomA atomB =
        let diff = abs (atomA.Location - atomB.Location)   // get non-negative location difference
        Point.create(                                      // normalize so first coord is not smaller
            max diff.X diff.Y,
            min diff.X diff.Y)

    /// Determines whether the given location is available.
    let private isAvailable
        (loc : Point) (idOpt : Option<AtomId>) world =
        seq {
            for dx = -1 to 1 do
                for dy = -1 to 1 do
                    loc + Point.create (dx, dy)
        }
            |> Seq.forall (fun neighbor ->
                world.OccupancyMap
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

    /// Determines whether a bond between the given atoms
    /// is valid.
    let private testBond atomA atomB =
        let diff = getDiff atomA atomB
        match diff.X, diff.Y with
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

    /// Allows one atom in the given world to move.
    let update (rng : Random) world =

            // pick atom to move
        let idA = rng.Next(world.AtomMap.Count)   // assume for now that atoms are never removed
        let atom = world.AtomMap[idA]

            // pick direction to move in
        let delta =
            Point.create <|
                match rng.Next(4) with
                    | 0 ->  1,  0
                    | 1 ->  0, -1
                    | 2 -> -1,  0
                    | 3 ->  0,  1
                    | _ -> failwith "Unexpected"

            // move atom in that direction provisionally
        let atom' =
            { atom with
                Location = atom.Location + delta }

            // avoids overlap with other atoms? (to-do: check only the leading edge)
        if isAvailable atom'.Location (Some idA) world then

                // all bonds are still valid?
            let isValid =
                world.BondMap
                    |> Map.tryFind idA
                    |> Option.defaultValue Set.empty
                    |> Seq.forall (fun idB ->
                        let atomB = world.AtomMap[idB]
                        testBond atom' atomB)
            if isValid then
                assert(world.OccupancyMap[atom.Location] = idA)
                {
                    world with
                        AtomMap =
                            world.AtomMap
                                |> Map.add idA atom'
                        OccupancyMap =
                            world.OccupancyMap
                                |> Map.remove atom.Location
                                |> Map.add atom'.Location idA
                }
            else world
        else world
