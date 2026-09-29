namespace Ecos

open System
open System.Collections.Immutable

/// Unique index of an atom in a world.
type AtomIndex = int

/// An indivisble atom.
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
        /// Atoms in the world.
        Atoms : ImmutableArray<Atom>

        /// Undirected bonds between atoms in the world.
        Bonds : Map<AtomIndex, Set<AtomIndex>>

        /// Maps each atom's location to its index.
        OccupancyMap : Map<Point, AtomIndex>
    }

module World =

    /// An empty world.
    let empty =
        {
            Atoms = ImmutableArray.Empty
            Bonds = Map.empty
            OccupancyMap = Map.empty
        }

    /// Gets a normalized point representing the difference
    /// in position of the given atom locations.
    let private getDiff (locA : Point) (locB : Point) =
        let diff = abs (locA - locB)   // get non-negative position difference
        Point.create(                  // normalize so first coord is not smaller
            max diff.X diff.Y,
            min diff.X diff.Y)

    /// Determines whether the given atom locations are valid.
    /// Overlaps are not allowed.
    let private testLocation locA locB =

        let diff = getDiff locA locB

            // each atom is 2x2, so smaller diffs than that are invalid
        match diff.X, diff.Y with
            | 0, 0
            | 1, 0
            | 1, 1 -> false
            | _ -> true

    /// Adds the given atom to the given world if it is valid.
    let tryAddAtom atom world =
        let isValid =
            world.Atoms
                |> Seq.forall (fun atomB ->
                    testLocation atom.Location atomB.Location)                    
        if isValid then
            assert(
                world.OccupancyMap
                    |> Map.tryFind atom.Location
                    |> Option.isNone)
            let iAtom = world.Atoms.Length
            Some {
                world with
                    Atoms = world.Atoms.Add(atom)
                    OccupancyMap =
                        world.OccupancyMap
                            |> Map.add atom.Location iAtom
            }
        else None

    /// Adds the given atom to the given world.
    let addAtom atom world =
        match tryAddAtom atom world with
            | Some world -> world
            | None -> failwith "Invalid location"

    /// Determines whether a bond between the given atom locations
    /// is valid.
    let private testBond locA locB =
        let diff = getDiff locA locB
        match diff.X, diff.Y with
            | 2, 0
            | 2, 1
            | 2, 2
            | 3, 0
            | 3, 1
            | 3, 2 -> true
            | _ -> false

    /// Creates a unidirectional bond between the given atoms.
    let private createBond iAtomA iAtomB bonds =
        let idxs =
            Map.tryFind iAtomA bonds
                |> Option.defaultValue Set.empty
                |> Set.add iAtomB
        Map.add iAtomA idxs bonds
   
    /// Adds a bond between the given atoms in the given world
    /// if it is valid. Note that this currently allows crossed
    /// bonds. (Moves cannot create crossings, so a world
    /// created without crossed bonds will stay that way.)
    let tryAddBond iAtomA iAtomB world =
        let atomA = world.Atoms[iAtomA]
        let atomB = world.Atoms[iAtomB]
        if testBond atomA.Location atomB.Location then
            Some {
                world with
                    Bonds =
                        world.Bonds
                            |> createBond iAtomA iAtomB
                            |> createBond iAtomB iAtomA
            }
        else None

    /// Adds a bond between the given atoms in the given world.
    let addBond iAtomA iAtomB world =
        match tryAddBond iAtomA iAtomB world with
            | Some world -> world
            | None -> failwith "Invalid bond"

    /// Allows one atom in the given world to move.
    let update (rng : Random) world =

            // pick atom to move
        let iAtomA = rng.Next(world.Atoms.Length)
        let atom = world.Atoms[iAtomA]

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
        let isValid =
            seq {
                for dx in [ -1 .. 1 ] do
                    for dy in [ -1 .. 1 ] do
                        atom'.Location + Point.create (dx, dy)
            }
                |> Seq.forall (fun loc ->
                    world.OccupancyMap
                        |> Map.tryFind loc
                        |> Option.map ((=) iAtomA)
                        |> Option.defaultValue true)
        assert(
            Seq.indexed world.Atoms
                |> Seq.forall (fun (iAtomB, atomB) ->
                    if iAtomB = iAtomA then true
                    else
                        testLocation atom'.Location atomB.Location)
                = isValid)

            // all bonds are still valid?
        if isValid then
            let isValid =
                world.Bonds
                    |> Map.tryFind iAtomA
                    |> Option.defaultValue Set.empty
                    |> Seq.forall (fun iAtomB ->
                        let atomB = world.Atoms[iAtomB]
                        testBond atom'.Location atomB.Location)
            if isValid then
                assert(world.OccupancyMap[atom.Location] = iAtomA)
                {
                    world with
                        Atoms = world.Atoms.SetItem(iAtomA, atom')
                        OccupancyMap =
                            world.OccupancyMap
                                |> Map.remove atom.Location
                                |> Map.add atom'.Location iAtomA
                }
            else world
        else world
