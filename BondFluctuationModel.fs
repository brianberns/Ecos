namespace Ecos

open System
open System.Collections.Immutable

/// Unique index of an atom in a world.
type AtomIndex = int

/// An indivisbile atom.
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
/// Every atom occupies a 2x2 square.
type World =
    {
        /// Atoms in the world.
        Atoms : ImmutableArray<Atom>

        /// Undirected bonds between atoms in the world.
        Bonds : Map<AtomIndex, Set<AtomIndex>>
    }

module World =

    /// An empty world.
    let empty =
        {
            Atoms = ImmutableArray.Empty
            Bonds = Map.empty
        }

    /// Gets a normalized point representing the difference
    /// in position of the given atoms.
    let private getDiff iAtomA iAtomB world =

            // get position difference
        let diff =
            let a = world.Atoms[iAtomA]
            let b = world.Atoms[iAtomB]
            a.Location - b.Location

            // normalize so both coords are non-negative
        let diff = abs diff

            // normalize so first coord is not smaller
        Point.create(
            max diff.X diff.Y,
            min diff.X diff.Y)

    /// Are the locations of the given atoms valid? Overlaps are
    /// not allowed.
    let private testLocation iAtomA iAtomB world =

        let diff = getDiff iAtomA iAtomB world

            // each atom is 2x2, so smaller diffs than that are invalid
        match diff.X, diff.Y with
            | 0, 0
            | 1, 0
            | 1, 1 -> false
            | _ -> true

    let tryAddAtom atom world =
        let iAtomA = world.Atoms.Length
        let world' = { world with Atoms = world.Atoms.Add(atom) }
        let isValid =
            [ 0 .. world.Atoms.Length - 1 ]
                |> Seq.forall (fun iAtomB ->
                    testLocation iAtomA iAtomB world')
        if isValid then Some world'
        else None

    let addAtom atom world =
        match tryAddAtom atom world with
            | Some world -> world
            | None -> failwith "Invalid location"

    let private testBond iAtomA iAtomB world =
        let diff = getDiff iAtomA iAtomB world
        match diff.X, diff.Y with
            | 2, 0
            | 2, 1
            | 2, 2
            | 3, 0
            | 3, 1
            | 3, 2 -> true
            | _ -> false

    let private setBond iAtomA iAtomB bonds =
        let idxs =
            Map.tryFind iAtomA bonds
                |> Option.defaultValue Set.empty
                |> Set.add iAtomB
        Map.add iAtomA idxs bonds
    
    let tryAddBond iAtomA iAtomB world =   // to-do: test for crossed bonds
        if testBond iAtomA iAtomB world then
            Some {
                world with
                    Bonds =
                        world.Bonds
                            |> setBond iAtomA iAtomB
                            |> setBond iAtomB iAtomA
            }
        else None

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

            // move atom in that direction
        let atom' =
            { atom with
                Location = atom.Location + delta }
        let world' =
            { world with
                Atoms = world.Atoms.SetItem(iAtomA, atom') }

            // avoids overlap with other atoms?
        let isValidLocation =
            [ 0 .. world.Atoms.Length - 1 ]
                |> Seq.forall (fun iAtomB ->
                    if iAtomB = iAtomA then true
                    else
                        testLocation iAtomA iAtomB world')

            // all bonds are still valid?
        let isValidBond =
            world.Bonds
                |> Map.tryFind iAtomA
                |> Option.defaultValue Set.empty
                |> Seq.forall (fun iAtomB ->
                    testBond iAtomA iAtomB world')

        if isValidLocation && isValidBond then world'
        else world
