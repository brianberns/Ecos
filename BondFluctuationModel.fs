namespace Ecos

type AtomId = int

type Atom =
    {
        Location : Point
    }

module Atom =

    let create location =
        {
            Location = location
        }

open System.Collections.Immutable

type World =
    {
        Atoms : ImmutableArray<Atom>
        Bonds : Map<AtomId, Set<AtomId>>
    }

module World =

    open System

    let initialize (atoms : seq<_>) =
        {
            Atoms = ImmutableArray.ToImmutableArray(atoms)
            Bonds = Map.empty
        }

    let private testBond iAtomA iAtomB world =
        let diff =
            let diff =
                let a = world.Atoms[iAtomA]
                let b = world.Atoms[iAtomB]
                abs (a.Location - b.Location)
            Point.create(
                max diff.X diff.Y,
                min diff.X diff.Y)
        match diff.X, diff.Y with
            | 2, 0
            | 2, 1
            | 2, 2
            | 3, 0 -> true
            | _ -> false

    let private testLocation iAtomA iAtomB world =
        let diff =
            let diff =
                let a = world.Atoms[iAtomA]
                let b = world.Atoms[iAtomB]
                abs (a.Location - b.Location)
            Point.create(
                max diff.X diff.Y,
                min diff.X diff.Y)
        match diff.X, diff.Y with
            | 0, 0
            | 1, 0
            | 1, 1 -> false
            | _ -> true

    let private setBond iAtomA iAtomB bonds =
        let idxs =
            match Map.tryFind iAtomA bonds with
                | Some idxs -> idxs
                | None -> Set.empty
                |> Set.add iAtomB
        Map.add iAtomA idxs bonds

    let tryAddBond iAtomA iAtomB world =
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

    let getBonds world =
        world.Bonds
            |> Map.toSeq
            |> Seq.collect (fun (iAtomA, idxs) ->
                Seq.map (fun iAtomB ->
                    world.Atoms[iAtomA], world.Atoms[iAtomB])
                    idxs)

    /// Allows one atom in the given world to move.
    let update (rng : Random) world =

            // pick atom to move
        let iAtomA = rng.Next(world.Atoms.Length)
        let atom = world.Atoms[iAtomA]

            // pick direction to move in
        let delta =
            match rng.Next(8) with
                | 0 ->  1,  0
                | 1 ->  1,  1
                | 2 ->  0,  1
                | 3 -> -1,  1
                | 4 -> -1,  0
                | 5 -> -1, -1
                | 6 ->  0, -1
                | 7 ->  1, -1
                | _ -> failwith "Unexpected"
                |> Point.create

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
            world'.Bonds[iAtomA]
                |> Seq.forall (fun iAtomB ->
                    testBond iAtomA iAtomB world')

        if isValidLocation && isValidBond then world'
        else world
