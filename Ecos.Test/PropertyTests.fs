namespace Ecos.Test

open System
open FsCheck.FSharp
open FsCheck.Xunit
open Ecos

/// Fuzz tests of the model's rules, checked against independent
/// statements of those rules.
module PropertyTests =

    /// Oracle: do atoms at the given locations overlap? Each atom
    /// occupies a 2x2 square, so they overlap if they are closer than
    /// 2 along both axes.
    let private overlaps (a : Point) (b : Point) =
        abs (a.X - b.X) < 2 && abs (a.Y - b.Y) < 2

    /// Oracle: is the given vector an allowed bond?
    let private isBondVector (v : Point) =
        Array.contains v Chain.bondVectors

    /// Checks all of a world's invariants.
    let private isValid (world : World) =
        let atoms = world.AtomMap |> Map.toArray

            // no two atoms overlap
        let noOverlaps =
            atoms
                |> Array.forall (fun (idA, a) ->
                    atoms
                        |> Array.forall (fun (idB, b) ->
                            idA = idB || not (overlaps a.Location b.Location)))

            // every bond is allowed, and bonds are symmetric
        let bondsValid =
            world.BondMap
                |> Map.forall (fun idA ids ->
                    ids
                        |> Set.forall (fun idB ->
                            isBondVector
                                (world.AtomMap[idB].Location - world.AtomMap[idA].Location)
                                && world.BondMap[idB].Contains idA))

            // the occupancy map is exactly the atoms' locations
        let occupancyConsistent =
            world.OccupancyMap.Count = world.AtomMap.Count
                && atoms
                    |> Array.forall (fun (id, atom) ->
                        world.OccupancyMap.TryFind atom.Location = Some id)

        noOverlaps && bondsValid && occupancyConsistent

    /// Adding atoms one by one: each is accepted if and only if it
    /// doesn't overlap any atom already present.
    [<Property>]
    let ``tryAddAtom rejects exactly the overlapping atoms`` (coords : list<int * int>) =
        let locs =
            coords
                |> List.map (fun (x, y) -> Point.create (x % 6, y % 6))   // crowd atoms together
        (Some World.empty, locs)
            ||> List.fold (fun worldOpt loc ->
                worldOpt
                    |> Option.bind (fun world ->
                        let expected =
                            world.AtomMap
                                |> Map.forall (fun _ atom -> not (overlaps loc atom.Location))
                        match World.tryAddAtom (Atom.create loc) world, expected with
                            | Some world', true -> Some world'
                            | None, false -> Some world
                            | _ -> None))   // mismatch
            |> Option.exists isValid

    /// A bond between two non-overlapping atoms is accepted if and only
    /// if their separation is an allowed bond vector.
    [<Property>]
    let ``tryAddBond accepts exactly the allowed bond vectors`` (x : int) (y : int) =
        let v = Point.create (x % 5, y % 5)
        let origin = Point.create (0, 0)
        not (overlaps origin v) ==> lazy (
            let world =
                World.empty
                    |> World.addAtom (Atom.create origin)
                    |> World.addAtom (Atom.create v)
            (World.tryAddBond 0 1 world).IsSome = isBondVector v)

    /// Each update moves at most one atom, by a single lattice step
    /// along an axis, and leaves the world valid.
    [<Property>]
    let ``update makes a single valid step`` (length : int) (seed : int) (nSteps : int) =
        let length = 2 + abs (length % 20)
        let rng = Random(seed)
        let mutable world = Chain.grow rng length
        let bonds = world.BondMap
        let mutable ok = isValid world
        for _ in 1 .. abs (nSteps % 500) do
            let world' = World.update rng world
            let moved =
                Map.toList world.AtomMap
                    |> List.choose (fun (id, atom) ->
                        let d = world'.AtomMap[id].Location - atom.Location
                        if d = Point.create (0, 0) then None else Some d)
            let stepOk =
                match moved with
                    | [] -> true
                    | [ d ] -> abs d.X + abs d.Y = 1
                    | _ -> false
            ok <- ok && stepOk && world'.BondMap = bonds && isValid world'
            world <- world'
        ok
