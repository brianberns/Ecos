namespace Ecos.Test

open System
open FsCheck.FSharp
open FsCheck.Xunit
open Xunit
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

    /// Unit steps an atom can attempt.
    let private directions =
        [ 1, 0; 0, -1; -1, 0; 0, 1 ]
            |> List.map Point.create

    /// Oracle: all valid moves in the given world, as (atom ID, step)
    /// pairs. A move is valid if the moved atom overlaps no other atom
    /// and all of its bonds remain allowed.
    let private validMoves (world : World) =
        [
            for KeyValue(id, atom) in world.AtomMap do
                for delta in directions do
                    let loc = atom.Location + delta
                    let clear =
                        world.AtomMap
                            |> Map.forall (fun idB atomB ->
                                idB = id || not (overlaps loc atomB.Location))
                    let bondsOk =
                        world.BondMap
                            |> Map.tryFind id
                            |> Option.defaultValue Set.empty
                            |> Set.forall (fun idB ->
                                isBondVector (world.AtomMap[idB].Location - loc))
                    if clear && bondsOk then
                        id, delta
        ]

    /// Sign of the cross product (b - a) x (c - a).
    let private orientation (a : Point) (b : Point) (c : Point) =
        sign ((b.X - a.X) * (c.Y - a.Y) - (b.Y - a.Y) * (c.X - a.X))

    /// Is point p within the bounding box of segment ab?
    let private inBox (a : Point) (b : Point) (p : Point) =
        min a.X b.X <= p.X && p.X <= max a.X b.X
            && min a.Y b.Y <= p.Y && p.Y <= max a.Y b.Y

    /// Oracle: do closed segments ab and cd intersect (including
    /// touching)?
    let private segmentsIntersect a b c d =
        let o1 = orientation a b c
        let o2 = orientation a b d
        let o3 = orientation c d a
        let o4 = orientation c d b
        (o1 * o2 < 0 && o3 * o4 < 0)
            || (o1 = 0 && inBox a b c)
            || (o2 = 0 && inBox a b d)
            || (o3 = 0 && inBox c d a)
            || (o4 = 0 && inBox c d b)

    /// All pairs of bonds that intersect, excluding pairs that share
    /// an atom.
    let private crossings (world : World) =
        let bonds =
            [
                for KeyValue(idA, ids) in world.BondMap do
                    for idB in ids do
                        if idA < idB then idA, idB
            ]
        let loc id = world.AtomMap[id].Location
        set [
            for i, (a, b) in List.indexed bonds do
                for c, d in List.skip (i + 1) bonds do
                    if a <> c && a <> d && b <> c && b <> d
                        && segmentsIntersect (loc a) (loc b) (loc c) (loc d) then
                        (a, b), (c, d)
        ]

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

    /// Moves made by a single update, as (atom ID, step) pairs.
    let private movesBetween (world : World) (world' : World) =
        Map.toList world.AtomMap
            |> List.choose (fun (id, atom) ->
                let d = world'.AtomMap[id].Location - atom.Location
                if d = Point.create (0, 0) then None else Some (id, d))

    /// Each update makes at most one move, which must be valid, and
    /// leaves the world valid.
    [<Property>]
    let ``update makes a single valid step`` (length : int) (seed : int) (nSteps : int) =
        let length = 2 + abs (length % 20)
        let rng = Random(seed)
        let mutable world = Chain.grow rng length
        let bonds = world.BondMap
        let mutable ok = isValid world
        for _ in 1 .. abs (nSteps % 500) do
            let world' = World.update Chain.athermalKT rng world
            let stepOk =
                match movesBetween world world' with
                    | [] -> true
                    | [ move ] -> List.contains move (validMoves world)
                    | _ -> false
            ok <- ok && stepOk && world'.BondMap = bonds && isValid world'
            world <- world'
        ok

    /// Valid moves are actually made: an update picks an atom and a
    /// direction uniformly, so it should move with probability
    /// (number of valid moves) / (4 N). Over many updates, the number
    /// of moves made must match the sum of those probabilities. This
    /// catches an update that wrongly rejects valid moves, which the
    /// test above cannot.
    [<Fact>]
    let ``update makes valid moves at the expected rate`` () =
        for length in [ 1; 2; 5; 20 ] do
            let rng = Random(length)
            let mutable world = Chain.grow rng length
            let mutable expected, variance, actual = 0.0, 0.0, 0
            for _ in 1 .. 20000 do
                let p =
                    float (validMoves world).Length / float (4 * length)
                expected <- expected + p
                variance <- variance + p * (1.0 - p)
                let world' = World.update Chain.athermalKT rng world
                if not (List.isEmpty (movesBetween world world')) then
                    actual <- actual + 1
                world <- world'
            let tolerance = 5.0 * sqrt variance + 0.5   // 5 standard deviations
            Assert.True(
                abs (float actual - expected) <= tolerance,
                $"N = {length}: {actual} moves, expected {expected:F1} ± {tolerance:F1}")

    /// Topology, exhaustively: atom A (at the origin) is bonded to B
    /// and steps to A'; C and D are bonded to each other. For every
    /// such local configuration that the model accepts both before and
    /// after the step, bond AB must intersect bond CD before the step
    /// if and only if A'B intersects CD after it.
    ///
    /// A single-atom move is valid exactly when the resulting world is
    /// valid, so the model's own tryAddAtom and tryAddBond decide which
    /// configurations count. C ranges over every location from which
    /// bond CD could reach the region swept by the moving bond.
    [<Fact>]
    let ``No single move can carry one bond across another`` () =

            // world with bond A-B and bond C-D, if the model allows it
        let tryBuild a b c d =
            Some World.empty
                |> Option.bind (World.tryAddAtom (Atom.create a))
                |> Option.bind (World.tryAddAtom (Atom.create b))
                |> Option.bind (World.tryAddAtom (Atom.create c))
                |> Option.bind (World.tryAddAtom (Atom.create d))
                |> Option.bind (World.tryAddBond 0 1)
                |> Option.bind (World.tryAddBond 2 3)

        let a = Point.create (0, 0)
        let mutable nChecked, nCrossed = 0, 0
        for bondAB in Chain.bondVectors do
            let b = a + bondAB
            for cx in -7 .. 7 do
                for cy in -7 .. 7 do
                    let c = Point.create (cx, cy)
                    for bondCD in Chain.bondVectors do
                        let d = c + bondCD
                        if (tryBuild a b c d).IsSome then
                            let before = segmentsIntersect a b c d
                            for delta in directions do
                                let a' = a + delta
                                if (tryBuild a' b c d).IsSome then
                                    nChecked <- nChecked + 1
                                    if before then nCrossed <- nCrossed + 1
                                    if segmentsIntersect a' b c d <> before then
                                        Assert.Fail(
                                            $"A {a} -> {a'}, B {b}, C {c}, D {d}: crossing changed")

            // make sure the check isn't vacuous
        Assert.True(nChecked > 100000, $"Only {nChecked} configurations checked")
        Assert.True(nCrossed > 0, "No crossed configurations checked")

    /// Topology: a single step can never carry one bond across another,
    /// so updates must leave the set of intersecting bond pairs
    /// unchanged (including any crossings present at the start).
    [<Property>]
    let ``update never changes which bonds cross`` (length : int) (seed : int) (nSteps : int) =
        let length = 4 + abs (length % 30)
        let rng = Random(seed)
        let mutable world = Chain.grow rng length
        let initial = crossings world
        let mutable ok = true
        for _ in 1 .. abs (nSteps % 1000) do
            world <- World.update Chain.athermalKT rng world
            ok <- ok && crossings world = initial
        ok
