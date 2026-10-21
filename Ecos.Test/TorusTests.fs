namespace Ecos.Test

open System
open Xunit
open Ecos

/// Tests of worlds that are tori, whose opposite edges meet.
module TorusTests =

    /// Parameters that use most rules, hot enough that bonds form and
    /// break often.
    let private parameters =
        {
            GetTemperature = Parameters.getUniformTemperature 1.0
            BondParameterMap =
                Map [
                    Strong, { Energy = 6; PairEnergyMap = Map.empty; Barrier = 3; BarrierStackReduction = 3; StartBarrier = 0.5; Entropy = 0.5; HydrolysisRate = 0.01 }
                    Weak, { Energy = 2; PairEnergyMap = Map.empty; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 0; Entropy = 1; HydrolysisRate = 0 }
                ]
            PeelBarrier = 2.0
        }

    /// A capped template with free atoms that can copy it, in a box at the
    /// center of a torus of the given size, well away from its edges.
    let private boxedTemplateWorld size seed =
        let rng = Random(seed)
        let types =
            [|
                AtomType.P
                for _ in 1 .. 6 do
                    if rng.Next(2) = 0 then AtomType.W else AtomType.Y
                AtomType.Q
            |]
        let world =
            (World.create size size, Array.indexed types)
                ||> Array.fold (fun world (i, atomType) ->
                    let world = World.addAtom atomType (Point.create (2 * i - 8, 0)) world
                    if i > 0 then World.addStrongBond (i - 1) i world
                    else world)
        let free =
            [|
                for x in -8 .. 2 .. 8 do
                    AtomType.X, Point.create (x, 4)
                    AtomType.Z, Point.create (x, -4)
                    AtomType.W, Point.create (x, 8)
                    AtomType.Y, Point.create (x, -8)
                AtomType.Q, Point.create (-8, 6)
                AtomType.P, Point.create (8, 6)
            |]
        (world, free)
            ||> Array.fold (fun world (atomType, loc) ->
                World.addAtom atomType loc world)
            |> Chain.addBox 10

    /// The given world, rebuilt in a torus of the same size and shifted
    /// by the given amount: walls, then atoms in ID order, then bonds.
    let private shift (offset : Point) (world : World) =
        let torus =
            (World.create world.Width world.Height, world.Walls)
                ||> Array.fold (fun torus loc -> World.addWall (loc + offset) torus)
        let torus =
            (torus, world.Atoms)
                ||> Array.fold (fun torus atom ->
                    World.addAtom atom.Type (atom.Location + offset) torus)
        let torus =
            (torus, Bonds.strongBonds world)
                ||> Seq.fold (fun torus (idA, idB) -> World.addStrongBond idA idB torus)
        (torus, Bonds.weakBonds world)
            ||> Seq.fold (fun torus (idA, idB) -> World.addWeakBond idA idB torus)

    /// Dynamics depend only on where atoms are relative to each other.
    /// So a boxed world at the center of a torus, and the same world
    /// shifted across the edges of a torus (so that its box straddles
    /// them) and run with the same random numbers, must stay the same
    /// world, shifted, update for update: every atom in the same place
    /// (wrapped), and the same bonds. Any mistake in wrapping moves,
    /// bond ranges, angles, crossings or occupancy would show up as a
    /// difference.
    [<Fact>]
    let ``A world shifted across the edges of a torus behaves exactly as when away from them`` () =
        let size = 32   // the box spans 26 sites, so it fits
        for seed, offset in [ 1, Point.create (16, 16); 2, Point.create (16, 3); 3, Point.create (-5, 16) ] do
            let world = boxedTemplateWorld size seed
            let torus = shift offset world
            let rngA, rngB = Random(seed), Random(seed)
            let mutable world = world
            let mutable torus = torus
            for _ in 1 .. 20 do
                world <- World.sweep parameters rngA 50 world
                torus <- World.sweep parameters rngB 50 torus
                for atom in world.Atoms do
                    Assert.Equal(torus.Wrap(atom.Location + offset), torus.Atoms[atom.Id].Location)
                Assert.Equal<Set<int * int>>(Bonds.strongBonds world, Bonds.strongBonds torus)
                Assert.Equal<Set<int * int>>(Bonds.weakBonds world, Bonds.weakBonds torus)

                // make sure the check isn't vacuous: bonds really did change
            Assert.NotEqual<Set<int * int>>(Bonds.weakBonds (boxedTemplateWorld size seed), Bonds.weakBonds world)

    /// In an open torus with no walls, a chain and free atoms wander
    /// freely across the edges, and the world stays valid throughout:
    /// every atom's location is inside the torus and is where the grid
    /// says it is, no two atoms overlap (measured around the torus), and
    /// every bond is within range (measured the same way).
    [<Fact>]
    let ``Atoms wander across the edges of an open torus and the world stays valid`` () =
        let size = 16
        let rng = Random(0)
        let mutable world =
            (World.create size size, [ for x in -6 .. 2 .. 6 -> x ])
                ||> List.fold (fun world x ->
                    let world = World.addAtom AtomType.W (Point.create (x, 0)) world
                    if x > -6 then World.addStrongBond (world.Atoms.Length - 2) (world.Atoms.Length - 1) world
                    else world)
        world <-
            (world, [ for x in -6 .. 4 .. 6 do for y in [ -5; 5 ] -> x, y ])
                ||> List.fold (fun world (x, y) ->
                    World.addAtom AtomType.X (Point.create (x, y)) world)
        let start = world.Atoms |> Array.map _.Location
        let mutable crossed = false
        for _ in 1 .. 200 do
            world <- World.sweep parameters rng 10 world
            for atom in world.Atoms do
                let loc = atom.Location
                Assert.InRange(loc.X, -size / 2, size / 2 - 1)
                Assert.InRange(loc.Y, -size / 2, size / 2 - 1)
                Assert.Equal(Cell.Atom atom.Id, world[loc])
                if abs (loc.X - start[atom.Id].X) > size / 2 || abs (loc.Y - start[atom.Id].Y) > size / 2 then
                    crossed <- true
            for a in world.Atoms do
                for b in world.Atoms do
                    if a.Id < b.Id then
                        let d = world.Displacement(a.Location, b.Location)
                        Assert.True(
                            abs d.X >= 2 || abs d.Y >= 2,
                            $"Atoms {a.Id} and {b.Id} overlap")
            for idA, idB in Seq.append (Bonds.strongBonds world) (Bonds.weakBonds world) do
                let d = world.Displacement(world.Atoms[idA].Location, world.Atoms[idB].Location)
                Assert.True(
                    max (abs d.X) (abs d.Y) <= 3,
                    $"Bond {idA}-{idB} spans {d}")
        Assert.True(crossed, "No atom crossed an edge")
