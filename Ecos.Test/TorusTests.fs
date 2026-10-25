namespace Ecos.Test

open System
open Xunit
open Ecos

/// Tests of worlds that are tori, whose opposite edges meet.
module TorusTests =

    /// Parameters that use most rules, hot enough that bonds form and
    /// break often, including bending (up to right angles, at a cost)
    /// and protection from hydrolysis, which both depend on where atoms
    /// are relative to each other.
    let private parameters =
        TestParameters.basic 1.0
            |> TestParameters.withStrong (fun p -> { p with Energy = 6; Barrier = 3; BarrierStackReduction = 3; StartBarrier = 0.5; Entropy = 0.5; HydrolysisRate = 0.01; ProtectedHydrolysisRate = 0.002 })
            |> TestParameters.withWeak (fun p -> { p with Energy = 2; Entropy = 1 })
            |> TestParameters.withBend Strong TestParameters.rightAngleBend 1.5
            |> TestParameters.withPeelBarrier 2.0

    /// A capped template with free atoms that can copy it, in a box at the
    /// center of a torus of the given size, well away from its edges.
    let private boxedTemplateWorld size seed =
        let world = Chain.addTemplate (Random(seed)) (World.createSquare size)
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
            (World.create world.Size, world.Walls)
                ||> Array.fold (fun torus loc -> World.addWall (loc + offset) torus)
        let torus =
            (torus, world.Atoms)
                ||> Array.fold (fun torus atom ->
                    World.addAtom atom.Type (atom.Location + offset) torus)
        let torus =
            (torus, Bonds.strongBonds world)
                ||> Seq.fold (fun torus (idA, idB) -> World.addStrongBond TestParameters.standard idA idB torus)
        (torus, Bonds.weakBonds world)
            ||> Seq.fold (fun torus (idA, idB) -> World.addWeakBond TestParameters.standard idA idB torus)

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
                    Assert.Equal(torus.NormalizeLocation(atom.Location + offset), torus.Atoms[atom.Id].Location)
                Assert.Equal<Set<int * int>>(Bonds.strongBonds world, Bonds.strongBonds torus)
                Assert.Equal<Set<int * int>>(Bonds.weakBonds world, Bonds.weakBonds torus)

                // make sure the check isn't vacuous: bonds really did change
            Assert.NotEqual<Set<int * int>>(Bonds.weakBonds (boxedTemplateWorld size seed), Bonds.weakBonds world)

    /// In an open torus with no walls, a chain and free atoms wander
    /// freely across the edges, and the world stays valid throughout:
    /// every atom's location is inside the torus, and the world obeys
    /// every rule (see Validity.isValid), with distances measured around
    /// the torus.
    [<Fact>]
    let ``Atoms wander across the edges of an open torus and the world stays valid`` () =
        let size = 16
        let rng = Random(0)
        let mutable world =
            (World.createSquare size, [ for x in -6 .. 2 .. 6 -> x ])
                ||> List.fold (fun world x ->
                    let world = World.addAtom AtomType.W (Point.create (x, 0)) world
                    if x > -6 then World.addStrongBond TestParameters.standard (world.Atoms.Length - 2) (world.Atoms.Length - 1) world
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
                Assert.InRange(loc.X, 0, size - 1)
                Assert.InRange(loc.Y, 0, size - 1)
                if abs (loc.X - start[atom.Id].X) > size / 2 || abs (loc.Y - start[atom.Id].Y) > size / 2 then
                    crossed <- true
            Assert.True(Validity.isValid parameters world, "World became invalid")
        Assert.True(crossed, "No atom crossed an edge")
