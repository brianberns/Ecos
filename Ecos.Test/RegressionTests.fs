namespace Ecos.Test

open System
open System.Text
open Xunit
open Ecos

/// Regression tests: the model must reproduce, exactly, worlds that
/// were recorded from an earlier version of it.
///
/// These exist to check changes to how the world is stored that should
/// leave its behavior unchanged. Such a change makes the same random
/// draws in the same order, so for a given seed it must produce the
/// very same world, update for update. The other tests can only check
/// behavior statistically, so they can miss a mistake that this
/// catches.
///
/// A change that legitimately alters the order of random draws (or the
/// rules themselves) will break these tests too. In that case, confirm
/// with the other tests that the model is still right, and then record
/// the fingerprints again (see recordFingerprints).
module RegressionTests =

    /// A fingerprint of the given world: a 64-bit hash of every atom's
    /// ID, type and location, and every bond, with the way each strong
    /// bond runs. (Walls are left out, since nothing moves them.)
    /// Locations are measured from the origin, the short way around the
    /// torus, so that a world hashes the same whichever way its grid
    /// labels its coordinates.
    let fingerprint (world : World) =
        let sb = StringBuilder()
        for atom in world.Atoms do
            let loc = Grid.getDisplacement Point.zero atom.Location world.Cells
            sb.Append($"{atom.Id}:{atom.Type}:{loc.X},{loc.Y};")
                |> ignore
        for idPred, idSucc in Bonds.directedStrongBonds world do
            sb.Append($"S{idPred}>{idSucc};") |> ignore
        for idA, idB in Bonds.weakBonds world do
            sb.Append($"W{idA}-{idB};") |> ignore

            // FNV-1a
        let mutable hash = 14695981039346656037UL
        for c in sb.ToString() do
            hash <- (hash ^^^ uint64 c) * 1099511628211UL
        hash.ToString("x16")

    /// Parameters that use every rule: barriers with stack and start
    /// reductions, entropy for both bond types, peeling, and hydrolysis.
    let private allRules =
        TestParameters.basic 1.5
            |> TestParameters.withStrong (fun p -> { p with Energy = 12; Barrier = 6; BarrierStackReduction = 3; StartBarrier = 1; Entropy = 0.5; HydrolysisRate = 0.02; ProtectedHydrolysisRate = 0.02 })
            |> TestParameters.withWeak (fun p -> { p with Energy = 2; Barrier = 0.5; StartBarrier = 0.5; Entropy = 1 })
            |> TestParameters.withPeelBarrier 2.0

    /// A crowded world with no enclosing box: a chain, free atoms,
    /// dimers and walls, with every atom type. Atoms can wander off in
    /// any direction.
    let private crowdedWorld seed =
        let rng = Random(seed)
        Chain.grow rng 20
            |> Chain.withRandomTypes rng
            |> Chain.addSoup rng 10
            |> Chain.addDimers rng 5
            |> Chain.addObstacles rng 10

    /// Parameters for copying a template: a stack takes the whole
    /// barrier off a strong bond, so free atoms paired side by side on
    /// the template join, and stacks form and break constantly.
    let private copying =
        TestParameters.basic 0.7
            |> TestParameters.withStrong (fun p -> { p with Energy = 12; Barrier = 6; BarrierStackReduction = 6; StartBarrier = 0.5 })
            |> TestParameters.withWeak (fun p -> { p with Energy = 2; Entropy = 1 })
            |> TestParameters.withPeelBarrier 3.0

    /// Copying under the rules as in the standard scenarios: strands
    /// bend up to right angles, at a cost, and a strong bond that is one
    /// side of a stack, as a template's and its copy's are, is protected,
    /// hydrolyzing at a hundredth of the usual rate. Hydrolysis is frequent,
    /// so that protection often decides whether a bond breaks.
    let private standardPhysics =
        copying
            |> TestParameters.withBend Strong TestParameters.rightAngleBend 1.5
            |> TestParameters.withHydrolysis Strong 0.3
            |> TestParameters.withProtectedHydrolysis Strong 0.003

    /// A capped template strand in a box, with free atoms that can pair
    /// with it (X and Z) and join into a copy, and caps for the copy's
    /// ends. Each row of free atoms has both kinds, so that the row on
    /// the template's face (above it) can pair with any of its atoms.
    let private templateWorld seed =
        let world = Chain.addTemplate (Random(seed)) TestWorld.empty
        let free =
            [|
                for x in -8 .. 2 .. 8 do
                    let isX = (x / 2) % 2 = 0
                    yield (if isX then AtomType.X else AtomType.Z), Point.create (x, 4)
                    yield (if isX then AtomType.Z else AtomType.X), Point.create (x, -4)
                yield AtomType.Q, Point.create (-8, 7)
                yield AtomType.P, Point.create (8, 7)
                yield AtomType.Q, Point.create (-8, -7)
                yield AtomType.P, Point.create (8, -7)
            |]
        (world, free)
            ||> Array.fold (fun world (atomType, loc) ->
                World.addAtom atomType loc world)
            |> Chain.addBox 10

    /// Two strongly bonded pairs, W0->W1 and X3->X2, facing each other,
    /// in a box so small that their weak bonds form and break
    /// constantly, making and unmaking stacks. (The seed is not used.)
    let private ladderWorld (_seed : int) =
        TestWorld.empty
            |> World.addAtom AtomType.W (Point.create (-1, -1))   // 0
            |> World.addAtom AtomType.W (Point.create (1, -1))    // 1
            |> World.addAtom AtomType.X (Point.create (-1, 1))    // 2
            |> World.addAtom AtomType.X (Point.create (1, 1))     // 3
            |> World.addStrongBond TestParameters.standard 0 1
            |> World.addStrongBond TestParameters.standard 3 2
            |> Chain.addBox 2

    /// Parameters for the ladder: strong bonds can also break and
    /// re-form, with and without a stack.
    let private stacking =
        TestParameters.basic 1.0
            |> TestParameters.withStrong (fun p -> { p with Energy = 2; Barrier = 3; BarrierStackReduction = 2; StartBarrier = 0.5 })
            |> TestParameters.withWeak (fun p -> { p with Energy = 3; Entropy = 0.5 })

    /// The cases: a name, parameters, a function that builds a world
    /// from a seed, and the seed.
    let private cases =
        [
            "cold", TestParameters.create 0.5, crowdedWorld, 1
            "cold", TestParameters.create 0.5, crowdedWorld, 2
            "hot", TestParameters.create 20.0, crowdedWorld, 1
            "hot", TestParameters.create 20.0, crowdedWorld, 2
            "all rules", allRules, crowdedWorld, 1
            "all rules", allRules, crowdedWorld, 2
            "copying", copying, templateWorld, 1
            "copying", copying, templateWorld, 2
            "standard physics", standardPhysics, templateWorld, 1
            "standard physics", standardPhysics, templateWorld, 2
            "stacking", stacking, ladderWorld, 1
            "stacking", stacking, ladderWorld, 2
        ]

    /// Makes exactly the given number of updates to the given world. A
    /// sweep makes the same updates as single updates do, with the same
    /// random numbers, but without copying the world for each one, so
    /// this sweeps as far as it can and makes the rest singly.
    let private update parms rng nUpdates (world : World) =
        let nAtoms = world.Atoms.Length
        let mutable world = Ecos.World.sweep parms rng (nUpdates / nAtoms) world
        for _ in 1 .. nUpdates % nAtoms do
            world <- World.updateWith parms rng world
        world

    /// Runs the given case and answers fingerprints of its world after
    /// 10,000, 20,000 and 30,000 updates, and then after 20 more sweeps.
    let private runCase parms createWorld seed =
        let rng = Random(100 + seed)
        let mutable world = createWorld seed
        [
            for _ in 1 .. 3 do
                world <- update parms rng 10000 world
                fingerprint world
            for _ in 1 .. 20 do
                world <- Ecos.World.sweep parms rng 1 world
            fingerprint world
        ]

    /// Fingerprints recorded once strong bonds ran one way and atoms
    /// paired only on their strands' faces (commit 46dbe18), which
    /// changed every world. The fingerprints now include the way each
    /// strong bond runs.
    let private expected =
        Map [
            ("cold", 1), [ "04424eb09c8e601b"; "1a3783c2d2b2fba9"; "67123c1c397bcfbd"; "a361a176bf9f1d8f" ]
            ("cold", 2), [ "80a5c947706d8d87"; "64d9d095d21efa82"; "41185d73d1169367"; "9a00cc541413280a" ]
            ("hot", 1), [ "f169a4939732205b"; "a9b9e26757509582"; "83f1aa6ee659033f"; "48cea468904b1f8b" ]
            ("hot", 2), [ "bfcb7db8454da030"; "a4494bcc55851057"; "2e8d7a8a293c2e46"; "eb53175bd0b14093" ]
            ("all rules", 1), [ "42872f9026807768"; "f9622cb689364348"; "cdb83957dfca6f88"; "3b6412e09c9fce77" ]
            ("all rules", 2), [ "d708b53f7fdf0522"; "c72537d23529bdb4"; "65a7e364e65d1f14"; "af6f5c0f08cb7c50" ]
            ("copying", 1), [ "1351b95f75299a28"; "713396239b5694e2"; "5bb45f0cec31f3ad"; "24d663bf9106a6f9" ]
            ("copying", 2), [ "70abc22c5402f708"; "74886ca1edd4ae7a"; "7010108f0dce90f5"; "2ac0facb51fb7618" ]
            ("standard physics", 1), [ "7df5c465c1537a52"; "1b811df6df9aa126"; "c0347cedf893fd22"; "472c4a0267037c82" ]
            ("standard physics", 2), [ "b641acb178112202"; "075d3d16a7cee4b4"; "beac085f168abfba"; "a8c6b4a0ca6cc0c5" ]
            ("stacking", 1), [ "1d33eb3b55e8ba17"; "67852f20b4a13787"; "0e89c8945e574334"; "a7ff5a14709dda05" ]
            ("stacking", 2), [ "4c23cd4681d31c32"; "bbaece89731e9625"; "10efc6e134cce480"; "899c967961540d89" ]
        ]

    /// Prints the fingerprints of every case, in the form used above.
    /// For re-recording them after a legitimate change.
    let recordFingerprints () =
        for name, parms, createWorld, seed in cases do
            let prints = runCase parms createWorld seed
            let quoted = prints |> List.map (sprintf "\"%s\"") |> String.concat "; "
            printfn "            (\"%s\", %d), [ %s ]" name seed quoted

    /// The model reproduces the recorded worlds exactly.
    [<Fact>]
    let ``Recorded worlds are reproduced exactly`` () =
        let actual =
            cases
                |> List.toArray
                |> Array.Parallel.map (fun (_, parms, createWorld, seed) ->
                    runCase parms createWorld seed)   // the cases are independent
        for (name, _, _, seed), prints in Array.zip (List.toArray cases) actual do
            Assert.Equal<string list>(expected[name, seed], prints)

    /// Updating a world never changes it: update and sweep answer a new
    /// world and leave the one they were given as it was. Several worlds
    /// along the way are kept, and checked again at the end. Running
    /// the same updates twice from the same world, with the same seed,
    /// must also give the same result.
    [<Fact>]
    let ``Updates leave the given world unchanged`` () =
        let run () =
            let rng = Random(7)
            let mutable world = crowdedWorld 3
            let kept = ResizeArray()
            for _ in 1 .. 10 do
                kept.Add(world, fingerprint world)
                for _ in 1 .. 1000 do
                    world <- World.updateWith allRules rng world
                kept.Add(world, fingerprint world)
                world <- Ecos.World.sweep allRules rng 1 world
            kept, fingerprint world

        let kept, final = run ()
        for world, print in kept do
            Assert.Equal(print, fingerprint world)
        Assert.Equal(final, snd (run ()))

    /// Building a world never changes the one it starts from: adding a
    /// wall, an atom or a bond answers a new world and leaves the old
    /// one as it was, including every cell. The new wall and atom are
    /// placed inside the region the old world already covers.
    [<Fact>]
    let ``Adding to a world leaves the original unchanged`` () =
        let world =
            TestWorld.empty
                |> World.addWall (Point.create (0, 0))
                |> World.addWall (Point.create (12, 12))
                |> World.addAtom AtomType.W (Point.create (4, 4))   // 0
                |> World.addAtom AtomType.W (Point.create (6, 4))   // 1
                |> World.addAtom AtomType.X (Point.create (4, 6))   // 2
        let state (world : World) =
            [ for x in -2 .. 14 do
                for y in -2 .. 14 do
                    string world[x, y] ],
            fingerprint world,
            List.ofArray world.Walls
        let before = state world
        World.addWall (Point.create (8, 8)) world |> ignore
        World.addAtom AtomType.X (Point.create (8, 2)) world |> ignore
        World.addStrongBond TestParameters.standard 0 1 world |> ignore
        World.addWeakBond TestParameters.standard 0 2 world |> ignore
        Assert.Equal(before, state world)
