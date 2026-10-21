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
    /// ID, type and location, and every bond. (Walls are left out, since
    /// nothing moves them.)
    let fingerprint (world : World) =
        let sb = StringBuilder()
        for atom in world.Atoms do
            sb.Append($"{atom.Id}:{atom.Type}:{atom.Location.X},{atom.Location.Y};")
                |> ignore
        for idA, idB in Bonds.strongBonds world do
            sb.Append($"S{idA}-{idB};") |> ignore
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
        {
            GetTemperature = Parameters.getUniformTemperature 1.5
            BondParameterMap =
                Map [
                    Strong, { Energy = 12; PairEnergyMap = Map.empty; Barrier = 6; BarrierStackReduction = 3; StartBarrier = 1; Entropy = 0.5; HydrolysisRate = 0.02 }
                    Weak, { Energy = 2; PairEnergyMap = Map.empty; Barrier = 0.5; BarrierStackReduction = 0; StartBarrier = 0.5; Entropy = 1; HydrolysisRate = 0 }
                ]
            PeelBarrier = 2.0
        }

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
        {
            GetTemperature = Parameters.getUniformTemperature 0.7
            BondParameterMap =
                Map [
                    Strong, { Energy = 12; PairEnergyMap = Map.empty; Barrier = 6; BarrierStackReduction = 6; StartBarrier = 0.5; Entropy = 0; HydrolysisRate = 0 }
                    Weak, { Energy = 2; PairEnergyMap = Map.empty; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 0; Entropy = 1; HydrolysisRate = 0 }
                ]
            PeelBarrier = 3.0
        }

    /// A capped template strand in a box, with free atoms that can pair
    /// with it (X and Z) and join into a copy, and caps for the copy's
    /// ends.
    let private templateWorld seed =
        let rng = Random(seed)
        let template =
            [| AtomType.P |]
                |> Array.append <| Array.init 6 (fun _ ->
                    if rng.Next(2) = 0 then AtomType.W else AtomType.Y)
                |> Array.append <| [| AtomType.Q |]
        let world =
            (TestWorld.empty, Array.indexed template)
                ||> Array.fold (fun world (i, atomType) ->
                    let world = World.addAtom atomType (Point.create (2 * i - 8, 0)) world
                    if i > 0 then World.addStrongBond (i - 1) i world
                    else world)
        let free =
            [|
                for x in -8 .. 2 .. 8 do
                    yield AtomType.X, Point.create (x, 4)
                    yield AtomType.Z, Point.create (x, -4)
                yield AtomType.Q, Point.create (-8, 7)
                yield AtomType.P, Point.create (8, 7)
                yield AtomType.Q, Point.create (-8, -7)
                yield AtomType.P, Point.create (8, -7)
            |]
        (world, free)
            ||> Array.fold (fun world (atomType, loc) ->
                World.addAtom atomType loc world)
            |> Chain.addBox 10

    /// Two strongly bonded pairs, W0-W1 and X2-X3, in a box so small
    /// that their weak bonds form and break constantly, making and
    /// unmaking stacks. (The seed is not used.)
    let private ladderWorld (_seed : int) =
        TestWorld.empty
            |> World.addAtom AtomType.W (Point.create (-1, -1))   // 0
            |> World.addAtom AtomType.W (Point.create (1, -1))    // 1
            |> World.addAtom AtomType.X (Point.create (-1, 1))    // 2
            |> World.addAtom AtomType.X (Point.create (1, 1))     // 3
            |> World.addStrongBond 0 1
            |> World.addStrongBond 2 3
            |> Chain.addBox 2

    /// Parameters for the ladder: strong bonds can also break and
    /// re-form, with and without a stack.
    let private stacking =
        {
            GetTemperature = Parameters.getUniformTemperature 1.0
            BondParameterMap =
                Map [
                    Strong, { Energy = 2; PairEnergyMap = Map.empty; Barrier = 3; BarrierStackReduction = 2; StartBarrier = 0.5; Entropy = 0; HydrolysisRate = 0 }
                    Weak, { Energy = 3; PairEnergyMap = Map.empty; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 0; Entropy = 0.5; HydrolysisRate = 0 }
                ]
            PeelBarrier = Double.PositiveInfinity
        }

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
            "stacking", stacking, ladderWorld, 1
            "stacking", stacking, ladderWorld, 2
        ]

    /// Runs the given case and answers fingerprints of its world after
    /// 10,000, 20,000 and 30,000 single updates, and then after 20
    /// sweeps.
    let private runCase parms createWorld seed =
        let rng = Random(100 + seed)
        let mutable world = createWorld seed
        [
            for _ in 1 .. 3 do
                for _ in 1 .. 10000 do
                    world <- World.updateWith parms rng world
                fingerprint world
            for _ in 1 .. 20 do
                world <- Ecos.World.sweep parms rng 1 world
            fingerprint world
        ]

    /// Fingerprints recorded from the model once strong bonds could
    /// hydrolyze (commit 45ddcd6). Every world became a torus after that,
    /// which changed none of them except ("hot", 1), whose atoms wander
    /// far enough to reach around it; that one was recorded again.
    let private expected =
        Map [
            ("cold", 1), [ "39b648c110a17624"; "99ce0bfa34d59eb3"; "31b991455bb9c20c"; "ca699a36166ef37c" ]
            ("cold", 2), [ "235be71c8731e8d4"; "e714ff43a59914d4"; "343d78326d527865"; "0442bd92fa09268b" ]
            ("hot", 1), [ "1a179ff72bf65fde"; "bdb09c90e4bb8610"; "43fe617d446bf4ca"; "c274af3b727b0c7d" ]
            ("hot", 2), [ "634be7d4781ce79d"; "2e4612460bad8ab6"; "e09408eb0a1812ab"; "8fec0e9eced65629" ]
            ("all rules", 1), [ "a7dfd2b3d39d0c5c"; "704fa118c60c9d94"; "b88cce0943418f75"; "5e6e05223488425c" ]
            ("all rules", 2), [ "587bcd6cb790917b"; "a360f2e65d2feec8"; "cd302bdcbfd0145c"; "b7f03e2cf82e1125" ]
            ("copying", 1), [ "a7c6ff2eae63f7f4"; "9c525d3403942ad3"; "729ba3208c42eb17"; "b404136268009063" ]
            ("copying", 2), [ "1ff9eca18af2d82a"; "c221658a85ba0bda"; "712999403e57158c"; "2f0f7893aa224dde" ]
            ("stacking", 1), [ "a7d79956c3e49538"; "f462bac3b9350b3d"; "472f912a46f85b54"; "cb8aff596006fd6f" ]
            ("stacking", 2), [ "3699e163ec09dfcc"; "9742142691cf2eff"; "8a4fbe5b8398f6cd"; "d0fbdd4aa440b5d0" ]
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
        for name, parms, createWorld, seed in cases do
            Assert.Equal<string list>(expected[name, seed], runCase parms createWorld seed)

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
        World.addStrongBond 0 1 world |> ignore
        World.addWeakBond 0 2 world |> ignore
        Assert.Equal(before, state world)
