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
    /// reductions, entropy for both bond types, peeling, and caps coming
    /// off.
    let private allRules =
        {
            Temperature = 1.5
            BondParameterMap =
                Map [
                    Strong, { Energy = 12; Barrier = 6; BarrierStackReduction = 3; StartBarrier = 1; Entropy = 0.5 }
                    Weak, { Energy = 2; Barrier = 0.5; BarrierStackReduction = 0; StartBarrier = 0.5; Entropy = 1 }
                ]
            PeelBarrier = 2.0
            CapPeelBarrier = 4.0
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
            Temperature = 0.7
            BondParameterMap =
                Map [
                    Strong, { Energy = 12; Barrier = 6; BarrierStackReduction = 6; StartBarrier = 0.5; Entropy = 0 }
                    Weak, { Energy = 2; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 0; Entropy = 1 }
                ]
            PeelBarrier = 3.0
            CapPeelBarrier = Double.PositiveInfinity
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
            (World.empty, Array.indexed template)
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
        World.empty
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
            Temperature = 1.0
            BondParameterMap =
                Map [
                    Strong, { Energy = 2; Barrier = 3; BarrierStackReduction = 2; StartBarrier = 0.5; Entropy = 0 }
                    Weak, { Energy = 3; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 0; Entropy = 0.5 }
                ]
            PeelBarrier = Double.PositiveInfinity
            CapPeelBarrier = Double.PositiveInfinity
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

    /// Fingerprints recorded from the model as it was before the world
    /// stored its atoms and occupancy in arrays (commit 96e1595).
    let private expected =
        Map [
            ("cold", 1), [ "6081c98f05226c6c"; "0df76910647c17ca"; "9a28e0c71d109cad"; "70e9bd331d848370" ]
            ("cold", 2), [ "4be548fc409955f3"; "bbb5b2dd25899c02"; "772352ca28f390e1"; "9a9e152f2f2dace8" ]
            ("hot", 1), [ "cbdf1af662353506"; "cbe51102928a5690"; "956bbc6cca9db2e6"; "2d97d9312c554ab6" ]
            ("hot", 2), [ "982061cf7e519a8b"; "6922792d0ea555fb"; "704b3e0cab677587"; "bee4cc1e2b076a60" ]
            ("all rules", 1), [ "9d8f1fd1c9356dec"; "ddc4c7f87a4e811d"; "e887f5bf5c5a76de"; "b86efe849962f016" ]
            ("all rules", 2), [ "f252197b6585143d"; "f0d0f2260bc57dd9"; "3a0c9292eca50d0e"; "c3b189c914b2bcf3" ]
            ("copying", 1), [ "f92246e2c215799f"; "ad3fa1b237d86f56"; "c3927add7c280ca2"; "1633185ae99d66fc" ]
            ("copying", 2), [ "c08c578fc49996db"; "aace4ed31022884e"; "65e0577c7f093238"; "e7d41d1fb3f53b18" ]
            ("stacking", 1), [ "094b014f8bc984fd"; "5b2ca542c931b0ba"; "41d446eb7e5783fd"; "bffe4eed5950af4f" ]
            ("stacking", 2), [ "9043b8c20e432895"; "bc2fd2cd5ad61c0c"; "76933f8909c1d1c8"; "bbe6a98491608e45" ]
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
