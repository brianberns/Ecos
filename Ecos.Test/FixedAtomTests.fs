namespace Ecos.Test

open System
open Xunit
open Ecos

/// Fixed atoms: atoms that never move. Their strong bonds never change,
/// and no mobile atom can join them, but mobile atoms pair with them
/// as with any other atom.
module FixedAtomTests =

    /// A fixed strand W0->W1->X2->X3 along y = 12, with a fixed X4 just
    /// above W0, which W0 could pair with if both weren't fixed, and
    /// mobile W and X atoms around them.
    let private createWorld parms (rng : Random) =
        let add tryAdd atomType loc world =
            match tryAdd atomType loc world with
                | Some (world, _) -> world
                | None -> failwith "No room"
        let world =
            Ecos.World.createSquare 24
                |> add Ecos.World.tryAddFixedAtom AtomType.W (Point.create (8, 12))    // 0
                |> add Ecos.World.tryAddFixedAtom AtomType.W (Point.create (10, 12))   // 1
                |> add Ecos.World.tryAddFixedAtom AtomType.X (Point.create (12, 12))   // 2
                |> add Ecos.World.tryAddFixedAtom AtomType.X (Point.create (14, 12))   // 3
                |> add Ecos.World.tryAddFixedAtom AtomType.X (Point.create (8, 14))    // 4
        let world =
            (world, [ 0, 1; 1, 2; 2, 3 ])
                ||> List.fold (fun world (idPred, idSucc) ->
                    Ecos.World.tryAddStrongBond parms idPred idSucc world
                        |> Option.get)
        let rec addMobile n (world : World) =
            if n = 0 then world
            else
                let atomType = if n % 2 = 0 then AtomType.W else AtomType.X
                let loc = Point.create (2 * rng.Next(12), 2 * rng.Next(12))
                match Ecos.World.tryAddAtom atomType loc world with
                    | Some (world, _) -> addMobile (n - 1) world
                    | None -> addMobile n world
        addMobile 16 world

    let private numFixed = 5

    [<Fact>]
    let ``Atoms are mobile unless added as fixed`` () =
        let world = createWorld (TestParameters.create 1.0) (Random(0))
        for atom in world.Atoms do
            Assert.Equal(atom.Id >= numFixed, atom.IsMobile)

    /// At a temperature where bonds of both types form and break
    /// constantly among the mobile atoms.
    [<Theory>]
    [<InlineData(20.0)>]
    [<InlineData(3.0)>]
    let ``Fixed atoms never move, join or part, but do pair with mobile atoms`` (kT : float) =
        let parms = TestParameters.create kT
        let rng = Random(1)
        let start = createWorld parms rng
        let isFixed id = id < numFixed
        let mutable nFixedPairings = 0
        let mutable nMobileStrong = 0
        let onEvent event _ =
            match event with
                | MoveAtom id ->
                    Assert.False(isFixed id, $"Fixed atom {id} moved")
                | CreateBond (Strong, idA, idB) | BreakBond (Strong, idA, idB) ->
                    Assert.False(
                        isFixed idA || isFixed idB,
                        $"Strong bond {idA}-{idB} changed at a fixed atom")
                    nMobileStrong <- nMobileStrong + 1
                | CreateBond (Weak, idA, idB) | BreakBond (Weak, idA, idB) ->
                    Assert.False(
                        isFixed idA && isFixed idB,
                        $"Weak bond {idA}-{idB} changed between fixed atoms")
                    if isFixed idA || isFixed idB then
                        nFixedPairings <- nFixedPairings + 1
        let world = Ecos.World.sweepWith parms rng 20000 onEvent start

            // the fixed atoms are where they were, bonded as they were
        for id in 0 .. numFixed - 1 do
            Assert.Equal(start.Atoms[id], world.Atoms[id])
            Assert.Equal<Set<AtomId>>(
                BondMap.getBonds id start.BondTypeMap[Strong],
                BondMap.getBonds id world.BondTypeMap[Strong])

            // the test had something to stop, and pairing went on
        Assert.True(nFixedPairings > 0, "No mobile atom paired with a fixed one")
        Assert.True(nMobileStrong > 0, "No strong bond changed among the mobile atoms")
