namespace Ecos.Test

open System
open FsCheck.FSharp
open FsCheck.Xunit
open Xunit
open Ecos
open Ecos.Test.Geometry

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

    /// Creates an atom of the given type. (Atom.create is shadowed by
    /// the single-type helper in Chain.fs.)
    let private typedAtom typ loc : Atom =
        { Type = typ; Location = loc }

    /// Are the given atoms strongly bonded?
    let private isStronglyBonded (world : World) idA idB =
        (Bonds.strongBondMap world)
            |> Map.tryFind idA
            |> Option.exists (Set.contains idB)

    /// All bonds in the given world, strong and weak, as
    /// (smaller ID, larger ID) pairs.
    let private allBonds (world : World) =
        [
            for KeyValue(idA, ids) in (Bonds.strongBondMap world) do
                for idB in ids do
                    if idA < idB then idA, idB
            for KeyValue(idA, idB) in (Bonds.weakBondMap world) do
                if idA < idB then idA, idB
        ]

    /// Oracle: would a bond between the given atoms cross any bond in
    /// the given world? Bonds that share an atom with it can't cross
    /// it. This checks every bond in the world, so it doesn't depend on
    /// World's assumption that only nearby bonds need checking.
    let private crossesAnyBond (world : World) idA idB =
        let loc id = world.AtomMap[id].Location
        allBonds world
            |> List.exists (fun (idC, idD) ->
                idC <> idA && idC <> idB && idD <> idA && idD <> idB
                    && segmentsIntersect (loc idA) (loc idB) (loc idC) (loc idD))

    /// Checks all of a world's invariants.
    let private isValid (world : World) =
        let atoms = world.AtomMap |> Map.toArray

            // no two atoms overlap, no atom overlaps a wall, and no two
            // walls overlap (a wall occupies the same square as an atom)
        let noOverlaps =
            atoms
                |> Array.forall (fun (idA, a) ->
                    atoms
                        |> Array.forall (fun (idB, b) ->
                            idA = idB || not (overlaps a.Location b.Location))
                        && world.Walls
                            |> Set.forall (fun wall -> not (overlaps a.Location wall)))
                && world.Walls
                    |> Set.forall (fun wallA ->
                        world.Walls
                            |> Set.forall (fun wallB ->
                                wallA = wallB || not (overlaps wallA wallB)))

            // every strong bond is allowed and symmetric, and no atom
            // has more than two
        let strongBondsValid =
            (Bonds.strongBondMap world)
                |> Map.forall (fun idA ids ->
                    ids.Count <= 2
                        && ids
                            |> Set.forall (fun idB ->
                                isBondVector
                                    (world.AtomMap[idB].Location - world.AtomMap[idA].Location)
                                    && (Bonds.strongBondMap world).[idB].Contains idA))

            // no atom is listed with an empty set of bonds, and no atom
            // has more than one weak bond
        let bondSetsValid =
            world.BondTypeMap.Values
                |> Seq.forall (Map.forall (fun _ ids -> not ids.IsEmpty))
                && Bonds.weakBondSets world
                    |> Map.forall (fun _ ids -> ids.Count <= 1)

            // no bond, strong or weak, crosses another
        let noCrossings =
            allBonds world
                |> List.forall (fun (idA, idB) ->
                    not (crossesAnyBond world idA idB))

            // every weak bond is symmetric, in range, between
            // complementary types, between atoms that aren't also
            // strongly bonded, and doesn't cross any other bond
        let weakBondsValid =
            (Bonds.weakBondMap world)
                |> Map.forall (fun idA idB ->
                    let atomA = world.AtomMap[idA]
                    let atomB = world.AtomMap[idB]
                    Bonds.weakPartner world idB = Some idA
                        && Array.contains
                            (atomB.Location - atomA.Location)
                            Chain.weakBondVectors
                        && Chain.canPair atomA.Type atomB.Type
                        && not (isStronglyBonded world idA idB)
                        && not (crossesAnyBond world idA idB))

            // the occupancy map is exactly the atoms' locations
        let occupancyConsistent =
            world.OccupancyMap.Count = world.AtomMap.Count
                && atoms
                    |> Array.forall (fun (id, atom) ->
                        world.OccupancyMap.TryFind atom.Location = Some id)

        noOverlaps && strongBondsValid && bondSetsValid && noCrossings && weakBondsValid && occupancyConsistent

    /// Unit steps an atom can attempt.
    let private directions =
        [ 1, 0; 0, -1; -1, 0; 0, 1 ]
            |> List.map Point.create

    /// Oracle: all valid moves in the given world, as (atom ID, step)
    /// pairs. A move is valid if the moved atom overlaps no other atom
    /// and all of its bonds, strong and weak, remain allowed.
    let private validMoves (world : World) =
        [
            for KeyValue(id, atom) in world.AtomMap do
                for delta in directions do
                    let loc = atom.Location + delta
                    let clear =
                        world.AtomMap
                            |> Map.forall (fun idB atomB ->
                                idB = id || not (overlaps loc atomB.Location))
                            && world.Walls
                                |> Set.forall (fun wall -> not (overlaps loc wall))
                    let strongBondsOk =
                        (Bonds.strongBondMap world)
                            |> Map.tryFind id
                            |> Option.defaultValue Set.empty
                            |> Set.forall (fun idB ->
                                isBondVector (world.AtomMap[idB].Location - loc))
                    let weakBondOk =
                        (Bonds.weakBondMap world)
                            |> Map.tryFind id
                            |> Option.forall (fun idB ->
                                Array.contains
                                    (world.AtomMap[idB].Location - loc)
                                    Chain.weakBondVectors)
                    if clear && strongBondsOk && weakBondOk then
                        id, delta
        ]

    /// Weak bonds in the given world, as (smaller ID, larger ID) pairs.
    let private weakBonds (world : World) =
        (Bonds.weakBondMap world)
            |> Map.toSeq
            |> Seq.filter (fun (idA, idB) -> idA < idB)
            |> set

    /// Oracle: can the given atoms form a new weak bond? They must be
    /// complementary, in range, both unpaired, and not strongly
    /// bonded, and the new bond must not cross any existing bond.
    let private canFormWeakBond (world : World) idA idB =
        let atomA = world.AtomMap[idA]
        let atomB = world.AtomMap[idB]
        Chain.canPair atomA.Type atomB.Type
            && Array.contains (atomB.Location - atomA.Location) Chain.weakBondVectors
            && not ((Bonds.weakBondSets world).ContainsKey idA)
            && not ((Bonds.weakBondSets world).ContainsKey idB)
            && not (isStronglyBonded world idA idB)
            && not (crossesAnyBond world idA idB)

    /// Oracle: number of stacks that a weak bond between the given
    /// atoms takes part in (or would, if formed): weak bonds A'-B'
    /// other than A-B itself, where A' is strongly bonded to A, and B'
    /// to B. Found by scanning every weak bond in the world, rather
    /// than by walking out from A as World does.
    let private countStacks (world : World) idA idB =
        weakBonds world
            |> Seq.filter (fun (idC, idD) ->
                set [ idC; idD ] <> set [ idA; idB ]
                    && (isStronglyBonded world idA idC && isStronglyBonded world idB idD
                        || isStronglyBonded world idA idD && isStronglyBonded world idB idC))
            |> Seq.length

    /// Strong bonds in the given world, as (smaller ID, larger ID)
    /// pairs.
    let private strongBonds (world : World) =
        set [
            for KeyValue(idA, ids) in Bonds.strongBondMap world do
                for idB in ids do
                    if idA < idB then idA, idB
        ]

    /// Number of strong bonds the given atom has.
    let private strongBondCount (world : World) id =
        Bonds.strongBondMap world
            |> Map.tryFind id
            |> Option.map Set.count
            |> Option.defaultValue 0

    /// Oracle: can the given atoms form a new strong bond? They must
    /// be in range, each have fewer than two strong bonds, and not
    /// already be bonded (strongly or weakly), and the new bond must
    /// not cross any existing bond. Rings, including triangles, are
    /// allowed.
    let private canFormStrongBond (world : World) idA idB =
        let atomA = world.AtomMap[idA]
        let atomB = world.AtomMap[idB]
        isBondVector (atomB.Location - atomA.Location)
            && strongBondCount world idA < 2
            && strongBondCount world idB < 2
            && not (isStronglyBonded world idA idB)
            && Bonds.weakPartner world idA <> Some idB
            && not (crossesAnyBond world idA idB)

    /// Oracle: number of stacks (0 or 1) that exist only because of a
    /// strong bond between the given atoms (or would, if it formed).
    /// If A and B have weak partners A' and B', those two weak bonds
    /// are stacked when their ends are strongly bonded on both sides:
    /// A to B and A' to B', or A to B' and A' to B. So bond A-B makes
    /// the difference when A' and B' are strongly bonded, unless the
    /// weak bonds are already stacked the other way.
    let private countStacksStrong (world : World) idA idB =
        match Bonds.weakPartner world idA, Bonds.weakPartner world idB with
            | Some idA', Some idB' when idA' <> idB ->
                let stackedOtherWay =
                    isStronglyBonded world idA idB'
                        && isStronglyBonded world idA' idB
                if isStronglyBonded world idA' idB' && not stackedOtherWay then 1
                else 0
            | _ -> 0

    /// Is the given bond vector longer than any weak bond?
    let private isLong (v : Point) =
        not (Array.contains v Chain.weakBondVectors)

    /// Do the given bonds include one longer than any weak bond?
    let private isLongBond (world : World) (bonds : Set<AtomId * AtomId>) =
        bonds
            |> Set.exists (fun (idA, idB) ->
                isLong (world.AtomMap[idB].Location - world.AtomMap[idA].Location))

    /// Oracle: probabilities that a toggle of each bond type forms a
    /// bond, and that it breaks one, in the given world, as (weak
    /// form, weak break, strong form, strong break, long strong form,
    /// long strong break). The last two count only strong bonds longer
    /// than any weak bond, and are included in the two before them.
    ///
    /// A toggle picks an atom uniformly, then one of the bond type's
    /// vectors uniformly. If that finds an atom the first can bond
    /// with, the bond forms (which always lowers the energy); if it
    /// finds an atom already bonded to the first, the bond breaks with
    /// probability exp(E / kT), where E is the energy of the bond and
    /// of the stacks that depend on it. For a strong bond, either event
    /// must also cross the barrier, with probability
    /// exp(-barrier / kT).
    let private toggleProbabilities kT (world : World) =
        let nAtoms = float world.AtomMap.Count
        let mutable weakForm, weakBreak = 0.0, 0.0
        let mutable strongForm, strongBreak = 0.0, 0.0
        let mutable longForm, longBreak = 0.0, 0.0
        let barrierFactor = exp (-Chain.strongBondBarrier / kT)
        for KeyValue(idA, atomA) in world.AtomMap do

            for v in Chain.weakBondVectors do
                match world.OccupancyMap.TryFind (atomA.Location + v) with
                    | Some idB when Bonds.weakPartner world idA = Some idB ->
                        let energy =
                            Chain.weakBondEnergy
                                + Chain.stackEnergy * float (countStacks world idA idB)
                        weakBreak <- weakBreak + exp (energy / kT)
                    | Some idB when canFormWeakBond world idA idB ->
                        weakForm <- weakForm + 1.0
                    | _ -> ()

            for v in Chain.bondVectors do
                match world.OccupancyMap.TryFind (atomA.Location + v) with
                    | Some idB when isStronglyBonded world idA idB ->
                        let energy =
                            Chain.strongBondEnergy
                                + Chain.stackEnergy * float (countStacksStrong world idA idB)
                        let p = barrierFactor * exp (energy / kT)
                        strongBreak <- strongBreak + p
                        if isLong v then longBreak <- longBreak + p
                    | Some idB when canFormStrongBond world idA idB ->
                        strongForm <- strongForm + barrierFactor
                        if isLong v then longForm <- longForm + barrierFactor
                    | _ -> ()

        let nWeak = nAtoms * float Chain.weakBondVectors.Length
        let nStrong = nAtoms * float Chain.bondVectors.Length
        weakForm / nWeak, weakBreak / nWeak,
            strongForm / nStrong, strongBreak / nStrong,
            longForm / nStrong, longBreak / nStrong

    /// All pairs of bonds that intersect, excluding pairs that share
    /// an atom.
    let private crossings (world : World) =
        let bonds = allBonds world
        let loc id = world.AtomMap[id].Location
        set [
            for i, (a, b) in List.indexed bonds do
                for c, d in List.skip (i + 1) bonds do
                    if a <> c && a <> d && b <> c && b <> d
                        && segmentsIntersect (loc a) (loc b) (loc c) (loc d) then
                        (a, b), (c, d)
        ]

    /// Adding atoms and walls one by one: each is accepted if and only
    /// if it doesn't overlap any atom or wall already present.
    [<Property>]
    let ``tryAddAtom and tryAddWall reject exactly the overlapping locations`` (items : list<bool * int * int>) =
        (Some World.empty, items)
            ||> List.fold (fun worldOpt (isWall, x, y) ->
                worldOpt
                    |> Option.bind (fun world ->
                        let loc = Point.create (x % 6, y % 6)   // crowd things together
                        let expected =
                            world.AtomMap
                                |> Map.forall (fun _ atom -> not (overlaps loc atom.Location))
                                && world.Walls
                                    |> Set.forall (fun wall -> not (overlaps loc wall))
                        let actual =
                            if isWall then World.tryAddWall loc world
                            else World.tryAddAtom (Atom.create loc) world
                        match actual, expected with
                            | Some world', true -> Some world'
                            | None, false -> Some world
                            | _ -> None))   // mismatch
            |> Option.exists isValid

    /// A strong bond between two non-overlapping atoms is accepted if
    /// and only if their separation is an allowed bond vector.
    [<Property>]
    let ``tryAddStrongBond accepts exactly the allowed bond vectors`` (x : int) (y : int) =
        let v = Point.create (x % 5, y % 5)
        let origin = Point.create (0, 0)
        not (overlaps origin v) ==> lazy (
            let world =
                World.empty
                    |> World.addAtom (Atom.create origin)
                    |> World.addAtom (Atom.create v)
            (World.tryAddStrongBond 0 1 world).IsSome = isBondVector v)

    /// A weak bond between two non-overlapping atoms is accepted if and
    /// only if their types are complementary and their separation is an
    /// allowed weak bond vector. Checked for every pair of types and
    /// every nearby separation.
    [<Fact>]
    let ``tryAddWeakBond accepts exactly complementary atoms in range`` () =
        let types = [ AtomType.W; AtomType.X; AtomType.Y; AtomType.Z ]
        let origin = Point.create (0, 0)
        for typeA in types do
            for typeB in types do
                for x in -4 .. 4 do
                    for y in -4 .. 4 do
                        let v = Point.create (x, y)
                        if not (overlaps origin v) then
                            let world =
                                World.empty
                                    |> World.addAtom (typedAtom typeA origin)
                                    |> World.addAtom (typedAtom typeB v)
                            let expected =
                                Chain.canPair typeA typeB
                                    && Array.contains v Chain.weakBondVectors
                            Assert.True(
                                ((World.tryAddWeakBond 0 1 world).IsSome = expected),
                                $"{typeA}-{typeB} at {v}: expected {expected}")

    /// Valence and exclusivity: an atom can have at most two strong
    /// bonds and one weak bond, and two atoms can't be bonded both
    /// strongly and weakly.
    [<Fact>]
    let ``Bond limits are enforced`` () =

            // W1 in the middle, with W0 and W2 on either side, and
            // X3 and X4 above and below it, all in range of W1
        let world =
            World.empty
                |> World.addAtom (typedAtom AtomType.W (Point.create (-2, 0)))   // 0
                |> World.addAtom (typedAtom AtomType.W (Point.create (0, 0)))    // 1
                |> World.addAtom (typedAtom AtomType.W (Point.create (2, 0)))    // 2
                |> World.addAtom (typedAtom AtomType.X (Point.create (0, 2)))    // 3
                |> World.addAtom (typedAtom AtomType.X (Point.create (0, -2)))   // 4

            // strong bonds: a third one is rejected
        let world =
            world
                |> World.addStrongBond 0 1
                |> World.addStrongBond 1 2
        Assert.True((World.tryAddStrongBond 1 3 world).IsNone, "Third strong bond accepted")

            // a strongly bonded pair can't also pair weakly
        Assert.True(
            (World.tryAddWeakBond 0 3 (World.addStrongBond 0 3 world)).IsNone,
            "Weak bond accepted between strongly bonded atoms")

            // weak bonds: a second one is rejected, from either side
        let world = World.addWeakBond 1 3 world
        Assert.True((World.tryAddWeakBond 1 4 world).IsNone, "Second weak bond accepted")
        Assert.True((World.tryAddWeakBond 4 1 world).IsNone, "Second weak bond accepted")

            // a weakly bonded pair can't also bond strongly, even with
            // spare strong valence (W0 has one strong bond, X4 none),
            // and the same weak bond can't be added twice
        let world = World.addWeakBond 0 4 world
        Assert.True((World.tryAddStrongBond 4 0 world).IsNone, "Strong bond accepted between weakly bonded atoms")
        Assert.True((World.tryAddWeakBond 4 0 world).IsNone, "Duplicate weak bond accepted")

    /// Topology, exhaustively: atoms A (type W, at the origin) and B
    /// (type X) are separated by one of the given bond vectors, and
    /// atoms C (type Y) and D (type Z) are bonded to each other,
    /// strongly or weakly. For every such local configuration, the
    /// given function, which tries to add a bond A-B, must succeed if
    /// and only if A-B doesn't cross bond C-D. C ranges over every
    /// location within the given distance of A, which must be far
    /// enough for bond C-D to reach bond A-B.
    let private checkCrossingRejected bondVectors range tryAddBond =
        let a = Point.create (0, 0)
        let mutable nChecked, nCrossed = 0, 0
        for bondAB in bondVectors do
            let b = a + bondAB
            for cx in -range .. range do
                for cy in -range .. range do
                    let c = Point.create (cx, cy)
                    for bondCD in Chain.bondVectors do
                        let d = c + bondCD
                        let atomsOpt =
                            Some World.empty
                                |> Option.bind (World.tryAddAtom (typedAtom AtomType.W a))
                                |> Option.bind (World.tryAddAtom (typedAtom AtomType.X b))
                                |> Option.bind (World.tryAddAtom (typedAtom AtomType.Y c))
                                |> Option.bind (World.tryAddAtom (typedAtom AtomType.Z d))
                        let worlds =
                            [
                                atomsOpt |> Option.bind (World.tryAddStrongBond 2 3)
                                atomsOpt |> Option.bind (World.tryAddWeakBond 2 3)   // only if C-D is in weak bond range
                            ] |> List.choose id
                        for world in worlds do
                            nChecked <- nChecked + 1
                            let crosses = segmentsIntersect a b c d
                            if crosses then nCrossed <- nCrossed + 1
                            if (tryAddBond 0 1 world : Option<World>).IsSome = crosses then
                                Assert.Fail(
                                    $"A {a}, B {b}, C {c}, D {d}: crosses = {crosses}, but accepted = {not crosses}")

            // make sure the check isn't vacuous
        Assert.True(nChecked > 10000, $"Only {nChecked} configurations checked")
        Assert.True(nCrossed > 0, "No crossed configurations checked")

    /// A weak bond is accepted if and only if it doesn't cross another
    /// bond.
    [<Fact>]
    let ``tryAddWeakBond rejects exactly the bonds that would cross`` () =
        checkCrossingRejected Chain.weakBondVectors 6 World.tryAddWeakBond

    /// A strong bond is accepted if and only if it doesn't cross
    /// another bond.
    [<Fact>]
    let ``tryAddStrongBond rejects exactly the bonds that would cross`` () =
        checkCrossingRejected Chain.bondVectors 8 World.tryAddStrongBond

    /// Moves made by a single update, as (atom ID, step) pairs.
    let private movesBetween (world : World) (world' : World) =
        Map.toList world.AtomMap
            |> List.choose (fun (id, atom) ->
                let d = world'.AtomMap[id].Location - atom.Location
                if d = Point.create (0, 0) then None else Some (id, d))

    /// Temperatures for tests with bonds: cold enough that weak bonds
    /// persist, and hot enough that strong bonds form and break.
    let private temperatures = [| 0.3; 1.0; 3.0 |]

    /// Temperature at which strong bonds form and break often.
    let private hotKT = 20.0

    /// Worlds for these tests are a chain plus a "soup" of free atoms
    /// and dimers packed around it, since an isolated chain rarely
    /// brings atoms that can bond into range.
    ///
    /// Each update makes at most one event, which must be valid:
    /// * a valid move that leaves all bonds unchanged, or
    /// * the breaking of one existing bond, or
    /// * the forming of one allowed bond (strong or weak).
    /// The world must stay valid throughout.
    [<Property>]
    let ``update makes a single valid event`` (length : int) (seed : int) (iKT : int) (nSteps : int) =
        let length = 2 + abs (length % 20)
        let kTs = Array.append temperatures [| hotKT |]
        let kT = kTs[abs (iKT % kTs.Length)]
        let rng = Random(seed)
        let mutable world = Chain.grow rng length |> Chain.withRandomTypes rng |> Chain.addSoup rng 10 |> Chain.addDimers rng 5 |> Chain.addObstacles rng 10
        let mutable ok = isValid world
        for _ in 1 .. 500 + abs (nSteps % 1000) do
            let world' = World.update kT rng world
            let weakBroken = weakBonds world - weakBonds world'
            let weakFormed = weakBonds world' - weakBonds world
            let strongBroken = strongBonds world - strongBonds world'
            let strongFormed = strongBonds world' - strongBonds world
            let nBondChanges =
                weakBroken.Count + weakFormed.Count
                    + strongBroken.Count + strongFormed.Count
            let eventOk =
                match movesBetween world world' with
                    | [] ->
                        if nBondChanges = 0 then true
                        elif nBondChanges > 1 then false
                        elif weakFormed.Count = 1 then
                            let idA, idB = Seq.exactlyOne weakFormed
                            canFormWeakBond world idA idB
                        elif strongFormed.Count = 1 then
                            let idA, idB = Seq.exactlyOne strongFormed
                            canFormStrongBond world idA idB
                        else true   // any existing bond may break
                    | [ move ] ->
                        List.contains move (validMoves world) && nBondChanges = 0
                    | _ -> false
            ok <- ok && eventOk && isValid world'
            world <- world'
        ok

    /// Runs the given number of updates on the given world, and checks
    /// that each kind of event (move, or a bond of either type formed
    /// or broken) happens as often as expected.
    ///
    /// An update chooses between a hop and a toggle of each bond type
    /// with World's fixed rates. A hop picks an atom and a direction
    /// uniformly and accepts any valid move (no hop changes the energy
    /// yet). For toggles, see toggleProbabilities. Over many updates,
    /// the number of events of each kind must match the sum of its
    /// probabilities.
    let private checkEventRates label kT nSteps (rng : Random) (world : World) =
        let mutable world = world

            // expected and actual counts of each kind of event
        let kinds =
            [|
                "moves"
                "weak bonds formed"; "weak bonds broken"
                "strong bonds formed"; "strong bonds broken"
                "long strong bonds formed"; "long strong bonds broken"
            |]
        let expected = Array.zeroCreate<float> kinds.Length
        let variance = Array.zeroCreate<float> kinds.Length
        let actual = Array.zeroCreate<int> kinds.Length

        for _ in 1 .. nSteps do
            let weakForm, weakBreak, strongForm, strongBreak, longForm, longBreak =
                toggleProbabilities kT world
            let ps =
                [|
                    World.moveAtomRate
                        * float (validMoves world).Length / float (4 * world.AtomMap.Count)
                    World.toggleWeakBondRate * weakForm
                    World.toggleWeakBondRate * weakBreak
                    World.toggleStrongBondRate * strongForm
                    World.toggleStrongBondRate * strongBreak
                    World.toggleStrongBondRate * longForm
                    World.toggleStrongBondRate * longBreak
                |]
            for i, p in Array.indexed ps do
                expected[i] <- expected[i] + p
                variance[i] <- variance[i] + p * (1.0 - p)
            let world' = World.update kT rng world
            let nWeak, nWeak' = (weakBonds world).Count, (weakBonds world').Count
            let nStrong, nStrong' = (strongBonds world).Count, (strongBonds world').Count
            if world'.AtomMap <> world.AtomMap then actual[0] <- actual[0] + 1
            if nWeak' > nWeak then actual[1] <- actual[1] + 1
            if nWeak' < nWeak then actual[2] <- actual[2] + 1
            if nStrong' > nStrong then actual[3] <- actual[3] + 1
            if nStrong' < nStrong then actual[4] <- actual[4] + 1
            if isLongBond world' (strongBonds world' - strongBonds world) then actual[5] <- actual[5] + 1
            if isLongBond world (strongBonds world - strongBonds world') then actual[6] <- actual[6] + 1
            world <- world'

        for i, kind in Array.indexed kinds do
            let tolerance = 5.0 * sqrt variance[i] + 0.5   // 5 standard deviations
            Assert.True(
                abs (float actual[i] - expected[i]) <= tolerance,
                $"{label}, kT = {kT}: {actual[i]} {kind}, expected {expected[i]:F1} ± {tolerance:F1}")

    /// Valid events are actually made, at the right rates, in crowded
    /// worlds of chains, free atoms, and walls. This catches an update
    /// that wrongly rejects valid events, or accepts them at the wrong
    /// rate, which the test above cannot.
    [<Fact>]
    let ``update makes valid events at the expected rate`` () =
        for kT in temperatures do
            for length in [ 2; 20 ] do
                let rng = Random(length)
                Chain.grow rng length
                    |> Chain.withRandomTypes rng
                    |> Chain.addSoup rng 10
                    |> Chain.addObstacles rng 10
                    |> checkEventRates $"N = {length}" kT 20000 rng

    /// Stacking, at the right rates: four atoms are confined to a small
    /// box, so that weak bonds between them form and break constantly.
    ///
    /// * Ladder: W0-W1 and X2-X3 are both strongly bonded. When both
    ///   rungs (0-2 and 1-3, or 0-3 and 1-2) exist, they form a stack,
    ///   so each is harder to break.
    /// * Near miss: W0-W1 is strongly bonded, but X2 and X3 are free.
    ///   The same two rungs can exist, but they never form a stack,
    ///   since X2 and X3 aren't strongly bonded.
    ///
    /// The oracle counts stacks independently of World, so bonds must
    /// break at the rates it predicts in both cases.
    [<Fact>]
    let ``Stacked weak bonds break at the expected rate`` () =
        let atoms =
            World.empty
                |> World.addAtom (typedAtom AtomType.W (Point.create (-1, -1)))   // 0
                |> World.addAtom (typedAtom AtomType.W (Point.create (1, -1)))    // 1
                |> World.addAtom (typedAtom AtomType.X (Point.create (-1, 1)))    // 2
                |> World.addAtom (typedAtom AtomType.X (Point.create (1, 1)))     // 3
                |> World.addStrongBond 0 1
                |> Chain.addBox 2
        let worlds =
            [
                "Ladder", World.addStrongBond 2 3 atoms
                "Near miss", atoms
            ]
        for label, world in worlds do
            let kT = 0.7   // cold enough that a stack clearly changes how often bonds break
            checkEventRates label kT 100000 (Random(0)) world

    /// Strong bonds, at the right rates: eight atoms of random types
    /// are sealed in a box, at a temperature high enough that strong
    /// bonds form and break constantly. Chains, branches of weak bonds,
    /// and rings (including triangles) all come and go, so this covers
    /// the barrier, the strong bond vectors, valence, crossing, and
    /// breaking one bond of an atom that has two.
    [<Fact>]
    let ``Strong bonds form and break at the expected rate`` () =
        let size = 4   // atom locations have |x| <= size and |y| <= size
        let rng = Random(0)
        let types = [| AtomType.W; AtomType.X; AtomType.Y; AtomType.Z |]
        let rec addAtoms count world =
            if count = 0 then world
            else
                let atom =
                    typedAtom
                        types[rng.Next types.Length]
                        (Point.create (rng.Next(-size, size + 1), rng.Next(-size, size + 1)))
                match World.tryAddAtom atom world with
                    | Some world -> addAtoms (count - 1) world
                    | None -> addAtoms count world
        World.empty
            |> Chain.addBox size
            |> addAtoms 8
            |> checkEventRates "Box" hotKT 100000 rng

    /// Topology, exhaustively: atom A (at the origin) is bonded to B
    /// and steps to A'; C and D are bonded to each other. For every
    /// such local configuration that is valid both before and after
    /// the step, bond AB must intersect bond CD before the step if and
    /// only if A'B intersects CD after it.
    ///
    /// This is a statement about the model's rules, so validity is
    /// decided by the oracles (no overlapping atoms, allowed bond
    /// vectors), which other tests check World against. World itself
    /// can't be used here, since it refuses to build a configuration
    /// with crossed bonds, and those must be covered too: a step must
    /// not be able to uncross bonds either. C ranges over every
    /// location from which bond CD could reach the region swept by the
    /// moving bond.
    [<Fact>]
    let ``No single move can carry one bond across another`` () =

            // are bonds A-B and C-D a valid configuration, crossed or not?
        let isValidConfig (a : Point) b c d =
            isBondVector (b - a)
                && isBondVector (d - c)
                && not (overlaps a c) && not (overlaps a d)
                && not (overlaps b c) && not (overlaps b d)

        let a = Point.create (0, 0)
        let mutable nChecked, nCrossed = 0, 0
        for bondAB in Chain.bondVectors do
            let b = a + bondAB
            for cx in -7 .. 7 do
                for cy in -7 .. 7 do
                    let c = Point.create (cx, cy)
                    for bondCD in Chain.bondVectors do
                        let d = c + bondCD
                        if isValidConfig a b c d then
                            let before = segmentsIntersect a b c d
                            for delta in directions do
                                let a' = a + delta
                                if isValidConfig a' b c d then
                                    nChecked <- nChecked + 1
                                    if before then nCrossed <- nCrossed + 1
                                    if segmentsIntersect a' b c d <> before then
                                        Assert.Fail(
                                            $"A {a} -> {a'}, B {b}, C {c}, D {d}: crossing changed")

            // make sure the check isn't vacuous
        Assert.True(nChecked > 100000, $"Only {nChecked} configurations checked")
        Assert.True(nCrossed > 0, "No crossed configurations checked")

    /// Topology: bonds never cross. A world can't be built with crossed
    /// bonds, a single step can never carry one bond across another,
    /// and a bond can't form across another bond, so no pair of bonds
    /// (strong or weak) may ever intersect.
    [<Property>]
    let ``Bonds never cross`` (length : int) (seed : int) (nSteps : int) =
        let length = 4 + abs (length % 20)
        let rng = Random(seed)
        let mutable world = Chain.grow rng length |> Chain.withRandomTypes rng |> Chain.addSoup rng 10 |> Chain.addDimers rng 5 |> Chain.addObstacles rng 10
        let mutable ok = (crossings world).IsEmpty
        for _ in 1 .. 500 + abs (nSteps % 500) do
            world <- World.update 1.0 rng world
            ok <- ok && (crossings world).IsEmpty
        ok
