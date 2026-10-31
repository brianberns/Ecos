namespace Ecos.Test

open System
open FsCheck.FSharp
open FsCheck.Xunit
open Xunit
open Ecos
open Ecos.Test.Geometry
open Ecos.Test.Validity

/// Oracles for the property tests below: independent statements of
/// which events an update may make, and how likely each is, against
/// which World is checked. (Each module of tests below runs in parallel
/// with the others.)
module PropertyOracles =

    /// Unit steps an atom can attempt.
    let directions =
        [ 1, 0; 0, -1; -1, 0; 0, 1 ]
            |> List.map Point.create

    /// Oracle: all valid moves in the given world, as (atom ID, step)
    /// pairs. A move is valid if the moved atom overlaps no other atom
    /// or wall, all of its bonds (strong and weak) remain in range,
    /// every bend it's part of remains within the strong bend limit in
    /// the given parameters, both at the moved atom and at each of its
    /// strong partners, and each atom of every weak bond remains on the
    /// other's face. (A move turns the faces of the moved atom and of
    /// its strong partners.) Moves are valid whatever they cost in bend
    /// energy; see runEventRates.
    let validMoves parms (world : World) =
        let disp loc id =
            Grid.getDisplacement loc world.Atoms[id].Location world.Cells
        [
            for atom in world.Atoms do
                let id = atom.Id
                for delta in directions do
                    let newLoc = atom.Location + delta
                    let loc idB =
                        if idB = id then newLoc
                        else world.Atoms[idB].Location
                    let clear =
                        world.Atoms
                            |> Seq.forall (fun atomB ->
                                atomB.Id = id || not (overlaps world newLoc atomB.Location))
                            && world.Walls
                                |> Seq.forall (fun wall -> not (overlaps world newLoc wall))
                    let strongBondsOk =
                        (Bonds.strongBondMap world)
                            |> BondMap.getBonds id
                            |> Set.forall (fun idB ->
                                isBondVector (disp newLoc idB))
                    let weakPartners = Bonds.weakPartners world id
                    let weakBondsOk =
                        weakPartners
                            |> Set.forall (fun idB ->
                                Array.contains (disp newLoc idB) Chain.weakBondVectors)
                    let anglesOk =
                        anglesAreAllowed parms world loc id
                            && partners Strong world id
                                |> Set.forall (anglesAreAllowed parms world loc)
                    let facesOk =
                        facesAreValid world (withLocation id newLoc (strandsOf world))
                    if clear && strongBondsOk && weakBondsOk && anglesOk && facesOk then
                        id, delta
        ]

    /// Oracle: can the given atoms form a new weak bond? They must be
    /// complementary, in range, and not already bonded (weakly or
    /// strongly); neither may have a weak partner yet; each must be on
    /// the other's face; and the new bond must not cross any existing
    /// bond.
    let canFormWeakBond (world : World) idA idB =
        let atomA = world.Atoms[idA]
        let atomB = world.Atoms[idB]
        let strands = strandsOf world
        Chain.canPair atomA.Type atomB.Type
            && Array.contains (delta idA idB world) Chain.weakBondVectors
            && not (Bonds.areWeaklyBonded world idA idB)
            && not (isStronglyBonded world idA idB)
            && (Bonds.weakPartners world idA).IsEmpty
            && (Bonds.weakPartners world idB).IsEmpty
            && isOnFace world strands idA idB
            && isOnFace world strands idB idA
            && not (crossesAnyBond world idA idB)

    /// Oracle: are the two given weak bonds stacked? They are if their
    /// ends are strongly bonded on both sides, either way round.
    let areStacked (world : World) (idA, idB) (idC, idD) =
        (isStronglyBonded world idA idC && isStronglyBonded world idB idD)
            || (isStronglyBonded world idA idD && isStronglyBonded world idB idC)

    /// Oracle: number of stacks that a weak bond between the given
    /// atoms takes part in (or would, if formed): the other weak bonds
    /// that it's stacked with. Found by scanning every weak bond in
    /// the world, rather than by walking out from A as World does.
    let countStacks (world : World) idA idB =
        weakBonds world
            |> Seq.filter (fun (idC, idD) ->
                set [ idC; idD ] <> set [ idA; idB ]
                    && areStacked world (idA, idB) (idC, idD))
            |> Seq.length

    /// Number of strong bonds the given atom has.
    let strongBondCount (world : World) id =
        Bonds.strongBondMap world
            |> BondMap.getBonds id
            |> Set.count

    /// Oracle: can a new strong bond form from atom A to atom B, so
    /// that B follows A in their strand? They must be in range, not
    /// both be caps, each have fewer strong bonds than its type permits
    /// (one for a cap, otherwise two), and not already be bonded
    /// (strongly or weakly). A must have no successor yet, and B no
    /// predecessor, so that the strand doesn't branch. The new bond
    /// must not cross any existing bond or bend either atom's strand
    /// too sharply, and since it turns the faces of A and B, it must
    /// leave each atom of every weak bond on the other's face. Rings
    /// are allowed, if they're big enough to be that straight.
    let canFormStrongBond parms (world : World) idA idB =
        let atomA = world.Atoms[idA]
        let atomB = world.Atoms[idB]
        let hasPredecessor id =
            Bonds.directedStrongBonds world
                |> Set.exists (fun (_, idSucc) -> idSucc = id)
        isBondVector (delta idA idB world)
            && Chain.canJoin atomA.Type atomB.Type
            && strongBondCount world idA < Chain.strongValence atomA.Type
            && strongBondCount world idB < Chain.strongValence atomB.Type
            && not (isStronglyBonded world idA idB)
            && not (Bonds.areWeaklyBonded world idA idB)
            && (Bonds.successor world idA).IsNone
            && not (hasPredecessor idB)
            && hasRoomAtAngle parms world idA idB
            && hasRoomAtAngle parms world idB idA
            && not (crossesAnyBond world idA idB)
            && facesAreValid world (withBond idA idB (strandsOf world))

    /// Oracle: number of stacks that exist only because of a strong
    /// bond between the given atoms (or would, if it formed). Each
    /// pair of a weak bond of A with a weak bond of B is stacked if
    /// their ends are strongly bonded on both sides. Bond A-B is one of
    /// those sides, so it makes the difference when the two partners
    /// are strongly bonded to each other, unless the pair is already
    /// stacked the other way round. Each atom has at most one weak
    /// partner, so at most one stack can depend on a strong bond.
    let countStacksStrong (world : World) idA idB =
        List.sum [
            for idPartnerA in Bonds.weakPartners world idA do
                for idPartnerB in Bonds.weakPartners world idB do
                    let distinct =
                        idPartnerA <> idB
                            && idPartnerB <> idA
                            && idPartnerA <> idPartnerB
                    let stackedOtherWay =
                        isStronglyBonded world idA idPartnerB
                            && isStronglyBonded world idPartnerA idB
                    if distinct
                        && isStronglyBonded world idPartnerA idPartnerB
                        && not stackedOtherWay then 1
        ]

    /// Oracle: can the strong bond between the given atoms break?
    /// Breaking it turns the faces of both atoms, so only if that
    /// leaves each atom of every weak bond on the other's face.
    let canBreakStrongBond (world : World) idA idB =
        facesAreValid world (withoutBond idA idB (strandsOf world))

    /// Oracle: is the strong bond between the given atoms at an exposed
    /// end of a strand? It is if either atom has no other strong bond
    /// and isn't a cap. Such a bond can peel.
    let isExposed (world : World) idA idB =
        [ idA; idB ]
            |> List.exists (fun id ->
                not (Chain.isCap world.Atoms[id].Type)
                    && (partners Strong world id).Count = 1)

    /// Is the given bond vector longer than any weak bond?
    let isLong (v : Point) =
        not (Array.contains v Chain.weakBondVectors)

    /// Do the given bonds include one longer than any weak bond?
    let isLongBond (world : World) (bonds : Set<AtomId * AtomId>) =
        bonds
            |> Set.exists (fun (idA, idB) ->
                isLong (delta idA idB world))

    /// Do the given strong bonds include one that a stack depends on?
    let isStackedBond (world : World) (bonds : Set<AtomId * AtomId>) =
        bonds
            |> Set.exists (fun (idA, idB) ->
                countStacksStrong world idA idB > 0)

    /// Oracle: probabilities that a toggle of each bond type forms a
    /// bond, and that it breaks one, in the given world, as (weak
    /// form, weak break, strong form, strong break, long strong form,
    /// long strong break, stacked strong break). The long ones count
    /// only strong bonds longer than any weak bond, and the stacked one
    /// counts only strong bonds that a stack depends on. All three are
    /// included in the strong form and strong break probabilities.
    ///
    /// A toggle picks an atom uniformly, then one of the bond type's
    /// vectors uniformly. If that finds an atom the first can bond
    /// with, it proposes forming the bond; if it finds an atom already
    /// bonded to the first, it proposes breaking the bond. A strong
    /// bond it forms runs from the atom it picked to the one it found,
    /// so a bond that can run only one way is proposed from only one of
    /// its atoms, while it can be broken from either. Either
    /// proposal must first cross the bond type's barrier, with
    /// probability exp(-barrier / kT), and is then accepted with the
    /// Metropolis probability for its energy change. That change is
    /// the energy of the bond plus that of the stacks that depend on
    /// it, gained on forming and lost on breaking, less kT times the
    /// bond type's entropy, which is lost on forming and gained on
    /// breaking.
    ///
    /// A strong bond at an exposed end of a strand has a second way to
    /// break: it peels, with probability exp(-peel barrier / kT),
    /// whatever its energy; and any strong bond has a third: it
    /// hydrolyzes, with the given fixed probability. Weak bonds never
    /// hydrolyze.
    /// Nothing reverses these, so they take the
    /// model out of equilibrium.
    let toggleProbabilities (parms : Parameters) (world : World) =
            // each bond has its own temperature (see Chain.bondTemperature)
        let nAtoms = float world.Atoms.Length
        let mutable weakForm, weakBreak = 0.0, 0.0
        let mutable strongForm, strongBreak = 0.0, 0.0
        let mutable longForm, longBreak = 0.0, 0.0
        let mutable stackedBreak = 0.0

            // probability of forming, or of breaking, a bond of the given
            // type and energy that the given number of stacks depend on.
            // Forming changes the energy by the bond's (negative) energy and
            // loses the bond type's entropy; breaking does the reverse.
        let acceptWith kT bondType nStacks isStart energyDelta entropyDelta =
            exp (-Chain.bondBarrier parms bondType nStacks isStart / kT)
                * min 1.0 (exp (entropyDelta - energyDelta / kT))
        let acceptForm kT bondType nStacks isStart energy =
            acceptWith kT bondType nStacks isStart energy -(Chain.bondEntropy parms bondType)
        let acceptBreak kT bondType nStacks isStart energy =
            acceptWith kT bondType nStacks isStart -energy (Chain.bondEntropy parms bondType)

            // does a bond of the given type between the given atoms start a
            // strand? It does if neither atom has another bond of that type.
        let isStart bondType idA idB =
            (partners bondType world idA).Remove(idB).IsEmpty
                && (partners bondType world idB).Remove(idA).IsEmpty

        for atomA in world.Atoms do
            let idA = atomA.Id

            for v in Chain.weakBondVectors do
                match world[atomA.Location + v] with
                    | Cell.Atom idB ->
                        let kT = Chain.bondTemperature parms atomA world.Atoms[idB]
                        let nStacks = countStacks world idA idB
                        let energy =
                            Chain.bondEnergy parms Weak atomA.Type world.Atoms[idB].Type
                                + Chain.stackEnergy * float nStacks
                        if Bonds.areWeaklyBonded world idA idB then
                            let pBreak = acceptBreak kT Weak nStacks (isStart Weak idA idB) energy
                            let pHydrolysis = Chain.hydrolysisRate parms Weak world idA idB
                            weakBreak <- weakBreak + 1.0 - (1.0 - pBreak) * (1.0 - pHydrolysis)
                        elif canFormWeakBond world idA idB then
                            weakForm <- weakForm + acceptForm kT Weak nStacks (isStart Weak idA idB) energy
                    | _ -> ()

            for v in Chain.bondVectors do
                match world[atomA.Location + v] with
                    | Cell.Atom idB ->
                        let kT = Chain.bondTemperature parms atomA world.Atoms[idB]
                        let nStacks = countStacksStrong world idA idB
                        let energy =
                            Chain.bondEnergy parms Strong atomA.Type world.Atoms[idB].Type
                                + Chain.stackEnergy * float nStacks
                        if isStronglyBonded world idA idB then

                                // the bond breaks if the usual test passes or,
                                // failing that, if it peels
                            let p =
                                if canBreakStrongBond world idA idB then
                                    let pBreak = acceptBreak kT Strong nStacks (isStart Strong idA idB) energy
                                    let pPeel =
                                        if isExposed world idA idB then
                                            exp (-Chain.peelBarrier parms Strong / kT)
                                        else 0.0
                                    let pHydrolysis = Chain.hydrolysisRate parms Strong world idA idB
                                    1.0 - (1.0 - pBreak) * (1.0 - pPeel) * (1.0 - pHydrolysis)
                                else 0.0
                            strongBreak <- strongBreak + p
                            if isLong v then longBreak <- longBreak + p
                            if nStacks > 0 then stackedBreak <- stackedBreak + p
                        elif canFormStrongBond parms world idA idB then
                            let p = acceptForm kT Strong nStacks (isStart Strong idA idB) energy
                            strongForm <- strongForm + p
                            if isLong v then longForm <- longForm + p
                    | _ -> ()

        let nWeak = nAtoms * float Chain.weakBondVectors.Length
        let nStrong = nAtoms * float Chain.bondVectors.Length
        weakForm / nWeak, weakBreak / nWeak,
            strongForm / nStrong, strongBreak / nStrong,
            longForm / nStrong, longBreak / nStrong,
            stackedBreak / nStrong

    /// Moves made by a single update, as (atom ID, step) pairs.
    let movesBetween (world : World) (world' : World) =
        Seq.toList world.Atoms
            |> List.choose (fun atom ->
                let d =
                    Grid.getDisplacement
                        atom.Location world'.Atoms[atom.Id].Location world'.Cells
                if d = Point.create (0, 0) then None else Some (atom.Id, d))

    /// Temperatures for tests with bonds: cold enough that weak bonds
    /// persist, and hot enough that strong bonds form and break.
    let temperatures = [| 0.3; 1.0; 3.0 |]

    /// Temperature at which strong bonds form and break often.
    let hotKT = 20.0

    /// Runs the given number of updates on the given world, and checks
    /// that each kind of event (move, or a bond of either type formed
    /// or broken) happens as often as expected.
    ///
    /// An update chooses between a hop and a toggle of each bond type
    /// with World's fixed rates. A hop picks an atom and a direction
    /// uniformly and accepts any valid move, so the parameters must not
    /// make bending cost energy: the tests of bend energy compare whole
    /// distributions instead (see PhysicsTests). For toggles, see
    /// toggleProbabilities. Over many updates, the number of events of
    /// each kind must match the sum of its probabilities.
    let runEventRates label (parms : Parameters) nSteps (rng : Random) (world : World) =
        for bondType in BondType.all do
            Assert.Equal(0.0, parms.BondTypeParameterMap[bondType].BendStiffness)
        let mutable world = world

            // expected and actual counts of each kind of event
        let kinds =
            [|
                "moves"
                "weak bonds formed"; "weak bonds broken"
                "strong bonds formed"; "strong bonds broken"
                "long strong bonds formed"; "long strong bonds broken"
                "stacked strong bonds broken"
            |]
        let expected = Array.zeroCreate<float> kinds.Length
        let variance = Array.zeroCreate<float> kinds.Length
        let actual = Array.zeroCreate<int> kinds.Length

        /// Probability of each kind of event in the given world.
        let getProbabilities world =
            let weakForm, weakBreak, strongForm, strongBreak, longForm, longBreak, stackedBreak =
                toggleProbabilities parms world
            [|
                World.moveAtomRate
                    * float (validMoves parms world).Length / float (4 * world.Atoms.Length)
                World.toggleWeakBondRate * weakForm
                World.toggleWeakBondRate * weakBreak
                World.toggleStrongBondRate * strongForm
                World.toggleStrongBondRate * strongBreak
                World.toggleStrongBondRate * longForm
                World.toggleStrongBondRate * longBreak
                World.toggleStrongBondRate * stackedBreak
            |]

            // the probabilities depend only on the world, so they're
            // computed again only when an update changes it (most don't)
        let mutable ps = getProbabilities world
        for _ in 1 .. nSteps do
            for i, p in Array.indexed ps do
                expected[i] <- expected[i] + p
                variance[i] <- variance[i] + p * (1.0 - p)
            match World.tryUpdate parms rng world with
                | Some (world', _) ->
                    let nWeak, nWeak' = (weakBonds world).Count, (weakBonds world').Count
                    let nStrong, nStrong' = (strongBonds world).Count, (strongBonds world').Count
                    let moved =
                        Seq.exists2 (fun (atom : Atom) (atom' : Atom) ->
                            atom.Location <> atom'.Location)
                            world.Atoms world'.Atoms
                    if moved then actual[0] <- actual[0] + 1
                    if nWeak' > nWeak then actual[1] <- actual[1] + 1
                    if nWeak' < nWeak then actual[2] <- actual[2] + 1
                    if nStrong' > nStrong then actual[3] <- actual[3] + 1
                    if nStrong' < nStrong then actual[4] <- actual[4] + 1
                    if isLongBond world' (strongBonds world' - strongBonds world) then actual[5] <- actual[5] + 1
                    if isLongBond world (strongBonds world - strongBonds world') then actual[6] <- actual[6] + 1
                    if isStackedBond world (strongBonds world - strongBonds world') then actual[7] <- actual[7] + 1
                    world <- world'
                    ps <- getProbabilities world
                | None -> ()   // nothing happened

        for i, kind in Array.indexed kinds do
            let tolerance = 5.0 * sqrt variance[i] + 0.5   // 5 standard deviations
            Assert.True(
                abs (float actual[i] - expected[i]) <= tolerance,
                $"{label}: {actual[i]} {kind}, expected {expected[i]:F1} ± {tolerance:F1}")
        world

    /// As runEventRates, but answers nothing.
    let checkEventRates label parms nSteps rng world =
        runEventRates label parms nSteps rng world |> ignore

    /// Runs the given independent checks at the same time, to keep the
    /// tests quick. Each check needs its own random number generator.
    let inParallel (checks : (unit -> unit)[]) =
        try
            Array.Parallel.iter (fun check -> check ()) checks
        with :? AggregateException as exn ->
            raise (exn.Flatten().InnerExceptions[0])   // report the first failure itself

    /// Worlds for testing stacking: four atoms confined to a small box,
    /// so that bonds between them form and break constantly.
    ///
    /// * Ladder: W0->W1 and X3->X2 are both strongly bonded, running
    ///   opposite ways, so that they face each other as built. When
    ///   both rungs (0-2 and 1-3) exist, they form a stack.
    /// * Near miss: W0->W1 is strongly bonded, but X2 and X3 are free.
    ///   The same two rungs can exist, but they don't form a stack,
    ///   since X2 and X3 aren't strongly bonded.
    let stackingWorlds =
        let atoms =
            TestWorld.empty
                |> World.addAtom AtomType.W (Point.create (-1, -1))   // 0
                |> World.addAtom AtomType.W (Point.create (1, -1))    // 1
                |> World.addAtom AtomType.X (Point.create (-1, 1))    // 2
                |> World.addAtom AtomType.X (Point.create (1, 1))     // 3
                |> World.addStrongBond TestParameters.standard 0 1
                |> Chain.addBox 2
        [
            "Ladder", World.addStrongBond TestParameters.standard 3 2 atoms
            "Near miss", atoms
        ]

    /// A world for testing faces: a strand W0->W1->W2, paired at its
    /// middle atom with a free X3 on its face, in a box so small that
    /// bonds form and break constantly.
    let pairedStrandWorld =
        TestWorld.empty
            |> World.addAtom AtomType.W (Point.create (-2, 0))   // 0
            |> World.addAtom AtomType.W (Point.create (0, 0))    // 1
            |> World.addAtom AtomType.W (Point.create (2, 0))    // 2
            |> World.addAtom AtomType.X (Point.create (0, 2))    // 3
            |> World.addStrongBond TestParameters.standard 0 1
            |> World.addStrongBond TestParameters.standard 1 2
            |> World.addWeakBond TestParameters.standard 1 3
            |> Chain.addBox 2

    /// Parameters for the paired strand: strong bonds have no energy or
    /// barrier, so they form and break at every opportunity, running
    /// either way, and they bend up to right angles, which turns faces
    /// more than a nearly straight strand does. Weak bonds are strong,
    /// so the X is usually paired.
    let pairedStrandParameters =
        TestParameters.basic 1.0
            |> TestParameters.withBend Strong TestParameters.rightAngleBend 0.0
            |> TestParameters.withWeak (fun p -> { p with Energy = 3 })

/// Building a world: each way of adding an atom, wall or bond is
/// accepted exactly when the rules allow it, checked exhaustively or
/// against random cases.
module ConstructionTests =

    open PropertyOracles

    /// Points have hand-written equality, ordering and hashing (for
    /// speed). They must behave exactly like their coordinates do:
    /// equal when both coordinates are, ordered by X and then Y, and
    /// with equal hashes when equal.
    [<Fact>]
    let ``Points compare like their coordinates`` () =
        let points =
            [
                for x in -3 .. 3 do
                    for y in -3 .. 3 do
                        Point.create (x, y)
            ]
        for a in points do
            for b in points do
                let expectedEqual = (a.X, a.Y) = (b.X, b.Y)
                let expectedOrder = compare (a.X, a.Y) (b.X, b.Y)
                Assert.True(((a = b) = expectedEqual), $"{a} = {b}")
                Assert.True((a.Equals(box b) = expectedEqual), $"{a} equals {b}")
                Assert.True((sign (compare a b) = sign expectedOrder), $"{a} vs. {b}")
                Assert.True(
                    (sign ((a :> IComparable).CompareTo(box b)) = sign expectedOrder),
                    $"{a} vs. {b}")
                if expectedEqual then
                    Assert.True((hash a = hash b), $"hash of {a}")

            // distinct points mostly have distinct hashes
        let nHashes = points |> List.map hash |> List.distinct |> List.length
        Assert.True(nHashes > points.Length * 9 / 10, $"Only {nHashes} distinct hashes")

            // sets and maps of points, which rely on the above
        Assert.Equal(points.Length, (set (points @ points)).Count)
        Assert.Equal<Point list>(
            points |> List.sortBy (fun p -> p.X, p.Y),
            Set.toList (set (List.rev points)))

    /// Adding atoms and walls one by one: each is accepted if and only
    /// if it doesn't overlap any atom or wall already present.
    [<Property>]
    let ``tryAddAtom and tryAddWall reject exactly the overlapping locations`` (items : list<bool * int * int>) =
        (Some TestWorld.empty, items)
            ||> List.fold (fun worldOpt (isWall, x, y) ->
                worldOpt
                    |> Option.bind (fun world ->
                        let loc = Point.create (x % 6, y % 6)   // crowd things together
                        let expected =
                            world.Atoms
                                |> Seq.forall (fun atom -> not (overlaps TestWorld.empty loc atom.Location))
                                && world.Walls
                                    |> Seq.forall (fun wall -> not (overlaps TestWorld.empty loc wall))
                        let actual =
                            if isWall then World.tryAddWall loc world
                            else World.tryAddAtomW loc world
                        match actual, expected with
                            | Some world', true -> Some world'
                            | None, false -> Some world
                            | _ -> None))   // mismatch
            |> Option.exists (isValid TestParameters.standard)

    /// A strong bond between two non-overlapping atoms is accepted if
    /// and only if their separation is an allowed bond vector.
    [<Property>]
    let ``tryAddStrongBond accepts exactly the allowed bond vectors`` (x : int) (y : int) =
        let v = Point.create (x % 5, y % 5)
        let origin = Point.create (0, 0)
        not (overlaps TestWorld.empty origin v) ==> lazy (
            let world =
                TestWorld.empty
                    |> World.addAtomW origin
                    |> World.addAtomW v
            (World.tryAddStrongBond TestParameters.standard 0 1 world).IsSome = isBondVector v)

    /// A weak bond between two non-overlapping atoms is accepted if and
    /// only if their types are complementary and their separation is an
    /// allowed weak bond vector. Checked for every pair of types and
    /// every nearby separation.
    [<Fact>]
    let ``tryAddWeakBond accepts exactly complementary atoms in range`` () =
        let types = List.ofArray Chain.allTypes
        let origin = Point.create (0, 0)
        for typeA in types do
            for typeB in types do
                for x in -4 .. 4 do
                    for y in -4 .. 4 do
                        let v = Point.create (x, y)
                        if not (overlaps TestWorld.empty origin v) then
                            let world =
                                TestWorld.empty
                                    |> World.addAtom typeA origin
                                    |> World.addAtom typeB v
                            let expected =
                                Chain.canPair typeA typeB
                                    && Array.contains v Chain.weakBondVectors
                            Assert.True(
                                ((World.tryAddWeakBond TestParameters.standard 0 1 world).IsSome = expected),
                                $"{typeA}-{typeB} at {v}: expected {expected}")

    /// Valence and exclusivity: an atom can have at most two strong
    /// bonds and one weak bond, and two atoms can't be bonded both
    /// strongly and weakly.
    [<Fact>]
    let ``Bond limits are enforced`` () =

            // W0->W1->W2 in a row, with X3 above W1 and X4 above and to
            // the right of it, both on the strand's face and in range of
            // W1
        let world =
            TestWorld.empty
                |> World.addAtom AtomType.W (Point.create (-2, 0))   // 0
                |> World.addAtom AtomType.W (Point.create (0, 0))    // 1
                |> World.addAtom AtomType.W (Point.create (2, 0))    // 2
                |> World.addAtom AtomType.X (Point.create (0, 2))    // 3
                |> World.addAtom AtomType.X (Point.create (2, 2))    // 4
                |> World.addStrongBond TestParameters.standard 0 1
                |> World.addStrongBond TestParameters.standard 1 2

            // strong bonds: a third one is rejected, either way round
        Assert.True((World.tryAddStrongBond TestParameters.standard 1 3 world).IsNone, "Third strong bond accepted")
        Assert.True((World.tryAddStrongBond TestParameters.standard 3 1 world).IsNone, "Third strong bond accepted")

            // weak bonds: W1 can pair with either X, but not with both,
            // from either side
        Assert.True((World.tryAddWeakBond TestParameters.standard 1 4 world).IsSome, "Weak bond rejected")
        let world = World.addWeakBond TestParameters.standard 1 3 world
        Assert.True((World.tryAddWeakBond TestParameters.standard 1 4 world).IsNone, "Second weak bond accepted")
        Assert.True((World.tryAddWeakBond TestParameters.standard 4 1 world).IsNone, "Second weak bond accepted")

            // a strongly bonded pair can't also pair weakly, a weakly
            // bonded pair can't also bond strongly, either way round, and
            // the same weak bond can't be added twice
        let pair =
            TestWorld.empty
                |> World.addAtom AtomType.W (Point.create (0, 0))
                |> World.addAtom AtomType.X (Point.create (0, 2))
        Assert.True((World.tryAddWeakBond TestParameters.standard 0 1 pair).IsSome, "Weak bond rejected")
        Assert.True(
            (World.tryAddWeakBond TestParameters.standard 0 1 (World.addStrongBond TestParameters.standard 0 1 pair)).IsNone,
            "Weak bond accepted between strongly bonded atoms")
        let paired = World.addWeakBond TestParameters.standard 0 1 pair
        Assert.True((World.tryAddStrongBond TestParameters.standard 0 1 paired).IsNone, "Strong bond accepted between weakly bonded atoms")
        Assert.True((World.tryAddStrongBond TestParameters.standard 1 0 paired).IsNone, "Strong bond accepted between weakly bonded atoms")
        Assert.True((World.tryAddWeakBond TestParameters.standard 1 0 paired).IsNone, "Duplicate weak bond accepted")

            // weak bonds: a free atom (one with no strong bond) can't have a
            // second one either
        let world =
            TestWorld.empty
                |> World.addAtom AtomType.W (Point.create (0, 0))    // 0
                |> World.addAtom AtomType.X (Point.create (0, 2))    // 1
                |> World.addAtom AtomType.X (Point.create (0, -2))   // 2
                |> World.addWeakBond TestParameters.standard 0 1
        Assert.True((World.tryAddWeakBond TestParameters.standard 0 2 world).IsNone, "Free atom got a second weak bond")
        Assert.True((World.tryAddWeakBond TestParameters.standard 2 0 world).IsNone, "Free atom got a second weak bond")

    /// Caps, exhaustively: atoms A, B and C lie in a row, close enough
    /// for A-B and B-C to be strongly bonded. For every combination of
    /// types:
    /// * A-B is accepted, running either way, if and only if A and B
    ///   aren't both caps.
    /// * Then, with A-B running from A to B, B-C is accepted if and
    ///   only if B isn't a cap, since a cap can have only one strong
    ///   bond. C may be a cap: it's C's first bond, and B isn't a cap
    ///   if the bond is allowed at all. B-C is tried running from B to
    ///   C, the only way it can continue the strand (see the next
    ///   test).
    [<Fact>]
    let ``tryAddStrongBond gives a cap exactly one strong bond`` () =
        let mutable nAccepted, nRejected = 0, 0
        for typeA in Chain.allTypes do
            for typeB in Chain.allTypes do
                for typeC in Chain.allTypes do
                    let world =
                        TestWorld.empty
                            |> World.addAtom typeA (Point.create (-3, 0))   // 0
                            |> World.addAtom typeB (Point.create (0, 0))    // 1
                            |> World.addAtom typeC (Point.create (3, 0))    // 2

                        // first bond
                    let expected = not (Chain.isCap typeA && Chain.isCap typeB)
                    for idFrom, idTo in [ 0, 1; 1, 0 ] do
                        Assert.True(
                            ((World.tryAddStrongBond TestParameters.standard idFrom idTo world).IsSome = expected),
                            $"{typeA}-{typeB}: expected {expected}")

                        // second bond
                    if expected then
                        let world = World.addStrongBond TestParameters.standard 0 1 world
                        let expected = not (Chain.isCap typeB)
                        Assert.True(
                            ((World.tryAddStrongBond TestParameters.standard 1 2 world).IsSome = expected),
                            $"{typeA}-{typeB}, then {typeB}-{typeC}: expected {expected}")
                        if expected then nAccepted <- nAccepted + 1
                        else nRejected <- nRejected + 1

            // make sure the check isn't vacuous
        Assert.True(nAccepted > 0, "No accepted configurations checked")
        Assert.True(nRejected > 0, "No rejected configurations checked")

    /// Direction, exhaustively: two strands, A-B and C-D, lie in a row,
    /// each running either way, and a strong bond between B and C is
    /// tried, running either way. A bond from one atom to another is
    /// accepted if and only if the first has no successor and the
    /// second no predecessor, so that it joins the tail of one strand
    /// to the head of the other. Any other join would branch a strand.
    [<Fact>]
    let ``tryAddStrongBond joins strands only tail to head`` () =
        let mutable nAccepted, nRejected = 0, 0
        for abForward in [ true; false ] do
            for cdForward in [ true; false ] do
                let world =
                    TestWorld.empty
                        |> World.addAtomW (Point.create (-6, 0))   // A
                        |> World.addAtomW (Point.create (-3, 0))   // B
                        |> World.addAtomW (Point.create (0, 0))    // C
                        |> World.addAtomW (Point.create (3, 0))    // D
                let world =
                    if abForward then World.addStrongBond TestParameters.standard 0 1 world
                    else World.addStrongBond TestParameters.standard 1 0 world
                let world =
                    if cdForward then World.addStrongBond TestParameters.standard 2 3 world
                    else World.addStrongBond TestParameters.standard 3 2 world
                let describe forward first second =
                    if forward then $"{first}->{second}" else $"{second}->{first}"
                let strands = $"{describe abForward 'A' 'B'}, {describe cdForward 'C' 'D'}"

                    // B is a tail if A-B runs forward, and C is a head if
                    // C-D does
                let expectedBC = abForward && cdForward
                let expectedCB = not abForward && not cdForward
                for idFrom, idTo, expected in [ 1, 2, expectedBC; 2, 1, expectedCB ] do
                    match World.tryAddStrongBond TestParameters.standard idFrom idTo world with
                        | Some world' ->
                            Assert.True(expected, $"{strands}: {idFrom}->{idTo} accepted")
                            Assert.True(isValid TestParameters.standard world', $"{strands}: {idFrom}->{idTo} is invalid")
                            nAccepted <- nAccepted + 1
                        | None ->
                            Assert.False(expected, $"{strands}: {idFrom}->{idTo} rejected")
                            nRejected <- nRejected + 1

            // make sure the check isn't vacuous
        Assert.Equal(2, nAccepted)
        Assert.Equal(6, nRejected)

    /// Faces, exhaustively: atom A (type W, at the origin) is in a
    /// strand, as its head, its tail, or in its middle, and atom B
    /// (type X, free) is in weak bond range of it. A weak bond A-B must
    /// be accepted if and only if B is on A's face: to the left of the
    /// strand's direction at A (see Bonds.isOnFace). Checked for every
    /// placement of A's strand partners (type Y) with a bend up to a
    /// right angle, and of B, with the bond added from either end.
    [<Fact>]
    let ``tryAddWeakBond accepts a partner exactly on the face of an atom in a strand`` () =
        let parms = TestParameters.standard |> TestParameters.withBend Strong TestParameters.rightAngleBend 0.0
        let a = Point.create (0, 0)
        let free (locs : Point list) loc =
            locs |> List.forall (fun other -> not (overlaps TestWorld.empty loc other))

            // A's strand partners: their locations, and A's predecessor
            // and successor among them, as indexes
        let strands =
            [
                for n in Chain.bondVectors do
                    "head", [ n ], None, Some 0
                    "tail", [ n ], Some 0, None
                for p in Chain.bondVectors do
                    for s in Chain.bondVectors do
                        if free [ p ] s && Bonds.isBendAllowed TestParameters.rightAngleBend a p s then
                            "middle", [ p; s ], Some 0, Some 1
            ]
        let counts = Collections.Generic.Dictionary<string * bool, int>()
        for position, partnerLocs, iPredOpt, iSuccOpt in strands do
            for b in Chain.weakBondVectors do
                if free partnerLocs b then
                    let world =
                        (TestWorld.empty |> World.addAtom AtomType.W a |> World.addAtom AtomType.X b, partnerLocs)
                            ||> List.fold (fun world loc -> World.addAtom AtomType.Y loc world)
                    let world =
                        match iPredOpt with
                            | Some i -> World.addStrongBond parms (2 + i) 0 world
                            | None -> world
                    let world =
                        match iSuccOpt with
                            | Some i -> World.addStrongBond parms 0 (2 + i) world
                            | None -> world
                    let direction =
                        Bonds.strandDirection
                            (iPredOpt |> Option.map (fun i -> partnerLocs[i]))
                            a
                            (iSuccOpt |> Option.map (fun i -> partnerLocs[i]))
                    let expected = Bonds.isOnFace direction b
                    for idFrom, idTo in [ 0, 1; 1, 0 ] do
                        Assert.True(
                            ((World.tryAddWeakBond parms idFrom idTo world).IsSome = expected),
                            $"A at the {position} of a strand with partners at {partnerLocs}, B at {b}: expected {expected}")
                    counts[(position, expected)] <-
                        (match counts.TryGetValue((position, expected)) with | true, n -> n | _ -> 0) + 1

            // make sure the check isn't vacuous: in every position, some
            // partners are accepted and some rejected
        for position in [ "head"; "tail"; "middle" ] do
            for expected in [ true; false ] do
                Assert.True(counts.ContainsKey((position, expected)), $"No {position} cases with expected {expected}")

    /// Faces on both sides, exhaustively: atoms A (type W, at the
    /// origin) and B (type X) are each in a strand of two, with a
    /// partner of type Y. Each strand runs either way. A weak bond A-B
    /// must be accepted if and only if each of A and B is on the
    /// other's face, so that the two strands face each other. Checked
    /// for every placement of B in weak bond range of A, and of each
    /// strand partner in strong bond range of its atom, except where
    /// the two strong bonds would cross, with the bond added from
    /// either end.
    [<Fact>]
    let ``tryAddWeakBond pairs two strands exactly when they face each other`` () =
        let a = Point.create (0, 0)
        let mutable nAccepted, nRejected = 0, 0
        for b in Chain.weakBondVectors do
            for n in Chain.bondVectors do
                for mv in Chain.bondVectors do
                    let m = b + mv
                    let clear =
                        [ a, m; b, n; n, m ]
                            |> List.forall (fun (p, q) -> not (overlaps TestWorld.empty p q))
                    if clear && not (segmentsIntersect a n b m) then
                        for aFirst in [ true; false ] do
                            for bFirst in [ true; false ] do
                                let world =
                                    TestWorld.empty
                                        |> World.addAtom AtomType.W a   // 0
                                        |> World.addAtom AtomType.X b   // 1
                                        |> World.addAtom AtomType.Y n   // 2
                                        |> World.addAtom AtomType.Y m   // 3
                                let world =
                                    if aFirst then World.addStrongBond TestParameters.standard 0 2 world
                                    else World.addStrongBond TestParameters.standard 2 0 world
                                let world =
                                    if bFirst then World.addStrongBond TestParameters.standard 1 3 world
                                    else World.addStrongBond TestParameters.standard 3 1 world
                                let directionA = if aFirst then n - a else a - n
                                let directionB = if bFirst then m - b else b - m
                                let expected =
                                    Bonds.isOnFace directionA (b - a)
                                        && Bonds.isOnFace directionB (a - b)
                                for idFrom, idTo in [ 0, 1; 1, 0 ] do
                                    Assert.True(
                                        ((World.tryAddWeakBond TestParameters.standard idFrom idTo world).IsSome = expected),
                                        $"A's strand toward {directionA}, B at {b} with strand toward {directionB}: expected {expected}")
                                if expected then nAccepted <- nAccepted + 1
                                else nRejected <- nRejected + 1

            // make sure the check isn't vacuous
        Assert.True(nAccepted > 0, "No accepted configurations checked")
        Assert.True(nRejected > 0, "No rejected configurations checked")

    /// Faces turn as strands grow, exhaustively: atom A (type W, at the
    /// origin) has a weak partner, B (type X), and is either free or
    /// the tail of a strand from P (type Y) to A. A strong bond between
    /// A and C (type Y) is tried, running either way. It must be
    /// accepted if and only if it can run that way (A must have no
    /// predecessor to get one), it bends no more than a right angle at
    /// A, and it leaves B on A's face, which turns as the bond gives A
    /// a strand or extends it. Checked for every placement of B in weak
    /// bond range of A (on A's face, if A is in a strand), and of C in
    /// strong bond range.
    [<Fact>]
    let ``tryAddStrongBond is accepted exactly when it leaves a weak partner on its face`` () =
        let parms = TestParameters.standard |> TestParameters.withBend Strong TestParameters.rightAngleBend 0.0
        let a = Point.create (0, 0)
        let p = Point.create (-3, 0)
        let mutable nAccepted, nRejected = 0, 0
        for hasPred in [ false; true ] do
            let predOpt = if hasPred then Some p else None
            for b in Chain.weakBondVectors do
                for c in Chain.bondVectors do
                    let clear =
                        not (overlaps TestWorld.empty b c)
                            && (not hasPred || (not (overlaps TestWorld.empty p b) && not (overlaps TestWorld.empty p c)))
                    let onFaceBefore =
                        not hasPred || Bonds.isOnFace (Bonds.strandDirection predOpt a None) b
                    if clear && onFaceBefore then
                        let world =
                            TestWorld.empty
                                |> World.addAtom AtomType.W a   // 0
                                |> World.addAtom AtomType.X b   // 1
                                |> World.addAtom AtomType.Y c   // 2
                        let world =
                            if hasPred then
                                world
                                    |> World.addAtom AtomType.Y p   // 3
                                    |> World.addStrongBond parms 3 0
                            else world
                        let world = World.addWeakBond parms 0 1 world

                            // A to C continues the strand, if any; C to A
                            // starts one, if A has no predecessor yet
                        let expectedAC =
                            (not hasPred || Bonds.isBendAllowed TestParameters.rightAngleBend a p c)
                                && Bonds.isOnFace (Bonds.strandDirection predOpt a (Some c)) b
                        let expectedCA =
                            not hasPred
                                && Bonds.isOnFace (Bonds.strandDirection (Some c) a None) b
                        for idFrom, idTo, expected in [ 0, 2, expectedAC; 2, 0, expectedCA ] do
                            Assert.True(
                                ((World.tryAddStrongBond parms idFrom idTo world).IsSome = expected),
                                $"Predecessor at {predOpt}, B at {b}, C at {c}, {idFrom}->{idTo}: expected {expected}")
                            if expected then nAccepted <- nAccepted + 1
                            else nRejected <- nRejected + 1

            // make sure the check isn't vacuous
        Assert.True(nAccepted > 0, "No accepted configurations checked")
        Assert.True(nRejected > 0, "No rejected configurations checked")

    /// Stiffness, exhaustively: atom A (at the origin) is strongly
    /// bonded to B, from B to A. A second strong bond, from A to C (the
    /// only way it can continue the strand), must be accepted if and
    /// only if the angle between the two bonds at A is within the bend
    /// limit. Checked for every pair of locations in strong bond range
    /// of A, at the tests' usual limit (nearly straight) and the
    /// standard scenarios' (right angles).
    [<Fact>]
    let ``tryAddStrongBond accepts a second bond exactly when it bends no more than the limit`` () =
        let a = Point.create (0, 0)
        for limit in [ TestParameters.straightBend; TestParameters.rightAngleBend ] do
            let parms = TestParameters.standard |> TestParameters.withBend Strong limit 0.0
            let mutable nAccepted, nRejected = 0, 0
            for b in Chain.bondVectors do
                for c in Chain.bondVectors do
                    if not (overlaps TestWorld.empty b c) then
                        let world =
                            TestWorld.empty
                                |> World.addAtomW a   // 0
                                |> World.addAtomW b   // 1
                                |> World.addAtomW c   // 2
                                |> World.addStrongBond parms 1 0
                        let expected = Bonds.isBendAllowed limit a b c
                        Assert.True(
                            ((World.tryAddStrongBond parms 0 2 world).IsSome = expected),
                            $"Limit {limit.Numerator}/{limit.Denominator}, B at {b}, C at {c}: expected {expected}")
                        if expected then nAccepted <- nAccepted + 1
                        else nRejected <- nRejected + 1

                // make sure the check isn't vacuous
            Assert.True(nAccepted > 0, "No accepted configurations checked")
            Assert.True(nRejected > 0, "No rejected configurations checked")

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
                            Some TestWorld.empty
                                |> Option.bind (World.tryAddAtom AtomType.W a)
                                |> Option.map fst
                                |> Option.bind (World.tryAddAtom AtomType.X b)
                                |> Option.map fst
                                |> Option.bind (World.tryAddAtom AtomType.Y c)
                                |> Option.map fst
                                |> Option.bind (World.tryAddAtom AtomType.Z d)
                                |> Option.map fst
                        let worlds =
                            [
                                atomsOpt |> Option.bind (World.tryAddStrongBond TestParameters.standard 2 3)
                                atomsOpt |> Option.bind (World.tryAddWeakBond TestParameters.standard 2 3)   // only if C-D is in weak bond range
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
        checkCrossingRejected Chain.weakBondVectors 6 (World.tryAddWeakBond TestParameters.standard)

    /// A strong bond is accepted if and only if it doesn't cross
    /// another bond.
    [<Fact>]
    let ``tryAddStrongBond rejects exactly the bonds that would cross`` () =
        checkCrossingRejected Chain.bondVectors 8 (World.tryAddStrongBond TestParameters.standard)

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
                && not (overlaps TestWorld.empty a c) && not (overlaps TestWorld.empty a d)
                && not (overlaps TestWorld.empty b c) && not (overlaps TestWorld.empty b d)

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

/// Updating a world: every update makes at most one valid event, and
/// the world stays valid, in crowded worlds and in small ones built to
/// test particular rules.
module DynamicsTests =

    open PropertyOracles

    /// A capped strand is never extended. The strand P0-W1-W2-P3 sits
    /// among free atoms of every type. Strong bonds have no barrier,
    /// so the free atoms join each other, and the strand's inner
    /// atoms, at every opportunity; and they're too strong to break.
    /// After every update, the world must still be valid, which
    /// includes each cap having at most one strong bond, and at the
    /// end the strand must be exactly the same four atoms.
    [<Fact>]
    let ``A capped strand is never extended`` () =
        let rng = Random(0)
        let parms =
            TestParameters.basic 1.0
                |> TestParameters.withStrong (fun p -> { p with Energy = 30 })
                |> TestParameters.withWeak (fun p -> { p with Energy = 1 })
        let mutable world =
            TestWorld.empty
                |> World.addAtom AtomType.P (Point.create (-3, 0))   // 0
                |> World.addAtom AtomType.W (Point.create (-1, 0))   // 1
                |> World.addAtom AtomType.W (Point.create (1, 0))    // 2
                |> World.addAtom AtomType.P (Point.create (3, 0))    // 3
                |> World.addStrongBond TestParameters.standard 0 1
                |> World.addStrongBond TestParameters.standard 1 2
                |> World.addStrongBond TestParameters.standard 2 3
                |> Chain.addSoup rng 30
                |> Chain.addBox 10
        let strand = set [ 0; 1; 2; 3 ]
        let strandBonds world =
            strongBonds world
                |> Set.filter (fun (idA, idB) ->
                    strand.Contains idA || strand.Contains idB)
        let expected = set [ 0, 1; 1, 2; 2, 3 ]
        let mutable nJoins = 0
        for _ in 1 .. 50000 do
            match World.tryUpdate parms rng world with
                | Some (world', _) ->
                    nJoins <- nJoins + max 0 ((strongBonds world').Count - (strongBonds world).Count)
                    Assert.True(isValid parms world', "World became invalid")
                    world <- world'
                | None -> ()   // nothing happened
        Assert.Equal<Set<int * int>>(expected, strandBonds world)

            // make sure the check isn't vacuous: free atoms did join each
            // other, including some that started next to the strand
        Assert.True(nJoins > 5, $"Only {nJoins} strong bonds formed")

    /// Faces stay facing as atoms move: a hop turns the faces of the
    /// moved atom and of its strong partners, and must never turn one
    /// away from its weak partner. Here two dimers face each other, as
    /// in a ladder: W0->W1 below, and X3->X2 above, running the other
    /// way, with rungs 0-2 and 1-3, in a box that leaves them room to
    /// move. Strong bonds are frozen and it's cold, so that the rungs
    /// usually exist. After every update, the world must still be
    /// valid, which includes each atom of a rung being on the other's
    /// face.
    [<Fact>]
    let ``Weak partners stay on each other's faces`` () =
        let rng = Random(0)
        let parms = TestParameters.createFrozen 0.3
        let mutable world =
            TestWorld.empty
                |> World.addAtom AtomType.W (Point.create (-1, 0))   // 0
                |> World.addAtom AtomType.W (Point.create (1, 0))    // 1
                |> World.addAtom AtomType.X (Point.create (-1, 2))   // 2
                |> World.addAtom AtomType.X (Point.create (1, 2))    // 3
                |> World.addStrongBond TestParameters.standard 0 1
                |> World.addStrongBond TestParameters.standard 3 2
                |> World.addWeakBond TestParameters.standard 0 2
                |> World.addWeakBond TestParameters.standard 1 3
                |> Chain.addBox 4
        let mutable nPaired, nMoves = 0, 0
        for _ in 1 .. 20000 do
            let world' = World.updateWith parms rng world
            if not (weakBonds world').IsEmpty then
                nPaired <- nPaired + 1
                if not (movesBetween world world').IsEmpty then
                    nMoves <- nMoves + 1
            Assert.True(isValid parms world', "World became invalid")
            world <- world'

            // make sure the check isn't vacuous
        Assert.True(nPaired > 10000, $"Rungs existed in only {nPaired} updates")
        Assert.True(nMoves > 1000, $"Only {nMoves} moves made with rungs")

    /// Faces stay facing as strands form and break: a strong bond turns
    /// the faces of both of its atoms, when it forms and when it
    /// breaks, and must never turn one away from its weak partner.
    /// Here a strand of three W atoms is paired with a free X (see
    /// pairedStrandWorld and pairedStrandParameters). After every
    /// update, the world must still be valid.
    [<Fact>]
    let ``Weak partners stay on each other's faces as strong bonds form and break`` () =
        let rng = Random(0)
        let parms = pairedStrandParameters
        let mutable world = pairedStrandWorld
        let mutable nPaired, nFormed, nBroken = 0, 0, 0
        for _ in 1 .. 50000 do
            let world' = World.updateWith parms rng world
            if not (weakBonds world').IsEmpty then
                nPaired <- nPaired + 1
                let nStrong, nStrong' = (strongBonds world).Count, (strongBonds world').Count
                if nStrong' > nStrong then nFormed <- nFormed + 1
                if nStrong' < nStrong then nBroken <- nBroken + 1
            Assert.True(isValid parms world', "World became invalid")
            world <- world'

            // make sure the check isn't vacuous: strong bonds formed and
            // broke while the X was paired
        Assert.True(nPaired > 10000, $"The X was paired in only {nPaired} updates")
        Assert.True(nFormed > 20, $"Only {nFormed} strong bonds formed while paired")
        Assert.True(nBroken > 20, $"Only {nBroken} strong bonds broke while paired")

    /// Worlds for these tests are a chain plus a "soup" of free atoms
    /// and dimers packed around it, since an isolated chain rarely
    /// brings atoms that can bond into range.
    ///
    /// Each update makes at most one event, which must be valid:
    /// * a valid move that leaves all bonds unchanged, or
    /// * the breaking of one existing bond (if that's allowed), or
    /// * the forming of one allowed bond (strong or weak).
    /// The world must stay valid throughout. Strands are either nearly
    /// straight (the tests' usual parameters), or bend up to right
    /// angles at a cost (as in the standard scenarios).
    [<Property>]
    let ``update makes a single valid event`` (length : int) (seed : int) (iKT : int) (nSteps : int) (canFold : bool) =
        let length = 2 + abs (length % 20)
        let kTs = Array.append temperatures [| hotKT |]
        let kT = kTs[abs (iKT % kTs.Length)]
        let parms =
            let parms = TestParameters.create kT
            if canFold then parms |> TestParameters.withBend Strong TestParameters.rightAngleBend 1.5
            else parms
        let rng = Random(seed)
        let mutable world = Chain.grow rng length |> Chain.withRandomTypes rng |> Chain.addSoup rng 10 |> Chain.addDimers rng 5 |> Chain.addObstacles rng 10
        let mutable ok = isValid parms world
        for _ in 1 .. 300 + abs (nSteps % 500) do
            let world' = World.updateWith parms rng world
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
                            let idPred, idSucc =   // the way the new bond runs
                                Bonds.directedStrongBonds world' - Bonds.directedStrongBonds world
                                    |> Seq.exactlyOne
                            canFormStrongBond parms world idPred idSucc
                        elif strongBroken.Count = 1 then
                            let idA, idB = Seq.exactlyOne strongBroken
                            canBreakStrongBond world idA idB
                        else true   // any existing weak bond may break
                    | [ move ] ->
                        List.contains move (validMoves parms world) && nBondChanges = 0
                    | _ -> false
            ok <- ok && eventOk && isValid parms world'
            world <- world'
        ok

/// Event rates with stacks, bond entropy and the start barrier: each
/// kind of event happens as often as the oracle predicts.
module StackingRateTests =

    open PropertyOracles

    /// Stacking and weak bonds, at the right rates: with the standard
    /// parameters, strong bonds stay fixed, and a stack makes each of
    /// its weak bonds harder to break. The oracle counts stacks
    /// independently of World, so weak bonds must break at the rates
    /// it predicts in both worlds.
    [<Fact>]
    let ``Stacked weak bonds break at the expected rate`` () =
        for label, world in stackingWorlds do
            let kT = 0.7   // cold enough that a stack clearly changes how often bonds break
            checkEventRates label (TestParameters.create kT) 100000 (Random(0)) world

    /// Stacking and strong bonds, at the right rates: a stack also
    /// depends on the strong bonds along its sides, so it makes those
    /// harder to break too, and forming one of them can create a stack.
    /// With the standard parameters, a strong bond is so much stronger
    /// than a stack that the difference can't be measured. Here strong
    /// bonds have no energy or barrier of their own, so they form and
    /// break constantly, and only a stack holds one in place. Weak
    /// bonds are made stronger, so that the rungs of a stack persist
    /// while its sides come and go. Strong bonds that a stack depends
    /// on are counted separately from the rest.
    [<Fact>]
    let ``Stacked strong bonds break at the expected rate`` () =
        let parms =
            TestParameters.basic 1.0
                |> TestParameters.withWeak (fun p -> { p with Energy = 3 })
        for label, world in stackingWorlds do
            checkEventRates label parms 200000 (Random(0)) world

    /// A stack lowers the barrier, at the right rates: a strong bond
    /// that a stack depends on has a lower barrier, both to forming and
    /// to breaking. Here strong bonds have a barrier that keeps them
    /// from forming or breaking often, except where a stack removes it.
    /// As above, they have no energy of their own, and weak bonds are
    /// made stronger. Strong bonds that a stack depends on are counted
    /// separately from the rest, so both their forming and their
    /// breaking are checked against the lowered barrier.
    [<Fact>]
    let ``A stack lowers the barrier of its strong bonds`` () =
        let parms =
            TestParameters.basic 1.0
                |> TestParameters.withStrong (fun p -> { p with Barrier = 3; BarrierStackReduction = 3 })
                |> TestParameters.withWeak (fun p -> { p with Energy = 3 })
        for label, world in stackingWorlds do
            checkEventRates label parms 200000 (Random(1)) world

    /// Bond entropy, at the right rates: the stacking worlds again,
    /// with entropy on both bond types. Two cases:
    /// * Entropy smaller than a bond's energy over kT: bonds still
    ///   form freely, and break more easily than without entropy.
    /// * Entropy larger than that, even for a stacked bond: bonds
    ///   break freely, and it's forming one that is hard. Without
    ///   this case, an update that ignored entropy when forming a
    ///   bond would pass.
    [<Fact>]
    let ``Bonds with entropy form and break at the expected rate`` () =
        let parms weakEntropy strongEntropy =
            TestParameters.basic 1.0
                |> TestParameters.withStrong (fun p -> { p with Energy = 2; Entropy = strongEntropy })
                |> TestParameters.withWeak (fun p -> { p with Energy = 3; Entropy = weakEntropy })
        inParallel [|
            for iWorld, (label, world) in List.indexed stackingWorlds do
                for iCase, (weakEntropy, strongEntropy) in List.indexed [ 1.5, 1.0; 6.0, 5.0 ] do
                    fun () ->
                        checkEventRates
                            $"{label}, entropy {weakEntropy} and {strongEntropy}"
                            (parms weakEntropy strongEntropy)
                            200000
                            (Random(10 * iWorld + iCase))
                            world
        |]

    /// The start barrier, at the right rates. A bond that starts a
    /// strand (one between two atoms with no other bond of its type)
    /// has an extra barrier, to forming and to breaking alike.
    /// * In the hot box, bonds of both types start and end constantly.
    /// * In the same box when cooler, strands start slowly and then grow
    ///   freely. Adding an atom to a strand is not a start.
    /// * In the stacking worlds, a stack takes more off the barrier than
    ///   there is, which must leave zero, not less, for the start
    ///   barrier to be added to. Otherwise a stacked start would have no
    ///   barrier at all.
    [<Fact>]
    let ``Bonds that start a strand form and break at the expected rate`` () =
        let size = 4   // atom locations have |x| <= size and |y| <= size
        let rng = Random(3)
        let rec addAtoms count world =
            if count = 0 then world
            else
                let atomType = Chain.codingTypes[rng.Next Chain.codingTypes.Length]
                let loc = Point.create (rng.Next(-size, size + 1), rng.Next(-size, size + 1))
                match World.tryAddAtom atomType loc world with
                    | Some (world, _) -> addAtoms (count - 1) world
                    | None -> addAtoms count world
        let box =
            TestWorld.empty
                |> Chain.addBox size
                |> addAtoms 8
        let hot =
            TestParameters.create hotKT
                |> TestParameters.withStartBarrier hotKT (hotKT / 2.0)
        let stacked =
            TestParameters.basic 1.0
                |> TestParameters.withStrong (fun p -> { p with Barrier = 3; BarrierStackReduction = 5; StartBarrier = 1.5 })
                |> TestParameters.withWeak (fun p -> { p with Energy = 3; StartBarrier = 0.5 })
            // strands start slowly but then grow freely, so most strong bonds
            // formed extend a strand and must not get the start barrier
        let growing =
            TestParameters.basic 1.0
                |> TestParameters.withStrong (fun p -> { p with Energy = 2; StartBarrier = 2 })
                |> TestParameters.withWeak (fun p -> { p with Energy = 1 })
        inParallel [|
            fun () -> checkEventRates "Box, strands growing" growing 150000 (Random(9)) box
            fun () -> checkEventRates "Box, with start barriers" hot 150000 (Random(4)) box
            for i, (label, world) in List.indexed stackingWorlds do
                fun () ->
                    checkEventRates
                        $"{label}, with start barriers" stacked 200000 (Random(5 + i)) world
        |]

/// Event rates of strong bonds: forming and breaking, peeling, and
/// hydrolysis, with and without protection.
module BondRateTests =

    open PropertyOracles

    /// Strong bonds, at the right rates: eight atoms of random types
    /// are sealed in a box, at a temperature high enough that strong
    /// bonds form and break constantly. Chains, weak bonds, and rings
    /// (including triangles) all come and go, so this covers the
    /// barrier, the strong bond vectors, valence, direction, crossing,
    /// and breaking one bond of an atom that has two.
    ///
    /// Run once without peeling and once with it. With peeling, bonds
    /// re-form as fast as they peel, so there are enough peeling events
    /// to pin down the peeling probability. A third run has caps, which
    /// stop peeling, and hydrolysis, which breaks any strong bond (caps'
    /// included), and of weak bonds, at their own rate. A fourth makes
    /// weak bonds deep, so that their own hydrolysis rate shows.
    [<Fact>]
    let ``Strong bonds form and break at the expected rate`` () =
        let size = 4   // atom locations have |x| <= size and |y| <= size
        let rng = Random(0)

        let rec addAtoms (types : AtomType[]) count world =
            if count = 0 then world
            else
                let atomType = types[rng.Next types.Length]
                let loc =
                    Point.create (rng.Next(-size, size + 1), rng.Next(-size, size + 1))
                match World.tryAddAtom atomType loc world with
                    | Some (world, _) -> addAtoms types (count - 1) world
                    | None -> addAtoms types count world
        let createWorld types =
            TestWorld.empty
                |> Chain.addBox size
                |> addAtoms types 8
        let parms = TestParameters.create hotKT
        let world = createWorld Chain.allTypes
        let worldNoCaps = createWorld Chain.codingTypes

            // with peeling: strong bonds form freely and are too strong to
            // break in the usual way, so every break is a peel. No caps
            // here: with caps, the atoms soon settle into capped strands
            // that never peel.
        let peeling =
            TestParameters.basic 1.0
                |> TestParameters.withStrong (fun p -> { p with Energy = 30 })
                |> TestParameters.withWeak (fun p -> { p with Energy = 1 })
                |> TestParameters.withPeelBarrier 1.0   // peels with probability 1/e
        inParallel [|
            fun () -> checkEventRates "Box" parms 100000 (Random(1)) world
            fun () ->
                    // some pairs of types with energies of their own
                let withPairs =
                    parms
                        |> TestParameters.withPairEnergies Weak [ (AtomType.Y, AtomType.Z), 15.0 ]
                        |> TestParameters.withPairEnergies Strong
                            [
                                (AtomType.W, AtomType.Y), 4.0
                                (AtomType.X, AtomType.Z), 30.0
                            ]
                checkEventRates "Box, with pair energies" withPairs 100000 (Random(4)) worldNoCaps
            fun () ->
                checkEventRates
                    "Box, with peeling" peeling 200000 (Random(2)) worldNoCaps
            fun () ->
                    // caps too, and hydrolysis, which takes capped strands
                    // apart, and weak bonds hydrolyzing too, at their own rate
                checkEventRates
                    "Box, with caps and hydrolysis"
                    (peeling
                        |> TestParameters.withHydrolysis Strong 0.1
                        |> TestParameters.withHydrolysis Weak 0.05)
                    200000 (Random(3)) world
            fun () ->
                    // deep weak bonds, which seldom break by the usual test,
                    // so that how often they hydrolyze shows: each bond type
                    // at its own rate, far apart
                checkEventRates
                    "Box, with deep weak bonds and hydrolysis"
                    (peeling
                        |> TestParameters.withWeak (fun p -> { p with Energy = 3.0 })
                        |> TestParameters.withHydrolysis Strong 0.3
                        |> TestParameters.withHydrolysis Weak 0.05)
                    200000 (Random(8)) world
        |]

    /// Strong bonds and faces, at the right rates: a strand of three W
    /// atoms is paired with a free X (see pairedStrandWorld and
    /// pairedStrandParameters). Each strong bond that forms or breaks
    /// turns the faces of its atoms, so whether it may depends on where
    /// the X is, and so does each move, which turns the faces of the
    /// moved atom and of its strong partners. (See canFormStrongBond,
    /// canBreakStrongBond and validMoves.)
    [<Fact>]
    let ``Strong bonds form and break at the expected rate around a weak partner`` () =
        checkEventRates
            "Paired strand" pairedStrandParameters 200000 (Random(0)) pairedStrandWorld

    /// Peeling: with strong bonds otherwise frozen (an impassable
    /// barrier), the only way one can break is by peeling from an
    /// exposed end, and none can form.
    /// * In crowded worlds of uncapped chains and dimers, strong bonds
    ///   break at the expected rate until few are left. (See
    ///   toggleProbabilities.)
    /// * A capped strand, P0-W1-W2-P3, never loses a bond.
    /// * With hydrolysis, a capped strand is taken apart: a bond breaks,
    ///   and the pieces peel.
    /// * Hydrolysis breaks bonds without peeling too, anywhere in a
    ///   strand.
    /// * With peeling turned off, nothing breaks at all.
    [<Fact>]
    let ``Exposed ends peel at the expected rate, and capped strands only once hydrolysis cuts them`` () =
        let frozen = TestParameters.createFrozen 1.0
        let peeling = { frozen with PeelBarrier = 0.5 }

            // uncapped chains and dimers peel
        let checkSeed seed () =
            let rng = Random(seed)
            let world =
                Chain.grow rng 12
                    |> Chain.withRandomTypes rng
                    |> Chain.addDimers rng 5
                    |> Chain.addSoup rng 5
            let nBefore = (strongBonds world).Count
            let peeled =
                runEventRates $"Peeling, seed {seed}" peeling 40000 rng world

                // make sure the check isn't vacuous
            let nAfter = (strongBonds peeled).Count
            Assert.True(
                nAfter <= nBefore - 4,   // most of the five dimers, at least (a chain can be capped)
                $"Seed {seed}: only {nBefore - nAfter} of {nBefore} strong bonds peeled")

                // nothing breaks with peeling off
            let mutable unpeeled = world
            unpeeled <- Chain.updatesWith frozen rng 20000 unpeeled
            Assert.Equal<Set<int * int>>(strongBonds world, strongBonds unpeeled)

            // a strand P0-W1-W2 capped at its other end by the given cap
            // (atom 3), among free atoms
        let cappedStrand endCap rng =
            TestWorld.empty
                |> World.addAtom AtomType.P (Point.create (-3, 0))   // 0
                |> World.addAtom AtomType.W (Point.create (-1, 0))   // 1
                |> World.addAtom AtomType.W (Point.create (1, 0))    // 2
                |> World.addAtom endCap (Point.create (3, 0))        // 3
                |> World.addStrongBond TestParameters.standard 0 1
                |> World.addStrongBond TestParameters.standard 1 2
                |> World.addStrongBond TestParameters.standard 2 3
                |> Chain.addSoup rng 10

            // a capped strand doesn't peel
        let checkCapped () =
            let rng = Random(0)
            let capped = cappedStrand AtomType.P rng
            let mutable world = capped
            world <- Chain.updatesWith { peeling with PeelBarrier = 0.0 } rng 50000 world
            Assert.Equal<Set<int * int>>(strongBonds capped, strongBonds world)

            // with hydrolysis, a capped strand is taken apart: a bond
            // breaks, and then the pieces peel
        let checkHydrolysis () =
            let rng = Random(1)
            let capped = cappedStrand AtomType.Q rng
            let mutable world = capped
            world <- Chain.updatesWith (TestParameters.withHydrolysis Strong 0.2 peeling) rng 100000 world   // each bond expects several breaks
            Assert.Empty(
                strongBonds world
                    |> Set.filter (fun (idA, idB) -> idA <= 3 && idB <= 3))

            // hydrolysis alone, with peeling off, breaks every bond of a
            // capped strand, interior and caps alike, when it is sure to
            // happen (and nothing re-forms, since the barrier is impassable)
        let checkHydrolysisOnly () =
            let rng = Random(2)
            let capped = cappedStrand AtomType.Q rng
            let parms =
                { frozen with PeelBarrier = Double.PositiveInfinity }
                    |> TestParameters.withHydrolysis Strong 1.0
            let mutable world = capped
            world <- Chain.updatesWith parms rng 50000 world
            Assert.Empty(
                strongBonds world
                    |> Set.filter (fun (idA, idB) -> idA <= 3 && idB <= 3))

        inParallel [| checkSeed 0; checkSeed 1; checkSeed 2; checkCapped; checkHydrolysis; checkHydrolysisOnly |]

    /// Protection from hydrolysis: a strong bond that is one side of a
    /// stack hydrolyzes at its type's protected rate, and any other at
    /// the usual rate. In a box:
    /// * A duplex: a strand of four W atoms paired with one of three X
    ///   atoms, running the other way. Two of the W strand's bonds are
    ///   stacked, and so protected; its third has only one paired atom.
    /// * A capped strand P Y Y Q, with a free Z atom paired with each Y,
    ///   on the strand's face. Both atoms of the Y-Y bond are paired,
    ///   but it isn't stacked, so it isn't protected. (The two Zs
    ///   never bond to each other, since a Z-Z strong bond is made far
    ///   too costly to form.)
    /// Bonds of both types are too strong to break in the usual way, and
    /// strong bonds have no barrier, so a hydrolyzed bond soon forms
    /// again, which keeps hydrolysis going; nothing peels. The two rates
    /// are far apart, so a model that ignored protection, protected a
    /// bond with only one paired atom, or protected any bond whose atoms
    /// are both paired, would break bonds at the wrong rate.
    [<Fact>]
    let ``Stacked strong bonds are protected from hydrolysis`` () =
        let parms =
            TestParameters.basic 1.0
                |> TestParameters.withStrong (fun p -> { p with Energy = 30 })
                |> TestParameters.withPairEnergies Strong [ (AtomType.Z, AtomType.Z), -30.0 ]
                |> TestParameters.withWeak (fun p -> { p with Energy = 15 })
                |> TestParameters.withHydrolysis Strong 0.3
                |> TestParameters.withProtectedHydrolysis Strong 0.02
        let addStrand atomType (locs : (int * int) list) world =
            let first = (world : World).Atoms.Length
            let world =
                (world, locs)
                    ||> List.fold (fun world (x, y) ->
                        World.addAtom atomType (Point.create (x, y)) world)
            (world, [ first + 1 .. world.Atoms.Length - 1 ])
                ||> List.fold (fun world id ->
                    World.addStrongBond parms (id - 1) id world)
        let world =
            TestWorld.empty
                |> addStrand AtomType.W [ -3, 0; -1, 0; 1, 0; 3, 0 ]    // atoms 0 to 3, toward +x
                |> addStrand AtomType.X [ 1, 2; -1, 2; -3, 2 ]          // atoms 4 to 6, toward -x
                |> World.addWeakBond parms 0 6
                |> World.addWeakBond parms 1 5
                |> World.addWeakBond parms 2 4
                |> World.addAtom AtomType.P (Point.create (-3, -6))     // 7
                |> World.addAtom AtomType.Y (Point.create (-1, -6))     // 8
                |> World.addAtom AtomType.Y (Point.create (1, -6))      // 9
                |> World.addAtom AtomType.Q (Point.create (3, -6))      // 10
                |> World.addStrongBond parms 7 8
                |> World.addStrongBond parms 8 9
                |> World.addStrongBond parms 9 10
                |> World.addAtom AtomType.Z (Point.create (-1, -4))     // 11, above Y8
                |> World.addAtom AtomType.Z (Point.create (1, -4))      // 12, above Y9
                |> World.addWeakBond parms 8 11
                |> World.addWeakBond parms 9 12
                |> Chain.addBox 8
        checkEventRates "Duplex and paired strand, with protection" parms 300000 (Random(0)) world

/// Event rates in crowded worlds of chains, free atoms and walls, and
/// where temperature varies by location.
module UpdateRateTests =

    open PropertyOracles

    /// Valid events are actually made, at the right rates, in crowded
    /// worlds of chains, free atoms, and walls. This catches an update
    /// that wrongly rejects valid events, or accepts them at the wrong
    /// rate, which the test above cannot. Strands are nearly straight,
    /// or can bend up to right angles (at no cost, which the rates
    /// assume).
    [<Fact>]
    let ``update makes valid events at the expected rate`` () =
        inParallel [|
            for limit in [ TestParameters.straightBend; TestParameters.rightAngleBend ] do
                for kT in temperatures do
                    for length in [ 2; 20 ] do
                        fun () ->
                            let rng = Random(length)
                            let parms =
                                TestParameters.create kT
                                    |> TestParameters.withBend Strong limit 0.0
                            Chain.grow rng length
                                |> Chain.withRandomTypes rng
                                |> Chain.addSoup rng 10
                                |> Chain.addObstacles rng 10
                                |> checkEventRates
                                    $"N = {length}, kT {kT}, limit {limit.Numerator}/{limit.Denominator}"
                                    parms 20000 rng
        |]

    /// Bonds, at the right rates, where temperature varies by location:
    /// two atoms in a small box with a step in temperature down the
    /// middle, so that most bonds between them join atoms at very
    /// different temperatures. A bond's temperature is the average of
    /// those at its two atoms (see Chain.bondTemperature), which makes
    /// it the same whichever atom an update starts from.
    ///
    /// The steps are chosen so that, for a bond across the middle, the
    /// probability of breaking at the average temperature differs by
    /// 30% or more from that at either atom's alone. Once for weak bonds,
    /// which break by the usual test, and once for strong bonds, which
    /// can only break by peeling.
    [<Fact>]
    let ``Bonds across a step in temperature form and break at the expected rate`` () =
        let size = 2   // atom locations have |x| <= size and |y| <= size
        let createWorld typeA typeB =
            TestWorld.empty
                |> Chain.addBox size
                |> World.addAtom typeA (Point.create (-2, 0))
                |> World.addAtom typeB (Point.create (1, 0))
        let step kTLeft kTRight (pt : Point) =
            if pt.X < 0 then kTLeft else kTRight

            // weak bonds with some depth, so that how often they break
            // depends steeply on temperature; strong bonds frozen
        let weak =
            TestParameters.createFrozen 1.0
                |> TestParameters.withTemperature (step 1.0 10.0)
                |> TestParameters.withWeak (fun p -> { p with Energy = 3.0 })

            // strong bonds that form freely and are too strong to break in
            // the usual way, so every break is a peel (and no weak bonds,
            // since the atoms are of the same type)
        let peeling =
            TestParameters.basic nan
                |> TestParameters.withTemperature (step 0.25 25.0)
                |> TestParameters.withStrong (fun p -> { p with Energy = 30 })
                |> TestParameters.withWeak (fun p -> { p with Energy = 1 })
                |> TestParameters.withPeelBarrier 2.0

        inParallel [|
            fun () ->
                checkEventRates
                    "Weak bonds across a step"
                    weak 1000000 (Random(1)) (createWorld AtomType.W AtomType.X)
            fun () ->
                checkEventRates
                    "Peeling across a step"
                    peeling 1000000 (Random(2)) (createWorld AtomType.W AtomType.W)
        |]
