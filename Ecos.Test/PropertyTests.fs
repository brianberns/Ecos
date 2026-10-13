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

    /// Are the given atoms strongly bonded?
    let private isStronglyBonded = Bonds.areStronglyBonded

    /// Strong bonds in the given world, as (smaller ID, larger ID)
    /// pairs.
    let private strongBonds = Bonds.strongBonds

    /// Weak bonds in the given world, as (smaller ID, larger ID) pairs.
    let private weakBonds = Bonds.weakBonds

    /// All bonds in the given world, strong and weak, as
    /// (smaller ID, larger ID) pairs.
    let private allBonds (world : World) =
        Set.toList (strongBonds world) @ Set.toList (weakBonds world)

    /// Oracle: would a bond between the given atoms cross any bond in
    /// the given world? Bonds that share an atom with it can't cross
    /// it. This checks every bond in the world, so it doesn't depend on
    /// World's assumption that only nearby bonds need checking.
    let private crossesAnyBond (world : World) idA idB =
        let loc id = world.Atoms[id].Location
        allBonds world
            |> List.exists (fun (idC, idD) ->
                idC <> idA && idC <> idB && idD <> idA && idD <> idB
                    && segmentsIntersect (loc idA) (loc idB) (loc idC) (loc idD))

    /// The given atom's partners by bonds of the given type.
    let private partners bondType (world : World) id =
        world.BondTypeMap[bondType]
            |> Map.tryFind id
            |> Option.defaultValue Set.empty

    /// Oracle: is the angle between the given atom's bonds of the given
    /// type wide enough, when the atoms are at the locations given by
    /// the given function? (Only an atom with two such bonds can fail
    /// this.) For strong bonds, this keeps strands from bending
    /// sharply; for weak bonds, it keeps an atom's two partners on
    /// opposite sides of it.
    let private anglesAreAllowed bondType (world : World) (loc : AtomId -> Point) id =
        let ids = Set.toList (partners bondType world id)
        List.allPairs ids ids
            |> List.forall (fun (idB, idC) ->
                idB >= idC
                    || Bonds.isAngleAllowed bondType (loc id) (loc idB) (loc idC))

    /// Oracle: would the angle between the given atom's bonds of the
    /// given type still be wide enough if it also had such a bond to
    /// the given new partner?
    let private hasRoomAtAngle bondType (world : World) id idNew =
        let loc id = world.Atoms[id].Location
        partners bondType world id
            |> Set.forall (fun idB ->
                Bonds.isAngleAllowed bondType (loc id) (loc idB) (loc idNew))

    /// Checks all of a world's invariants.
    let private isValid (world : World) =
        let atoms = world.Atoms |> Seq.toArray
        let loc id = world.Atoms[id].Location

            // no two atoms overlap, no atom overlaps a wall, and no two
            // walls overlap (a wall occupies the same square as an atom)
        let noOverlaps =
            atoms
                |> Seq.forall (fun a ->
                    atoms
                        |> Array.forall (fun b ->
                            a.Id = b.Id || not (overlaps a.Location b.Location))
                        && world.Walls
                            |> Seq.forall (fun wall -> not (overlaps a.Location wall)))
                && world.Walls
                    |> Seq.forall (fun wallA ->
                        world.Walls
                            |> Seq.forall (fun wallB ->
                                wallA = wallB || not (overlaps wallA wallB)))

            // every strong bond is allowed and symmetric, no atom has more
            // than its type permits (one for a cap, otherwise two), no two
            // caps are bonded, and two bonds at one atom don't bend too
            // sharply
        let strongBondsValid =
            (Bonds.strongBondMap world)
                |> Map.forall (fun idA ids ->
                    ids.Count <= Chain.strongValence world.Atoms[idA].Type
                        && anglesAreAllowed Strong world loc idA
                        && ids
                            |> Set.forall (fun idB ->
                                isBondVector (loc idB - loc idA)
                                    && Chain.canJoin world.Atoms[idA].Type world.Atoms[idB].Type
                                    && isStronglyBonded world idB idA))

            // no atom is listed with an empty set of bonds
        let bondSetsValid =
            world.BondTypeMap.Values
                |> Seq.forall (Map.forall (fun _ ids -> not ids.IsEmpty))

            // no bond, strong or weak, crosses another
        let noCrossings =
            allBonds world
                |> List.forall (fun (idA, idB) ->
                    not (crossesAnyBond world idA idB))

            // every weak bond is symmetric, in range, between
            // complementary types, and between atoms that aren't also
            // strongly bonded
        let weakBondsValid =
            (Bonds.weakBondMap world)
                |> Map.forall (fun idA ids ->
                    ids
                        |> Set.forall (fun idB ->
                            Bonds.areWeaklyBonded world idB idA
                                && Array.contains (loc idB - loc idA) Chain.weakBondVectors
                                && Chain.canPair world.Atoms[idA].Type world.Atoms[idB].Type
                                && not (isStronglyBonded world idA idB)))

            // no atom has more than two weak partners, and only an atom
            // in a strand (one with a strong bond) has two; two partners are
            // on opposite sides of the atom, and (as a consequence) are
            // never strongly bonded to each other
        let weakPartnersValid =
            (Bonds.weakBondMap world)
                |> Map.forall (fun idA ids ->
                    ids.Count <= 2
                        && (ids.Count < 2 || not (partners Strong world idA).IsEmpty)
                        && anglesAreAllowed Strong world loc idA
                        && anglesAreAllowed Weak world loc idA
                        && List.allPairs (Set.toList ids) (Set.toList ids)
                            |> List.forall (fun (idB, idC) ->
                                not (isStronglyBonded world idB idC)))

            // every atom is stored under its own ID, the occupancy map is
            // exactly the atoms' locations, and the cached atom count is
            // right
        let occupancyConsistent =
            world.OccupancyMap.Count = world.Atoms.Count
                && world.Atoms.Count = world.Atoms.Count
                && atoms
                    |> Array.forall (fun atom ->
                        Dictionary.tryFind atom.Location world.OccupancyMap
                            = Some atom.Id)

        noOverlaps && strongBondsValid && bondSetsValid && noCrossings
            && weakBondsValid && weakPartnersValid && occupancyConsistent

    /// Unit steps an atom can attempt.
    let private directions =
        [ 1, 0; 0, -1; -1, 0; 0, 1 ]
            |> List.map Point.create

    /// Oracle: all valid moves in the given world, as (atom ID, step)
    /// pairs. A move is valid if the moved atom overlaps no other atom
    /// or wall, all of its bonds (strong and weak) remain in range, and
    /// every bond angle it's part of remains wide enough: strands stay
    /// straight enough, and
    /// weak partners remain on opposite sides, both around the moved
    /// atom and around each of its weak partners.
    let private validMoves (world : World) =
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
                                atomB.Id = id || not (overlaps newLoc atomB.Location))
                            && world.Walls
                                |> Seq.forall (fun wall -> not (overlaps newLoc wall))
                    let strongBondsOk =
                        (Bonds.strongBondMap world)
                            |> Map.tryFind id
                            |> Option.defaultValue Set.empty
                            |> Set.forall (fun idB ->
                                isBondVector (loc idB - newLoc))
                    let weakPartners = Bonds.weakPartners world id
                    let weakBondsOk =
                        weakPartners
                            |> Set.forall (fun idB ->
                                Array.contains (loc idB - newLoc) Chain.weakBondVectors)
                    let anglesOk =
                        BondType.all
                            |> Array.forall (fun bondType ->
                                anglesAreAllowed bondType world loc id
                                    && partners bondType world id
                                        |> Set.forall (anglesAreAllowed bondType world loc))
                    if clear && strongBondsOk && weakBondsOk && anglesOk then
                        id, delta
        ]

    /// Oracle: can the given atoms form a new weak bond? They must be
    /// complementary, in range, and not already bonded (weakly or
    /// strongly); each must have no weak partner yet, or have one and
    /// be part of a strand (have a strong bond), in which case the
    /// existing partner must be on the opposite side from the new one;
    /// and the new bond must not cross any existing bond.
    let private canFormWeakBond (world : World) idA idB =
        let atomA = world.Atoms[idA]
        let atomB = world.Atoms[idB]
        let hasRoom idC idNew =
            let nWeak = (Bonds.weakPartners world idC).Count
            let inStrand = not (partners Strong world idC).IsEmpty
            (nWeak = 0 || (nWeak = 1 && inStrand))
                && hasRoomAtAngle Weak world idC idNew
        Chain.canPair atomA.Type atomB.Type
            && Array.contains (atomB.Location - atomA.Location) Chain.weakBondVectors
            && not (Bonds.areWeaklyBonded world idA idB)
            && not (isStronglyBonded world idA idB)
            && hasRoom idA idB
            && hasRoom idB idA
            && not (crossesAnyBond world idA idB)

    /// Oracle: are the two given weak bonds stacked? They are if their
    /// ends are strongly bonded on both sides, either way round.
    let private areStacked (world : World) (idA, idB) (idC, idD) =
        (isStronglyBonded world idA idC && isStronglyBonded world idB idD)
            || (isStronglyBonded world idA idD && isStronglyBonded world idB idC)

    /// Oracle: number of stacks that a weak bond between the given
    /// atoms takes part in (or would, if formed): the other weak bonds
    /// that it's stacked with. Found by scanning every weak bond in
    /// the world, rather than by walking out from A as World does.
    let private countStacks (world : World) idA idB =
        weakBonds world
            |> Seq.filter (fun (idC, idD) ->
                set [ idC; idD ] <> set [ idA; idB ]
                    && areStacked world (idA, idB) (idC, idD))
            |> Seq.length

    /// Number of strong bonds the given atom has.
    let private strongBondCount (world : World) id =
        Bonds.strongBondMap world
            |> Map.tryFind id
            |> Option.map Set.count
            |> Option.defaultValue 0

    /// Oracle: can the given atoms form a new strong bond? They must
    /// be in range, not both be caps, each have fewer strong bonds than
    /// its type permits (one for a cap, otherwise two), and not
    /// already be bonded (strongly or weakly), and the new bond must
    /// not cross any existing bond or bend either atom's strand too
    /// sharply. Rings are allowed, if they're big enough to be that
    /// straight.
    let private canFormStrongBond (world : World) idA idB =
        let atomA = world.Atoms[idA]
        let atomB = world.Atoms[idB]
        isBondVector (atomB.Location - atomA.Location)
            && Chain.canJoin atomA.Type atomB.Type
            && strongBondCount world idA < Chain.strongValence atomA.Type
            && strongBondCount world idB < Chain.strongValence atomB.Type
            && not (isStronglyBonded world idA idB)
            && not (Bonds.areWeaklyBonded world idA idB)
            && hasRoomAtAngle Strong world idA idB
            && hasRoomAtAngle Strong world idB idA
            && not (crossesAnyBond world idA idB)

    /// Oracle: number of stacks that exist only because of a strong
    /// bond between the given atoms (or would, if it formed). Each
    /// pair of a weak bond of A with a weak bond of B is stacked if
    /// their ends are strongly bonded on both sides. Bond A-B is one of
    /// those sides, so it makes the difference when the two partners
    /// are strongly bonded to each other, unless the pair is already
    /// stacked the other way round. With two weak partners each, up to
    /// two stacks can depend on one strong bond: one on each side.
    let private countStacksStrong (world : World) idA idB =
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

    /// Oracle: can the strong bond between the given atoms break? Not
    /// if it's the last strong bond of an atom that has two weak
    /// partners, since only an atom in a strand can have two.
    let private canBreakStrongBond (world : World) idA idB =
        [ idA; idB ]
            |> List.forall (fun id ->
                (Bonds.weakPartners world id).Count < 2
                    || (partners Strong world id).Count > 1)

    /// Oracle: is the strong bond between the given atoms at an exposed
    /// end of a strand? It is if either atom has no other strong bond
    /// and isn't a cap. Such a bond can peel.
    let private isExposed (world : World) idA idB =
        [ idA; idB ]
            |> List.exists (fun id ->
                not (Chain.isCap world.Atoms[id].Type)
                    && (partners Strong world id).Count = 1)

    /// Is the given bond vector longer than any weak bond?
    let private isLong (v : Point) =
        not (Array.contains v Chain.weakBondVectors)

    /// Do the given bonds include one longer than any weak bond?
    let private isLongBond (world : World) (bonds : Set<AtomId * AtomId>) =
        bonds
            |> Set.exists (fun (idA, idB) ->
                isLong (world.Atoms[idB].Location - world.Atoms[idA].Location))

    /// Do the given strong bonds include one that a stack depends on?
    let private isStackedBond (world : World) (bonds : Set<AtomId * AtomId>) =
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
    /// bonded to the first, it proposes breaking the bond. Either
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
    /// whatever its energy. Nothing reverses that, so it takes the
    /// model out of equilibrium.
    let private toggleProbabilities (parms : Parameters) (world : World) =
        let kT = parms.Temperature
        let nAtoms = float world.Atoms.Count
        let mutable weakForm, weakBreak = 0.0, 0.0
        let mutable strongForm, strongBreak = 0.0, 0.0
        let mutable longForm, longBreak = 0.0, 0.0
        let mutable stackedBreak = 0.0

            // probability of forming, or of breaking, a bond of the given
            // type and energy that the given number of stacks depend on.
            // Forming changes the energy by the bond's (negative) energy and
            // loses the bond type's entropy; breaking does the reverse.
        let acceptWith bondType nStacks isStart energyDelta entropyDelta =
            exp (-Chain.bondBarrier parms bondType nStacks isStart / kT)
                * min 1.0 (exp (entropyDelta - energyDelta / kT))
        let acceptForm bondType nStacks isStart energy =
            acceptWith bondType nStacks isStart energy -(Chain.bondEntropy parms bondType)
        let acceptBreak bondType nStacks isStart energy =
            acceptWith bondType nStacks isStart -energy (Chain.bondEntropy parms bondType)

            // does a bond of the given type between the given atoms start a
            // strand? It does if neither atom has another bond of that type.
        let isStart bondType idA idB =
            (partners bondType world idA).Remove(idB).IsEmpty
                && (partners bondType world idB).Remove(idA).IsEmpty

        for atomA in world.Atoms do
            let idA = atomA.Id

            for v in Chain.weakBondVectors do
                match Dictionary.tryFind (atomA.Location + v) world.OccupancyMap with
                    | Some idB ->
                        let nStacks = countStacks world idA idB
                        let energy =
                            Chain.bondEnergy parms Weak
                                + Chain.stackEnergy * float nStacks
                        if Bonds.areWeaklyBonded world idA idB then
                            weakBreak <- weakBreak + acceptBreak Weak nStacks (isStart Weak idA idB) energy
                        elif canFormWeakBond world idA idB then
                            weakForm <- weakForm + acceptForm Weak nStacks (isStart Weak idA idB) energy
                    | None -> ()

            for v in Chain.bondVectors do
                match Dictionary.tryFind (atomA.Location + v) world.OccupancyMap with
                    | Some idB ->
                        let nStacks = countStacksStrong world idA idB
                        let energy =
                            Chain.bondEnergy parms Strong
                                + Chain.stackEnergy * float nStacks
                        if isStronglyBonded world idA idB then

                                // the bond breaks if the usual test passes or,
                                // failing that, if it peels
                            let p =
                                if canBreakStrongBond world idA idB then
                                    let pBreak = acceptBreak Strong nStacks (isStart Strong idA idB) energy
                                    let pPeel =
                                        if isExposed world idA idB then
                                            exp (-Chain.peelBarrier parms Strong / kT)
                                        else 0.0
                                    pBreak + (1.0 - pBreak) * pPeel
                                else 0.0
                            strongBreak <- strongBreak + p
                            if isLong v then longBreak <- longBreak + p
                            if nStacks > 0 then stackedBreak <- stackedBreak + p
                        elif canFormStrongBond world idA idB then
                            let p = acceptForm Strong nStacks (isStart Strong idA idB) energy
                            strongForm <- strongForm + p
                            if isLong v then longForm <- longForm + p
                    | None -> ()

        let nWeak = nAtoms * float Chain.weakBondVectors.Length
        let nStrong = nAtoms * float Chain.bondVectors.Length
        weakForm / nWeak, weakBreak / nWeak,
            strongForm / nStrong, strongBreak / nStrong,
            longForm / nStrong, longBreak / nStrong,
            stackedBreak / nStrong

    /// All pairs of bonds that intersect, excluding pairs that share
    /// an atom.
    let private crossings (world : World) =
        let bonds = allBonds world
        let loc id = world.Atoms[id].Location
        set [
            for i, (a, b) in List.indexed bonds do
                for c, d in List.skip (i + 1) bonds do
                    if a <> c && a <> d && b <> c && b <> d
                        && segmentsIntersect (loc a) (loc b) (loc c) (loc d) then
                        (a, b), (c, d)
        ]

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
        (Some World.empty, items)
            ||> List.fold (fun worldOpt (isWall, x, y) ->
                worldOpt
                    |> Option.bind (fun world ->
                        let loc = Point.create (x % 6, y % 6)   // crowd things together
                        let expected =
                            world.Atoms
                                |> Seq.forall (fun atom -> not (overlaps loc atom.Location))
                                && world.Walls
                                    |> Seq.forall (fun wall -> not (overlaps loc wall))
                        let actual =
                            if isWall then World.tryAddWall loc world
                            else World.tryAddAtomW loc world
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
                    |> World.addAtomW origin
                    |> World.addAtomW v
            (World.tryAddStrongBond 0 1 world).IsSome = isBondVector v)

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
                        if not (overlaps origin v) then
                            let world =
                                World.empty
                                    |> World.addAtom typeA origin
                                    |> World.addAtom typeB v
                            let expected =
                                Chain.canPair typeA typeB
                                    && Array.contains v Chain.weakBondVectors
                            Assert.True(
                                ((World.tryAddWeakBond 0 1 world).IsSome = expected),
                                $"{typeA}-{typeB} at {v}: expected {expected}")

    /// Valence and exclusivity: an atom can have at most two strong
    /// bonds and two weak bonds, and two atoms can't be bonded both
    /// strongly and weakly.
    [<Fact>]
    let ``Bond limits are enforced`` () =

            // W1 in the middle, with W0 and W2 on either side, and
            // X3 and X4 above and below it, all in range of W1
        let world =
            World.empty
                |> World.addAtom AtomType.W (Point.create (-2, 0))   // 0
                |> World.addAtom AtomType.W (Point.create (0, 0))    // 1
                |> World.addAtom AtomType.W (Point.create (2, 0))    // 2
                |> World.addAtom AtomType.X (Point.create (0, 2))    // 3
                |> World.addAtom AtomType.X (Point.create (0, -2))   // 4

            // strong bonds: a third one is rejected
        let world =
            world
                |> World.addStrongBond 0 1
                |> World.addStrongBond 1 2
        Assert.True((World.tryAddStrongBond 1 3 world).IsNone, "Third strong bond accepted")

            // a strongly bonded pair can't also pair weakly
        let pair =
            World.empty
                |> World.addAtom AtomType.W (Point.create (0, 0))
                |> World.addAtom AtomType.X (Point.create (0, 2))
        Assert.True((World.tryAddWeakBond 0 1 pair).IsSome, "Weak bond rejected")
        Assert.True(
            (World.tryAddWeakBond 0 1 (World.addStrongBond 0 1 pair)).IsNone,
            "Weak bond accepted between strongly bonded atoms")

            // weak bonds: a second one on the opposite side is accepted,
            // from either side
        let world = World.addWeakBond 1 3 world
        Assert.True((World.tryAddWeakBond 1 4 world).IsSome, "Second weak bond rejected")
        Assert.True((World.tryAddWeakBond 4 1 world).IsSome, "Second weak bond rejected")

            // a weakly bonded pair can't also bond strongly, even with
            // spare strong valence (W0 has one strong bond, X4 none),
            // and the same weak bond can't be added twice
        let world = World.addWeakBond 0 4 world
        Assert.True((World.tryAddStrongBond 4 0 world).IsNone, "Strong bond accepted between weakly bonded atoms")
        Assert.True((World.tryAddWeakBond 4 0 world).IsNone, "Duplicate weak bond accepted")

            // weak bonds: a third one is rejected. W0 at the center, in a
            // strand with Y4, with X1 and X2 on opposite sides of it, and
            // X3 also in range
        let world =
            World.empty
                |> World.addAtom AtomType.W (Point.create (0, 0))    // 0
                |> World.addAtom AtomType.X (Point.create (0, 2))    // 1
                |> World.addAtom AtomType.X (Point.create (0, -2))   // 2
                |> World.addAtom AtomType.X (Point.create (2, 0))    // 3
                |> World.addAtom AtomType.Y (Point.create (-2, 0))   // 4
                |> World.addStrongBond 0 4
                |> World.addWeakBond 0 1
                |> World.addWeakBond 0 2
        Assert.True((World.tryAddWeakBond 0 3 world).IsNone, "Third weak bond accepted")
        Assert.True((World.tryAddWeakBond 3 0 world).IsNone, "Third weak bond accepted")

            // weak bonds: a free atom (one with no strong bond) can't have a
            // second one, even on the opposite side
        let world =
            World.empty
                |> World.addAtom AtomType.W (Point.create (0, 0))    // 0
                |> World.addAtom AtomType.X (Point.create (0, 2))    // 1
                |> World.addAtom AtomType.X (Point.create (0, -2))   // 2
                |> World.addWeakBond 0 1
        Assert.True((World.tryAddWeakBond 0 2 world).IsNone, "Free atom got a second weak bond")
        Assert.True((World.tryAddWeakBond 2 0 world).IsNone, "Free atom got a second weak bond")

    /// Caps, exhaustively: atoms A, B and C lie in a row, close enough
    /// for A-B and B-C to be strongly bonded. For every combination of
    /// types:
    /// * A-B is accepted if and only if A and B aren't both caps.
    /// * Then B-C is accepted if and only if B isn't a cap, since a cap
    ///   can have only one strong bond. C may be a cap: it's C's first
    ///   bond, and B isn't a cap if the bond is allowed at all.
    /// Each bond is tried from both ends.
    [<Fact>]
    let ``tryAddStrongBond gives a cap exactly one strong bond`` () =
        let mutable nAccepted, nRejected = 0, 0
        for typeA in Chain.allTypes do
            for typeB in Chain.allTypes do
                for typeC in Chain.allTypes do
                    let world =
                        World.empty
                            |> World.addAtom typeA (Point.create (-3, 0))   // 0
                            |> World.addAtom typeB (Point.create (0, 0))    // 1
                            |> World.addAtom typeC (Point.create (3, 0))    // 2

                        // first bond
                    let expected = not (Chain.isCap typeA && Chain.isCap typeB)
                    for idFrom, idTo in [ 0, 1; 1, 0 ] do
                        Assert.True(
                            ((World.tryAddStrongBond idFrom idTo world).IsSome = expected),
                            $"{typeA}-{typeB}: expected {expected}")

                        // second bond
                    if expected then
                        let world = World.addStrongBond 0 1 world
                        let expected = not (Chain.isCap typeB)
                        for idFrom, idTo in [ 1, 2; 2, 1 ] do
                            Assert.True(
                                ((World.tryAddStrongBond idFrom idTo world).IsSome = expected),
                                $"{typeA}-{typeB}, then {typeB}-{typeC}: expected {expected}")
                        if expected then nAccepted <- nAccepted + 1
                        else nRejected <- nRejected + 1

            // make sure the check isn't vacuous
        Assert.True(nAccepted > 0, "No accepted configurations checked")
        Assert.True(nRejected > 0, "No rejected configurations checked")

    /// A cap at the end of a strand pairs like any other atom in a
    /// strand: with one partner on each side. P0 is strongly bonded to
    /// W1, with Q2 above it and Q3 below.
    [<Fact>]
    let ``A cap in a strand can have two weak partners`` () =
        let world =
            World.empty
                |> World.addAtom AtomType.P (Point.create (0, 0))    // 0
                |> World.addAtom AtomType.W (Point.create (2, 0))    // 1
                |> World.addAtom AtomType.Q (Point.create (0, 2))    // 2
                |> World.addAtom AtomType.Q (Point.create (0, -2))   // 3
                |> World.addWeakBond 0 2
        Assert.True(
            (World.tryAddWeakBond 0 3 world).IsNone,
            "Free cap got a second weak bond")
        let world = World.addStrongBond 0 1 world
        Assert.True(
            (World.tryAddWeakBond 0 3 world).IsSome,
            "Cap in a strand was refused a second weak bond")

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
            {
                Temperature = 1.0
                BondParameterMap =
                    Map [
                        Strong, { Energy = 30; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 0; Entropy = 0 }
                        Weak, { Energy = 1; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 0; Entropy = 0 }
                    ]
                PeelBarrier = Double.PositiveInfinity   // no peeling
            }
        let mutable world =
            World.empty
                |> World.addAtom AtomType.P (Point.create (-3, 0))   // 0
                |> World.addAtom AtomType.W (Point.create (-1, 0))   // 1
                |> World.addAtom AtomType.W (Point.create (1, 0))    // 2
                |> World.addAtom AtomType.P (Point.create (3, 0))    // 3
                |> World.addStrongBond 0 1
                |> World.addStrongBond 1 2
                |> World.addStrongBond 2 3
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
                    Assert.True(isValid world', "World became invalid")
                    world <- world'
                | None -> ()   // nothing happened
        Assert.Equal<Set<int * int>>(expected, strandBonds world)

            // make sure the check isn't vacuous: free atoms did join each
            // other, including some that started next to the strand
        Assert.True(nJoins > 5, $"Only {nJoins} strong bonds formed")

    /// Opposite sides, exhaustively: atom A (type W, at the origin) is
    /// weakly bonded to B (type X). A second weak bond, to C (also type
    /// X), must be accepted if and only if B and C are on opposite
    /// sides of A. Checked for every pair of locations in weak bond
    /// range of A, and with the new bond added from either end.
    ///
    /// A is strongly bonded to another atom, D (type Y), since only an
    /// atom in a strand can have two weak partners. D is placed
    /// wherever it fits.
    ///
    /// Whenever the second bond is accepted, B and C must also be too
    /// far apart to be strongly bonded to each other. That's what
    /// stops one atom from pairing with two neighbors in the same
    /// strand.
    [<Fact>]
    let ``tryAddWeakBond accepts a second partner exactly on the opposite side`` () =
        let a = Point.create (0, 0)
        let mutable nAccepted, nRejected = 0, 0
        for b in Chain.weakBondVectors do
            for c in Chain.weakBondVectors do
                if not (overlaps b c) then
                    let d =
                        Chain.bondVectors
                            |> Array.find (fun d ->
                                not (overlaps d b) && not (overlaps d c))
                    let world =
                        World.empty
                            |> World.addAtom AtomType.W a   // 0
                            |> World.addAtom AtomType.X b   // 1
                            |> World.addAtom AtomType.X c   // 2
                            |> World.addAtom AtomType.Y d   // 3
                            |> World.addStrongBond 0 3
                            |> World.addWeakBond 0 1
                    let expected = Bonds.areOpposite a b c
                    for idFrom, idTo in [ 0, 2; 2, 0 ] do
                        Assert.True(
                            ((World.tryAddWeakBond idFrom idTo world).IsSome = expected),
                            $"B at {b}, C at {c}: expected {expected}")
                    if expected then
                        nAccepted <- nAccepted + 1
                        Assert.False(
                            isBondVector (c - b),
                            $"B at {b}, C at {c}: both paired with A, but in strong bond range of each other")
                    else nRejected <- nRejected + 1

            // make sure the check isn't vacuous
        Assert.True(nAccepted > 0, "No accepted configurations checked")
        Assert.True(nRejected > 0, "No rejected configurations checked")

    /// Stiffness, exhaustively: atom A (at the origin) is strongly
    /// bonded to B. A second strong bond, to C, must be accepted if and
    /// only if the angle between the two bonds at A is wide enough.
    /// Checked for every pair of locations in strong bond range of A,
    /// and with the new bond added from either end.
    [<Fact>]
    let ``tryAddStrongBond accepts a second bond exactly when it's straight enough`` () =
        let a = Point.create (0, 0)
        let mutable nAccepted, nRejected = 0, 0
        for b in Chain.bondVectors do
            for c in Chain.bondVectors do
                if not (overlaps b c) then
                    let world =
                        World.empty
                            |> World.addAtomW a   // 0
                            |> World.addAtomW b   // 1
                            |> World.addAtomW c   // 2
                            |> World.addStrongBond 0 1
                    let expected = Bonds.isAngleAllowed Strong a b c
                    for idFrom, idTo in [ 0, 2; 2, 0 ] do
                        Assert.True(
                            ((World.tryAddStrongBond idFrom idTo world).IsSome = expected),
                            $"B at {b}, C at {c}: expected {expected}")
                    if expected then nAccepted <- nAccepted + 1
                    else nRejected <- nRejected + 1

            // make sure the check isn't vacuous
        Assert.True(nAccepted > 0, "No accepted configurations checked")
        Assert.True(nRejected > 0, "No rejected configurations checked")

    /// Opposite sides stay opposite: a hop can't bring an atom's two
    /// weak partners onto the same side of it. Here W0 sits between X1
    /// and X2, weakly bonded to both, and is strongly bonded to Y3 (so
    /// that it's allowed two weak partners), in a box that leaves all
    /// of them room to move. Strong bonds are frozen. After every
    /// update, the world must still be valid, which includes X1 and X2
    /// being on opposite sides of W0 whenever both bonds exist.
    [<Fact>]
    let ``Weak partners stay on opposite sides`` () =
        let rng = Random(0)
        let parms = TestParameters.createFrozen 0.3   // cold, so that both bonds usually exist
        let mutable world =
            World.empty
                |> World.addAtom AtomType.W (Point.create (0, 0))    // 0
                |> World.addAtom AtomType.X (Point.create (0, 2))    // 1
                |> World.addAtom AtomType.X (Point.create (0, -2))   // 2
                |> World.addAtom AtomType.Y (Point.create (2, 0))    // 3
                |> World.addStrongBond 0 3
                |> World.addWeakBond 0 1
                |> World.addWeakBond 0 2
                |> Chain.addBox 4
        let mutable nBothBonded = 0
        for _ in 1 .. 20000 do
            world <- World.updateWith parms rng world
            if (Bonds.weakPartners world 0).Count = 2 then
                nBothBonded <- nBothBonded + 1
            Assert.True(isValid world, "World became invalid")

            // make sure the check isn't vacuous
        Assert.True(nBothBonded > 1000, $"Both bonds existed in only {nBothBonded} updates")

    /// Only an atom in a strand has two weak partners: W0 is weakly
    /// bonded to X1 and X2, which it's allowed because it's strongly
    /// bonded to Y3. That strong bond is W0's only one, so it must not
    /// break while W0 has both weak partners.
    ///
    /// Here strong bonds have no energy or barrier, so they break at
    /// every opportunity, and weak bonds are strong, so both usually
    /// exist. After every update, the world must still be valid, which
    /// includes W0 having a strong bond whenever it has two weak
    /// partners. The strong bond must also break sometimes, when W0
    /// has fewer than two, or the test would pass trivially.
    [<Fact>]
    let ``An atom with two weak partners keeps a strong bond`` () =
        let rng = Random(0)
        let parms =
            {
                Temperature = 1.0
                BondParameterMap =
                    Map [
                        Strong, { Energy = 0; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 0; Entropy = 0 }
                        Weak, { Energy = 3; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 0; Entropy = 0 }
                    ]
                PeelBarrier = Double.PositiveInfinity   // no peeling
            }
        let mutable world =
            World.empty
                |> World.addAtom AtomType.W (Point.create (0, 0))    // 0
                |> World.addAtom AtomType.X (Point.create (0, 2))    // 1
                |> World.addAtom AtomType.X (Point.create (0, -2))   // 2
                |> World.addAtom AtomType.Y (Point.create (2, 0))    // 3
                |> World.addStrongBond 0 3
                |> World.addWeakBond 0 1
                |> World.addWeakBond 0 2
                |> Chain.addBox 2
        let mutable nBothBonded, nBroken = 0, 0
        for _ in 1 .. 50000 do
            let world' = World.updateWith parms rng world
            if (Bonds.weakPartners world' 0).Count = 2 then
                nBothBonded <- nBothBonded + 1
            if (strongBonds world').Count < (strongBonds world).Count then
                nBroken <- nBroken + 1
            Assert.True(
                isValid world',
                "World became invalid: an atom has two weak partners and no strong bond")
            world <- world'

            // make sure the check isn't vacuous
        Assert.True(nBothBonded > 1000, $"Both weak bonds existed in only {nBothBonded} updates")
        Assert.True(nBroken > 0, "No strong bond ever broke")

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
        Seq.toList world.Atoms
            |> List.choose (fun atom ->
                let d = world'.Atoms[atom.Id].Location - atom.Location
                if d = Point.create (0, 0) then None else Some (atom.Id, d))

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
    /// * the breaking of one existing bond (if that's allowed), or
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
                        elif strongBroken.Count = 1 then
                            let idA, idB = Seq.exactlyOne strongBroken
                            canBreakStrongBond world idA idB
                        else true   // any existing weak bond may break
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
    let private runEventRates label (parms : Parameters) nSteps (rng : Random) (world : World) =
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
                    * float (validMoves world).Length / float (4 * world.Atoms.Count)
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
                $"{label}, kT = {parms.Temperature}: {actual[i]} {kind}, expected {expected[i]:F1} ± {tolerance:F1}")
        world

    /// As runEventRates, but answers nothing.
    let private checkEventRates label parms nSteps rng world =
        runEventRates label parms nSteps rng world |> ignore

    /// Runs the given independent checks at the same time, to keep the
    /// tests quick. Each check needs its own random number generator.
    let private inParallel (checks : (unit -> unit)[]) =
        try
            Array.Parallel.iter (fun check -> check ()) checks
        with :? AggregateException as exn ->
            raise (exn.Flatten().InnerExceptions[0])   // report the first failure itself

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
                    |> checkEventRates $"N = {length}" (TestParameters.create kT) 20000 rng

    /// Peeling: with strong bonds otherwise frozen (an impassable
    /// barrier), the only way one can break is by peeling from an
    /// exposed end, and none can form.
    /// * In crowded worlds of uncapped chains and dimers, strong bonds
    ///   break at the expected rate until few are left. (See
    ///   toggleProbabilities.)
    /// * A capped strand, P0-W1-W2-P3, never loses a bond.
    /// * With peeling turned off, nothing breaks at all.
    [<Fact>]
    let ``Exposed ends peel at the expected rate, and capped ends never do`` () =
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
            for _ in 1 .. 20000 do
                unpeeled <- World.updateWith frozen rng unpeeled
            Assert.Equal<Set<int * int>>(strongBonds world, strongBonds unpeeled)

            // a capped strand doesn't peel
        let checkCapped () =
            let rng = Random(0)
            let capped =
                World.empty
                    |> World.addAtom AtomType.P (Point.create (-3, 0))   // 0
                    |> World.addAtom AtomType.W (Point.create (-1, 0))   // 1
                    |> World.addAtom AtomType.W (Point.create (1, 0))    // 2
                    |> World.addAtom AtomType.P (Point.create (3, 0))    // 3
                    |> World.addStrongBond 0 1
                    |> World.addStrongBond 1 2
                    |> World.addStrongBond 2 3
                    |> Chain.addSoup rng 10
            let mutable world = capped
            for _ in 1 .. 50000 do
                world <- World.updateWith { peeling with PeelBarrier = 0.0 } rng world
            Assert.Equal<Set<int * int>>(strongBonds capped, strongBonds world)

        inParallel [| checkSeed 0; checkSeed 1; checkSeed 2; checkCapped |]

    /// Worlds for testing stacking: four atoms confined to a small box,
    /// so that bonds between them form and break constantly.
    ///
    /// * Ladder: W0-W1 and X2-X3 are both strongly bonded. When both
    ///   rungs (0-2 and 1-3, or 0-3 and 1-2) exist, they form a stack.
    /// * Near miss: W0-W1 is strongly bonded, but X2 and X3 are free.
    ///   The same two rungs can exist, but they don't form a stack,
    ///   since X2 and X3 aren't strongly bonded.
    let private stackingWorlds =
        let atoms =
            World.empty
                |> World.addAtom AtomType.W (Point.create (-1, -1))   // 0
                |> World.addAtom AtomType.W (Point.create (1, -1))    // 1
                |> World.addAtom AtomType.X (Point.create (-1, 1))    // 2
                |> World.addAtom AtomType.X (Point.create (1, 1))     // 3
                |> World.addStrongBond 0 1
                |> Chain.addBox 2
        [
            "Ladder", World.addStrongBond 2 3 atoms
            "Near miss", atoms
        ]

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
            {
                Temperature = 1.0
                BondParameterMap =
                    Map [
                        Strong, { Energy = 0; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 0; Entropy = 0 }
                        Weak, { Energy = 3; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 0; Entropy = 0 }
                    ]
                PeelBarrier = Double.PositiveInfinity   // no peeling
            }
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
            {
                Temperature = 1.0
                BondParameterMap =
                    Map [
                        Strong, { Energy = 0; Barrier = 3; BarrierStackReduction = 3; StartBarrier = 0; Entropy = 0 }
                        Weak, { Energy = 3; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 0; Entropy = 0 }
                    ]
                PeelBarrier = Double.PositiveInfinity   // no peeling
            }
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
            {
                Temperature = 1.0
                BondParameterMap =
                    Map [
                        Strong, { Energy = 2; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 0; Entropy = strongEntropy }
                        Weak, { Energy = 3; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 0; Entropy = weakEntropy }
                    ]
                PeelBarrier = Double.PositiveInfinity   // no peeling
            }
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
            World.empty
                |> Chain.addBox size
                |> addAtoms 8
        let hot =
            TestParameters.create hotKT
                |> TestParameters.withStartBarrier hotKT (hotKT / 2.0)
        let stacked =
            {
                Temperature = 1.0
                BondParameterMap =
                    Map [
                        Strong, { Energy = 0; Barrier = 3; BarrierStackReduction = 5; StartBarrier = 1.5; Entropy = 0 }
                        Weak, { Energy = 3; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 0.5; Entropy = 0 }
                    ]
                PeelBarrier = Double.PositiveInfinity   // no peeling
            }
            // strands start slowly but then grow freely, so most strong bonds
            // formed extend a strand and must not get the start barrier
        let growing =
            {
                Temperature = 1.0
                BondParameterMap =
                    Map [
                        Strong, { Energy = 2; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 2; Entropy = 0 }
                        Weak, { Energy = 1; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 0; Entropy = 0 }
                    ]
                PeelBarrier = Double.PositiveInfinity   // no peeling
            }
        inParallel [|
            fun () -> checkEventRates "Box, strands growing" growing 150000 (Random(9)) box
            fun () -> checkEventRates "Box, with start barriers" hot 150000 (Random(4)) box
            for i, (label, world) in List.indexed stackingWorlds do
                fun () ->
                    checkEventRates
                        $"{label}, with start barriers" stacked 200000 (Random(5 + i)) world
        |]

    /// Strong bonds, at the right rates: eight atoms of random types
    /// are sealed in a box, at a temperature high enough that strong
    /// bonds form and break constantly. Chains, branches of weak bonds,
    /// and rings (including triangles) all come and go, so this covers
    /// the barrier, the strong bond vectors, valence, crossing, and
    /// breaking one bond of an atom that has two.
    ///
    /// Run once without peeling and once with it. With peeling, bonds
    /// re-form as fast as they peel, so there are enough peeling events
    /// to pin down the peeling probability.
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
            World.empty
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
            {
                Temperature = 1.0
                BondParameterMap =
                    Map [
                        Strong, { Energy = 30; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 0; Entropy = 0 }
                        Weak, { Energy = 1; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 0; Entropy = 0 }
                    ]
                PeelBarrier = 1.0   // peels with probability 1/e
            }
        inParallel [|
            fun () -> checkEventRates "Box" parms 100000 (Random(1)) world
            fun () ->
                checkEventRates
                    "Box, with peeling" peeling 200000 (Random(2)) worldNoCaps
        |]

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
