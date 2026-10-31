namespace Ecos

/// Worlds used by the tests.
module TestWorld =

    /// An empty torus of the given (even) size.
    let create size = World.createSquare size

    /// An empty world for tests: a small torus, so that the many tests
    /// that update a world one step at a time (each copying the world)
    /// stay fast. Their worlds are boxed, or local to the origin, so
    /// nothing reaches around it.
    let empty = create 32

/// Parameters used by the tests.
module TestParameters =

    /// Bend limit that keeps strands nearly straight: they bend by at
    /// most about 37 degrees at each atom.
    let straightBend = Rational.create -4 5

    /// Bend limit that allows strands to bend up to a right angle at
    /// each atom, as in the standard scenarios.
    let rightAngleBend = Rational.create 0 1

    /// Bend limit that allows any angle, apart from the bond type's own
    /// rules.
    let anyBend = Rational.create 1 1

    /// Parameters at the given temperature with nothing switched on:
    /// bonds of both types have no energy, barrier, entropy or
    /// hydrolysis, strands stay nearly straight at no cost, and nothing
    /// peels. Tests add what they need with the functions below, so each
    /// states only what it changes.
    let basic kT =
        let bondTypeParms bendLimit =
            {
                Energy = 0
                PairEnergyMap = Map.empty
                Barrier = 0
                BarrierStackReduction = 0
                StartBarrier = 0
                Entropy = 0
                HydrolysisRate = 0
                ProtectedHydrolysisRate = 0
                BendLimit = bendLimit
                BendStiffness = 0
            }
        {
            GetTemperature = Parameters.getUniformTemperature kT
            BondTypeParameterMap =
                Map [
                    Strong, bondTypeParms straightBend
                    Weak, bondTypeParms anyBend
                ] |> BondTypeParameterMap.ofMap
            PeelBarrier = System.Double.PositiveInfinity
        }

    /// The given parameters, with the given bond type's changed by the
    /// given function.
    let withBondType bondType f (parms : Parameters) =
        { parms with
            BondTypeParameterMap =
                BondTypeParameterMap.add
                    bondType (f parms.BondTypeParameterMap[bondType])
                    parms.BondTypeParameterMap }

    /// The given parameters, with strong bonds' changed by the given
    /// function.
    let withStrong f parms = withBondType Strong f parms

    /// The given parameters, with weak bonds' changed by the given
    /// function.
    let withWeak f parms = withBondType Weak f parms

    /// The given parameters, with the given barrier to peeling.
    let withPeelBarrier barrier (parms : Parameters) =
        { parms with PeelBarrier = barrier }

    /// The given parameters, with the given temperature at each location.
    let withTemperature getTemperature (parms : Parameters) =
        { parms with GetTemperature = getTemperature }

    /// Standard parameters at the given temperature. Most tests use
    /// these, and the exact tests' temperatures are chosen to suit them.
    let create kT =
        basic kT
            |> withStrong (fun p -> { p with Energy = 12; Barrier = 6 })
            |> withWeak (fun p -> { p with Energy = 1 })

    /// Standard parameters, for building worlds by hand. Building
    /// doesn't depend on temperature, so it is NaN.
    let standard = create nan

    /// Standard parameters at the given temperature, except that
    /// strong bonds can never form or break: their barrier is
    /// impassable. For tests whose exact enumeration treats strong
    /// bonds as fixed.
    let createFrozen kT =
        create kT
            |> withStrong (fun p -> { p with Barrier = System.Double.PositiveInfinity })

    /// The given parameters, with the given start barrier for each bond
    /// type.
    let withStartBarrier strongBarrier weakBarrier parms =
        parms
            |> withStrong (fun p -> { p with StartBarrier = strongBarrier })
            |> withWeak (fun p -> { p with StartBarrier = weakBarrier })

    /// The given parameters, with the given entropy for each bond type.
    let withEntropy strongEntropy weakEntropy parms =
        parms
            |> withStrong (fun p -> { p with Entropy = strongEntropy })
            |> withWeak (fun p -> { p with Entropy = weakEntropy })

    /// The given parameters, with the given hydrolysis rate for all
    /// bonds of the given type, protected or not.
    let withHydrolysis bondType rate parms =
        parms
            |> withBondType bondType (fun p ->
                { p with HydrolysisRate = rate; ProtectedHydrolysisRate = rate })

    /// The given parameters, with the given hydrolysis rate for
    /// protected bonds of the given type.
    let withProtectedHydrolysis bondType rate parms =
        parms
            |> withBondType bondType (fun p ->
                { p with ProtectedHydrolysisRate = rate })

    /// The given parameters, with the given bend limit and stiffness
    /// for the given bond type.
    let withBend bondType bendLimit kappa parms =
        parms
            |> withBondType bondType (fun p ->
                { p with BendLimit = bendLimit; BendStiffness = kappa })

    /// The given parameters, with energies for particular pairs of atom
    /// types.
    let withPairEnergies bondType (pairs : ((AtomType * AtomType) * float) list) parms =
        parms
            |> withBondType bondType (fun p ->
                (p, pairs)
                    ||> List.fold (fun p ((typeA, typeB), energy) ->
                        BondTypeParameters.addPairEnergy typeA typeB energy p))

module World =

    /// Helper for creating monotype atoms. Type W can't pair with
    /// itself, so worlds built from these atoms never form weak bonds.
    let tryAddAtomW loc world =
        world
            |> World.tryAddAtom AtomType.W loc
            |> Option.map fst

    /// Helper for creating monotype atoms. Type W can't pair with
    /// itself, so worlds built from these atoms never form weak bonds.
    let addAtomW loc world =
        world
            |> World.addAtom AtomType.W loc

    /// Helper function for updating a world with the given
    /// parameters. Ignores event.
    let updateWith parms rng world =
        World.update parms rng world |> fst

    /// Helper function for updating a world with standard parameters
    /// at the given temperature. Ignores event.
    let update kT rng world =
        updateWith (TestParameters.create kT) rng world

namespace Ecos.Test

open System
open Ecos

/// Views of a world's bonds.
module Bonds =

    /// Strong bonds, mapping each bonded atom to its partners.
    let strongBondMap (world : World) =
        world.BondTypeMap[BondType.Strong]

    /// Weak bonds, mapping each bonded atom to its partners.
    let weakBondMap (world : World) =
        world.BondTypeMap[BondType.Weak]

    /// The given atom's weak bond partners.
    let weakPartners (world : World) id =
        weakBondMap world
            |> BondMap.getBonds id

    /// Are the given atoms weakly bonded?
    let areWeaklyBonded (world : World) idA idB =
        (weakPartners world idA).Contains idB

    /// Are the given atoms strongly bonded?
    let areStronglyBonded (world : World) idA idB =
        strongBondMap world
            |> BondMap.getBonds idA
            |> Set.contains idB

    /// The bonds in the given bond map, as (smaller ID, larger ID)
    /// pairs.
    let private toPairs (bondMap : BondMap) =
        set [
            for KeyValue(idA, ids) in bondMap do
                for idB in ids do
                    if idA < idB then idA, idB
        ]

    /// Strong bonds, as (smaller ID, larger ID) pairs.
    let strongBonds world =
        toPairs (strongBondMap world)

    /// Weak bonds, as (smaller ID, larger ID) pairs.
    let weakBonds world =
        toPairs (weakBondMap world)

    /// Strong bonds, as (predecessor, successor) pairs: each runs from
    /// one atom of its strand to the next.
    let directedStrongBonds (world : World) =
        world.BondTypeMap.StrongBondDirectionMap
            |> Map.toSeq
            |> set

    /// The given atom's successor in its strand, if any.
    let successor (world : World) id =
        Map.tryFind id world.BondTypeMap.StrongBondDirectionMap

    /// Cosine of the angle at the given center between the directions
    /// to locations a and b.
    let private cosine (center : Point) (a : Point) (b : Point) =
        let da = a - center
        let db = b - center
        let length (d : Point) = sqrt (float (d.X * d.X + d.Y * d.Y))
        float (da.X * db.X + da.Y * db.Y) / (length da * length db)

    /// Oracle: is the angle between an atom's two strong bonds within
    /// the given bend limit, when the atom is at the given center and
    /// its partners are at locations a and b? The cosine of the angle
    /// between the two vectors from the center must be at most the
    /// limit, whatever their lengths. (An atom has at most one weak
    /// partner, so weak bonds make no angles.)
    let isBendAllowed (limit : Rational) center a b =
        cosine center a b <= float limit.Numerator / float limit.Denominator + 1e-9   // some angles are exactly at the limit

    /// Oracle: direction of a strand at an atom at the given location,
    /// from its predecessor to its successor. At an end of the strand,
    /// the atom itself stands in for the missing neighbor, so the
    /// direction is that of its one bond.
    let strandDirection (predOpt : Point option) (loc : Point) (succOpt : Point option) =
        Option.defaultValue loc succOpt - Option.defaultValue loc predOpt

    /// Oracle: is a partner at the given displacement from an atom in
    /// a strand on the atom's face, when the strand runs in the given
    /// direction there? An atom pairs only on one side of its strand:
    /// to the left of the strand's direction, which is where the cross
    /// product of the direction and the displacement is positive. (So a
    /// strand running toward +x has its face toward +y.)
    let isOnFace (direction : Point) (d : Point) =
        direction.X * d.Y - direction.Y * d.X > 0

    /// Oracle: energy of the bend at an atom at the given center, whose
    /// partners are at locations a and b: kappa * (1 + cos phi), where
    /// phi is the angle between the two vectors from the center.
    let bendEnergy kappa center a b =
        kappa * (1.0 + cosine center a b)

/// Geometry helpers.
module Geometry =

    /// Sign of the cross product (b - a) x (c - a).
    let private orientation (a : Point) (b : Point) (c : Point) =
        sign ((b.X - a.X) * (c.Y - a.Y) - (b.Y - a.Y) * (c.X - a.X))

    /// Is point p within the bounding box of segment ab?
    let private inBox (a : Point) (b : Point) (p : Point) =
        min a.X b.X <= p.X && p.X <= max a.X b.X
            && min a.Y b.Y <= p.Y && p.Y <= max a.Y b.Y

    /// Oracle: do closed segments ab and cd intersect (including
    /// touching)?
    let segmentsIntersect a b c d =
        let o1 = orientation a b c
        let o2 = orientation a b d
        let o3 = orientation c d a
        let o4 = orientation c d b
        (o1 * o2 < 0 && o3 * o4 < 0)
            || (o1 = 0 && inBox a b c)
            || (o2 = 0 && inBox a b d)
            || (o3 = 0 && inBox c d a)
            || (o4 = 0 && inBox c d b)

/// Helpers for simulating and measuring a single linear chain.
module Chain =

    /// All allowed bond vectors in the 2D bond fluctuation model
    /// (Carmesin & Kremer 1988): integer vectors b with 4 <= |b|^2 <= 13.
    /// This is an independent statement of the rule that
    /// World.tryAddStrongBond enforces, so tests can use it as an oracle.
    let bondVectors =
        [|
            for x in -3 .. 3 do
                for y in -3 .. 3 do
                    let d2 = x * x + y * y
                    if d2 >= 4 && d2 <= 13 then
                        Point.create (x, y)
        |]

    /// Allowed weak bond vectors: the closest separations at which two
    /// atoms don't overlap, i.e. 4 <= |b|^2 <= 8. An independent
    /// statement of the range World.tryAddWeakBond enforces.
    let weakBondVectors =
        bondVectors
            |> Array.filter (fun v -> v.X * v.X + v.Y * v.Y <= 8)

    /// Oracle: can atoms of the given types form a weak bond? Each
    /// type pairs with exactly one other.
    let canPair typeA typeB =
        match typeA, typeB with
            | AtomType.W, AtomType.X | AtomType.X, AtomType.W
            | AtomType.Y, AtomType.Z | AtomType.Z, AtomType.Y
            | AtomType.P, AtomType.Q | AtomType.Q, AtomType.P -> true
            | _ -> false

    /// Oracle: is the given type a cap? A cap ends a strand, because
    /// it can have only one strong bond.
    let isCap typ =
        typ = AtomType.P || typ = AtomType.Q

    /// Oracle: the most strong bonds an atom of the given type can
    /// have.
    let strongValence typ =
        if isCap typ then 1 else 2

    /// Oracle: can atoms of the given types form a strong bond? Any
    /// two can, except two caps.
    let canJoin typeA typeB =
        not (isCap typeA && isCap typeB)

    /// All atom types.
    let allTypes =
        [| AtomType.W; AtomType.X; AtomType.Y; AtomType.Z; AtomType.P; AtomType.Q |]

    /// Atom types that aren't caps.
    let codingTypes =
        [| AtomType.W; AtomType.X; AtomType.Y; AtomType.Z |]

    /// Oracle: temperature (kT) of a bond between the given atoms: the
    /// average of the temperatures at their two locations, so that
    /// forming and breaking the bond see the same temperature whichever
    /// atom the update starts from.
    let bondTemperature (parms : Parameters) (atomA : Atom) (atomB : Atom) =
        (parms.GetTemperature atomA.Location + parms.GetTemperature atomB.Location) / 2.0

    /// Oracle: energy of one bond of the given type between atoms of
    /// the given types, with the given parameters: the energy listed
    /// for that pair of types, in either order, or else the bond type's
    /// default. (World's parameters give the depth of the bond as a
    /// positive number, so the energy is its negative.)
    let bondEnergy (parms : Parameters) bondType typeA typeB =
        let bondTypeParms = parms.BondTypeParameterMap[bondType]
        let depth =
            match Map.tryFind (typeA, typeB) bondTypeParms.PairEnergyMap with
                | Some energy -> energy
                | None ->
                    match Map.tryFind (typeB, typeA) bondTypeParms.PairEnergyMap with
                        | Some energy -> energy
                        | None -> bondTypeParms.Energy
        -depth

    /// Oracle: barrier to forming or breaking a bond of the given type,
    /// with the given parameters, when the given number of stacks
    /// depend on that bond. Each such stack lowers the barrier, but not
    /// below zero. Then, if the bond starts a strand (see below), the
    /// bond type's start barrier is added. Either event is first
    /// accepted with probability exp(-barrier / kT), and then with the
    /// usual Metropolis probability.
    ///
    /// A bond starts a strand if neither of its atoms has any other
    /// bond of the same type. That describes both forming a bond
    /// between two atoms that have none, and breaking a bond that is
    /// the only one either atom has, so the start barrier slows both
    /// equally and doesn't change which state is favored.
    let bondBarrier (parms : Parameters) bondType nStacks isStart =
        let bondTypeParms = parms.BondTypeParameterMap[bondType]
        let barrier =
            bondTypeParms.Barrier - bondTypeParms.BarrierStackReduction * float nStacks
                |> max 0.0
        if isStart then barrier + bondTypeParms.StartBarrier
        else barrier

    /// Oracle: entropy lost when a bond of the given type forms, with
    /// the given parameters. A bonded state counts as exp(-entropy)
    /// states, as if only that fraction of an atom's hidden internal
    /// positions allowed the bond. So a bond's weight is
    /// exp(-energy / kT - entropy), and above kT = -energy / entropy
    /// the bond is more likely broken than not.
    let bondEntropy (parms : Parameters) bondType =
        parms.BondTypeParameterMap[bondType].Entropy

    /// Oracle: barrier to peeling a bond of the given type from an
    /// exposed end of a strand. Only strong bonds peel.
    let peelBarrier (parms : Parameters) bondType =
        match bondType with
            | Strong -> parms.PeelBarrier
            | Weak -> Double.PositiveInfinity

    /// Oracle: probability that an attempt to break a bond of the given
    /// type, between atoms with the given IDs, succeeds by hydrolysis,
    /// whatever its energy or position. A strong bond is protected when
    /// it is one side of a stack: A and B are weakly paired with atoms
    /// A' and B' that are strongly bonded to each other, so that the
    /// bond lies in a double-stranded stretch. It then hydrolyzes at its
    /// type's protected rate; any other bond, a cap's included, at its
    /// type's own rate. (Two atoms that are merely paired, with free
    /// atoms say, aren't protected.)
    let hydrolysisRate (parms : Parameters) bondType (world : World) idA idB =
        let bondTypeParms = parms.BondTypeParameterMap[bondType]
        let isStacked =
            Bonds.weakPartners world idA
                |> Set.exists (fun idA' ->
                    Bonds.weakPartners world idB
                        |> Set.exists (fun idB' ->
                            Bonds.areStronglyBonded world idA' idB'))
        if bondType = Strong && isStacked then
            bondTypeParms.ProtectedHydrolysisRate
        else bondTypeParms.HydrolysisRate

    /// Oracle: energy of one weak bond between W and X, with the
    /// standard parameters.
    let weakBondEnergy =
        bondEnergy (TestParameters.create 1.0) Weak AtomType.W AtomType.X

    /// Oracle: energy of one strong bond between W and X, with the
    /// standard parameters.
    let strongBondEnergy =
        bondEnergy (TestParameters.create 1.0) Strong AtomType.W AtomType.X

    /// Oracle: energy of one stack, i.e. two weak bonds A-B and A'-B'
    /// where A and A' are strongly bonded, and so are B and B'.
    let stackEnergy = -1.0

    /// Gives every atom in the given world a random type. An atom
    /// with two strong bonds is never made a cap, and neither is an
    /// atom strongly bonded to a cap, since those would be invalid.
    /// So caps appear at some ends of strands, and among free atoms.
    ///
    /// Only valid for a world without weak bonds, since types determine
    /// which weak bonds are allowed.
    let withRandomTypes (rng : Random) (world : World) =
        assert (Bonds.weakBondMap world).IsEmpty
        let strongBondMap = Bonds.strongBondMap world
        let strongPartners id =
            strongBondMap
                |> BondMap.getBonds id
        let pick (types : AtomType[]) = types[rng.Next types.Length]

            // choose types in ID order
        let types = Array.zeroCreate<AtomType> world.Atoms.Length
        for atom in world.Atoms do
            let id = atom.Id
            let partners = strongPartners id
            let canBeCap =
                partners.Count < 2
                    && partners
                        |> Set.forall (fun idPartner ->
                            idPartner > id   // not yet assigned
                                || not (isCap types[idPartner]))
            types[id] <- if canBeCap then pick allTypes else pick codingTypes

            // a world can't change an atom's type, so build a new one:
            // the same walls, then the same atoms (so with the same IDs)
            // with their new types, then the same strong bonds
        let rebuilt =
            (World.create world.Size, world.Walls)
                ||> Array.fold (fun rebuilt loc -> World.addWall loc rebuilt)
        let rebuilt =
            (rebuilt, world.Atoms)
                ||> Array.fold (fun rebuilt atom ->
                    World.addAtom types[atom.Id] atom.Location rebuilt)
        (rebuilt, Bonds.directedStrongBonds world)
            ||> Seq.fold (fun rebuilt (idPred, idSucc) ->
                World.addStrongBond TestParameters.standard idPred idSucc rebuilt)

    /// Tries to append atoms at the given locations to the end of the
    /// chain in the given world, bonding each to its predecessor. Fails
    /// if an atom would overlap another, or a bond would cross another.
    let private tryAppend (locs : Point[]) (world : World) =
        let n0 = world.Atoms.Length
        (Some world, Array.indexed locs)
            ||> Array.fold (fun worldOpt (i, loc) ->
                worldOpt
                    |> Option.bind (World.tryAddAtomW loc)
                    |> Option.bind (fun world ->
                        let id = n0 + i
                        if id > 0 then World.tryAddStrongBond TestParameters.standard (id - 1) id world
                        else Some world))

    /// Grows a random linear chain of n atoms, one atom at a time,
    /// starting at the origin. Each atom is placed by trying random
    /// bond vectors from the previous atom until one is valid (no
    /// overlap, no crossed bond, and no bond angle too sharp), starting
    /// the whole chain over if it gets stuck.
    ///
    /// The result is a valid chain, but it isn't a uniform sample of
    /// all valid chains, since chains with more ways to continue are
    /// favored. So it's a starting point for tests of rules and rates,
    /// not a sample from equilibrium.
    let rec grow (rng : Random) n : World =
        let rec extend tries (world : World) =
            if world.Atoms.Length = n then Some world
            elif tries = 0 then None
            else
                let last = world.Atoms[world.Atoms.Length - 1].Location
                let loc = last + bondVectors[rng.Next bondVectors.Length]
                match tryAppend [| loc |] world with
                    | Some world -> extend 1000 world
                    | None -> extend (tries - 1) world
        let start =
            World.addAtomW Point.zero (TestWorld.create (max 32 (6 * n + 2 * (n % 2))))   // room enough that the chain never meets itself around the torus
        match extend 1000 start with
            | Some world -> world
            | None -> grow rng n

    /// Adds a capped template strand to the given world, which must have
    /// no atoms yet: P, then six coding atoms, each a random W or Y, then
    /// Q, in a row along the x axis from -8 to 6, strongly bonded in
    /// order (atoms 0 to 7). So it runs toward +x, and its face is
    /// toward +y.
    let addTemplate (rng : Random) (world : World) =
        assert(world.Atoms.Length = 0)
        let types =
            [|
                AtomType.P
                for _ in 1 .. 6 do
                    if rng.Next(2) = 0 then AtomType.W else AtomType.Y
                AtomType.Q
            |]
        (world, Array.indexed types)
            ||> Array.fold (fun world (i, atomType) ->
                let world = World.addAtom atomType (Point.create (2 * i - 8, 0)) world
                if i > 0 then World.addStrongBond TestParameters.standard (i - 1) i world
                else world)

    /// Adds n free (unbonded) atoms of random types at random
    /// locations near the origin, packed closely enough that weak
    /// bonds form and break often.
    let addSoup (rng : Random) n (world : World) =
        let types = allTypes
        let size = 2 + int (ceil (1.5 * sqrt (float n)))   // half-width of the box
        let rec loop count attempts world =
            if count = 0 then world
            elif attempts > 100000 then failwith "Soup box is too full"
            else
                let loc = Point.create (rng.Next(-size, size + 1), rng.Next(-size, size + 1))
                let atomType = types[rng.Next types.Length]
                match Ecos.World.tryAddAtom atomType loc world with
                    | Some (world, _) -> loop (count - 1) (attempts + 1) world
                    | None -> loop count (attempts + 1) world
        loop n 0 world

    /// Adds n dimers (two strongly bonded atoms) of random types at
    /// random locations near the origin. Dimers that pair with each
    /// other form stacks, while a dimer whose atoms pair with two
    /// unrelated atoms doesn't, so these exercise both cases.
    let addDimers (rng : Random) n (world : World) =
        let types = allTypes
        let size = 4 + int (ceil (2.0 * sqrt (float n)))   // half-width of the box
        let rec loop count attempts (world : World) =
            if count = 0 then world
            elif attempts > 100000 then failwith "Dimer box is too full"
            else
                let locA = Point.create (rng.Next(-size, size + 1), rng.Next(-size, size + 1))
                let locB = locA + bondVectors[rng.Next bondVectors.Length]
                let idA = world.Atoms.Length
                let worldOpt =
                    world
                        |> World.tryAddAtom types[rng.Next types.Length] locA
                        |> Option.bind (
                            fun (world, atomA) ->
                                world
                                    |> World.tryAddAtom types[rng.Next types.Length] locB
                                    |> Option.bind (fun (world, atomB) ->
                                        world
                                            |> World.tryAddStrongBond TestParameters.standard atomA.Id atomB.Id))
                match worldOpt with
                    | Some world -> loop (count - 1) (attempts + 1) world
                    | None -> loop count (attempts + 1) world
        loop n 0 world

    /// Scatters up to n wall blocks at random locations near the
    /// origin, among whatever is already there. Locations that overlap
    /// an atom or another wall are skipped.
    let addObstacles (rng : Random) n (world : World) =
        let size = 2 + int (ceil (1.5 * sqrt (float n)))   // half-width of the box
        (world, [ 1 .. n ])
            ||> List.fold (fun world _ ->
                let loc = Point.create (rng.Next(-size, size + 1), rng.Next(-size, size + 1))
                World.tryAddWall loc world
                    |> Option.defaultValue world)

    /// Encloses the square region of atom locations with |x| <= size
    /// and |y| <= size in a box of walls.
    let addBox size (world : World) =
        let edge = size + 2   // nearest a wall can be to an atom inside
        assert (edge % 2 = 0)   // so that walls spaced 2 apart meet at the corners
        (world, [ -edge .. 2 .. edge ])
            ||> List.fold (fun world i ->
                (world, [ i, -edge; i, edge; -edge, i; edge, i ])
                    ||> List.fold (fun world (x, y) ->
                        World.tryAddWall (Point.create (x, y)) world   // corners are attempted twice
                            |> Option.defaultValue world))

    /// Temperature for tests of the athermal model. Atoms added by
    /// World.addAtomW above are all type W, which can't form
    /// weak bonds, so hops and weak bond toggles never change the
    /// energy, and the temperature doesn't affect them. It's set very
    /// low so that strong bonds, which could otherwise form between a
    /// chain's ends or break, are frozen: the barrier is never crossed.
    let athermalKT = 0.1

    /// Performs about the given number of updates with the given
    /// parameters, as whole sweeps (rounding up). Much faster than
    /// updating one at a time, which copies the world each time.
    let updatesWith parms rng nUpdates (world : World) =
        let nAtoms = world.Atoms.Length
        Ecos.World.sweep parms rng ((nUpdates + nAtoms - 1) / nAtoms) world

    /// Performs the given number of sweeps with the given parameters.
    let sweepsWith parms rng count (world : World) =
        Ecos.World.sweep parms rng count world

    /// Atom locations as floating-point (x, y) pairs, in ID order, for a
    /// chain whose atoms are bonded in ID order. They are unwrapped: each
    /// is the one before it plus the displacement between them, so that
    /// a chain straddling the edges of the torus stays in one piece.
    let private locations (world : World) =
        let atoms = world.Atoms
        let locs = Array.zeroCreate atoms.Length
        for i in 0 .. atoms.Length - 1 do
            locs[i] <-
                if i = 0 then atoms[0].Location
                else
                    let delta =
                        World.getDisplacement atoms[i - 1] atoms[i] world
                    locs[i - 1] + delta
        locs |> Array.map (fun (loc : Point) -> float loc.X, float loc.Y)

    /// Center of mass of a chain whose atoms are bonded in ID order,
    /// unwrapped as above (so it is near the chain's first atom, which
    /// itself may be anywhere on the torus).
    let centerOfMass world =
        let locs = locations world
        Array.averageBy fst locs, Array.averageBy snd locs

/// Simple statistics helpers.
module Stats =

    /// Least-squares slope of y against x.
    let slope (points : seq<float * float>) =
        let points = Seq.toArray points
        let xm = Array.averageBy fst points
        let ym = Array.averageBy snd points
        let sxy = points |> Array.sumBy (fun (x, y) -> (x - xm) * (y - ym))
        let sxx = points |> Array.sumBy (fun (x, _) -> (x - xm) ** 2.0)
        sxy / sxx

    /// Least-squares slope on log-log axes, i.e. the exponent of a
    /// power law y ∝ x^slope.
    let logLogSlope (points : seq<float * float>) =
        points
            |> Seq.map (fun (x, y) -> log x, log y)
            |> slope
