namespace Ecos

/// Parameters used by the tests.
module TestParameters =

    /// Standard bond parameters. Most tests use these, and the exact
    /// tests' temperatures are chosen to suit them.
    let standardBondParameterMap =
        Map [
            Strong, { Energy = 12; PairEnergyMap = Map.empty; Barrier = 6; BarrierStackReduction = 0; StartBarrier = 0; Entropy = 0 }
            Weak, { Energy = 1; PairEnergyMap = Map.empty; Barrier = 0; BarrierStackReduction = 0; StartBarrier = 0; Entropy = 0 }
        ]

    /// Standard parameters at the given temperature.
    let create kT =
        {
            Temperature = kT
            BondParameterMap = standardBondParameterMap
            PeelBarrier = System.Double.PositiveInfinity   // no peeling
            CapPeelBarrier = System.Double.PositiveInfinity   // no peeling
        }

    /// Standard parameters at the given temperature, except that
    /// strong bonds can never form or break: their barrier is
    /// impassable. For tests whose exact enumeration treats strong
    /// bonds as fixed.
    let createFrozen kT =
        let strong =
            { standardBondParameterMap[Strong] with
                Barrier = System.Double.PositiveInfinity }
        {
            Temperature = kT
            BondParameterMap =
                Map.add Strong strong standardBondParameterMap
            PeelBarrier = System.Double.PositiveInfinity   // no peeling
            CapPeelBarrier = System.Double.PositiveInfinity   // no peeling
        }

    /// The given parameters, with the given start barrier for each bond
    /// type.
    let withStartBarrier strongBarrier weakBarrier (parms : Parameters) =
        let set bondType barrier (map : Map<BondType, BondTypeParameters>) =
            Map.add bondType { map[bondType] with StartBarrier = barrier } map
        { parms with
            BondParameterMap =
                parms.BondParameterMap
                    |> set Strong strongBarrier
                    |> set Weak weakBarrier }

    /// The given parameters, with the given entropy for each bond type.
    let withEntropy strongEntropy weakEntropy (parms : Parameters) =
        let set bondType entropy (map : Map<BondType, BondTypeParameters>) =
            Map.add bondType { map[bondType] with Entropy = entropy } map
        { parms with
            BondParameterMap =
                parms.BondParameterMap
                    |> set Strong strongEntropy
                    |> set Weak weakEntropy }

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
            |> Map.tryFind id
            |> Option.defaultValue Set.empty

    /// Are the given atoms weakly bonded?
    let areWeaklyBonded (world : World) idA idB =
        (weakPartners world idA).Contains idB

    /// Are the given atoms strongly bonded?
    let areStronglyBonded (world : World) idA idB =
        strongBondMap world
            |> Map.tryFind idA
            |> Option.exists (Set.contains idB)

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

    /// Oracle: is the angle between an atom's two bonds of the given
    /// type wide enough, when the atom is at the given center and its
    /// partners are at locations a and b? The rule depends on the bond
    /// type:
    /// * Strong: a strand can't bend sharply. The cosine of the angle
    ///   between the two vectors from the center must be at most -0.8,
    ///   whatever their lengths, so the strand bends by at most about
    ///   37 degrees at each atom.
    /// * Weak: an atom's two weak partners are on roughly opposite
    ///   sides of it (about 125 degrees or more apart), which is also
    ///   too far apart for them to be strongly bonded to each other.
    ///   The dot product of the two vectors must be at most -3.
    let isAngleAllowed bondType (center : Point) (a : Point) (b : Point) =
        let da = a - center
        let db = b - center
        let dot = da.X * db.X + da.Y * db.Y
        match bondType with
            | Strong ->
                let length (d : Point) =
                    sqrt (float (d.X * d.X + d.Y * d.Y))
                let cosine = float dot / (length da * length db)
                cosine <= -0.8 + 1e-9   // some angles are exactly at the limit
            | Weak -> dot <= -3

    /// Oracle: are locations a and b on roughly opposite sides of the
    /// given center? An atom's two weak partners must be.
    let areOpposite center a b =
        isAngleAllowed Weak center a b

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

    /// Oracle: energy of one bond of the given type, with the given
    /// parameters. (World's parameters give the depth of the bond as a
    /// positive number, so the energy is its negative.)
    let bondEnergy (parms : Parameters) bondType =
        -float parms.BondParameterMap[bondType].Energy

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
        let bondParms = parms.BondParameterMap[bondType]
        let barrier =
            bondParms.Barrier - bondParms.BarrierStackReduction * float nStacks
                |> max 0.0
        if isStart then barrier + bondParms.StartBarrier
        else barrier

    /// Oracle: entropy lost when a bond of the given type forms, with
    /// the given parameters. A bonded state counts as exp(-entropy)
    /// states, as if only that fraction of an atom's hidden internal
    /// positions allowed the bond. So a bond's weight is
    /// exp(-energy / kT - entropy), and above kT = -energy / entropy
    /// the bond is more likely broken than not.
    let bondEntropy (parms : Parameters) bondType =
        parms.BondParameterMap[bondType].Entropy

    /// Oracle: barrier to peeling a bond of the given type from an
    /// exposed end of a strand. Only strong bonds peel.
    let peelBarrier (parms : Parameters) bondType =
        match bondType with
            | Strong -> parms.PeelBarrier
            | Weak -> Double.PositiveInfinity

    /// Oracle: barrier to a cap coming off the strand it ends. A strong
    /// bond between a cap and the atom it caps breaks with probability
    /// exp(-barrier / kT), whatever its energy, after which the strand
    /// has an exposed end and peels. Only strong bonds have caps.
    let capPeelBarrier (parms : Parameters) bondType =
        match bondType with
            | Strong -> parms.CapPeelBarrier
            | Weak -> Double.PositiveInfinity

    /// Oracle: energy of one weak bond, with the standard parameters.
    let weakBondEnergy =
        bondEnergy (TestParameters.create 1.0) Weak

    /// Oracle: energy of one strong bond, with the standard parameters.
    let strongBondEnergy =
        bondEnergy (TestParameters.create 1.0) Strong

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
                |> Map.tryFind id
                |> Option.defaultValue Set.empty
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
            (World.empty, world.Walls)
                ||> Array.fold (fun rebuilt loc -> World.addWall loc rebuilt)
        let rebuilt =
            (rebuilt, world.Atoms)
                ||> Array.fold (fun rebuilt atom ->
                    World.addAtom types[atom.Id] atom.Location rebuilt)
        (rebuilt, Bonds.strongBonds world)
            ||> Seq.fold (fun rebuilt (idA, idB) ->
                World.addStrongBond idA idB rebuilt)

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
                        if id > 0 then World.tryAddStrongBond (id - 1) id world
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
            World.addAtomW Point.zero World.empty
        match extend 1000 start with
            | Some world -> world
            | None -> grow rng n

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
                                            |> World.tryAddStrongBond atomA.Id atomB.Id))
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

    /// Temperature for tests of the athermal model. Atoms created by
    /// the Atom.create helper above are all type W, which can't form
    /// weak bonds, so hops and weak bond toggles never change the
    /// energy, and the temperature doesn't affect them. It's set very
    /// low so that strong bonds, which could otherwise form between a
    /// chain's ends or break, are frozen: the barrier is never crossed.
    let athermalKT = 0.1

    /// Performs one sweep with the standard parameters at the given
    /// temperature: N updates for N atoms.
    let sweep kT rng (world : World) =
        Ecos.World.sweep (TestParameters.create kT) rng 1 world

    /// Performs about the given number of updates with the given
    /// parameters, as whole sweeps (rounding up). Much faster than
    /// updating one at a time, which copies the world each time.
    let updatesWith parms rng nUpdates (world : World) =
        let nAtoms = world.Atoms.Length
        Ecos.World.sweep parms rng ((nUpdates + nAtoms - 1) / nAtoms) world

    /// Performs the given number of sweeps with the given parameters.
    let sweepsWith parms rng count (world : World) =
        Ecos.World.sweep parms rng count world

    /// Performs the given number of sweeps with the standard
    /// parameters at the given temperature.
    let sweeps kT rng count world =
        sweepsWith (TestParameters.create kT) rng count world

    /// Atom locations as floating-point (x, y) pairs, in ID order.
    let private locations (world : World) =
        world.Atoms
            |> Seq.map (fun atom ->
                float atom.Location.X, float atom.Location.Y)
            |> Seq.toArray

    /// Center of mass of all atoms.
    let centerOfMass world =
        let locs = locations world
        Array.averageBy fst locs, Array.averageBy snd locs

    /// Squared distance between the chain's first and last atoms.
    let endToEndSquared (world : World) =
        let first = world.Atoms[0].Location
        let last = world.Atoms[world.Atoms.Length - 1].Location
        let d = last - first
        float (d.X * d.X + d.Y * d.Y)

    /// Squared radius of gyration: mean squared distance of atoms
    /// from the center of mass.
    let radiusOfGyrationSquared world =
        let xc, yc = centerOfMass world
        locations world
            |> Array.averageBy (fun (x, y) ->
                (x - xc) ** 2.0 + (y - yc) ** 2.0)

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
