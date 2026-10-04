namespace Ecos

module Atom =

    /// Helper for creating monotype atoms. Type W can't pair with
    /// itself, so worlds built from these atoms never form weak bonds.
    let create loc =
        Ecos.Atom.create AtomType.W loc

module World =

    /// Helper function for updating a world. Ignores event.
    let update kT rng world =
        Ecos.World.update kT rng world |> fst

namespace Ecos.Test

open System
open Ecos

/// Views of a world's bonds.
module Bonds =

    /// Strong bonds, mapping each bonded atom to its partners.
    let strongBondMap (world : World) =
        world.BondTypeMap[BondType.Strong]

    /// Weak bonds, mapping each bonded atom to its partners (at most
    /// one each, which isValid checks).
    let weakBondSets (world : World) =
        world.BondTypeMap[BondType.Weak]

    /// Weak bonds, mapping each bonded atom to its partner. Only
    /// meaningful if each atom has at most one weak bond.
    let weakBondMap (world : World) : Map<AtomId, AtomId> =
        weakBondSets world
            |> Map.map (fun _ ids -> Seq.head ids)

    /// The given atom's weak bond partner, if any.
    let weakPartner (world : World) id =
        weakBondSets world
            |> Map.tryFind id
            |> Option.map Seq.head

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

    /// Oracle: can atoms of the given types form a weak bond?
    let canPair typeA typeB =
        match typeA, typeB with
            | AtomType.W, AtomType.X | AtomType.X, AtomType.W
            | AtomType.Y, AtomType.Z | AtomType.Z, AtomType.Y -> true
            | _ -> false

    /// Oracle: energy of one weak bond.
    let weakBondEnergy = -1.0

    /// Oracle: energy of one stack, i.e. two weak bonds A-B and A'-B'
    /// where A and A' are strongly bonded, and so are B and B'.
    let stackEnergy = -1.0

    /// Oracle: energy of one strong bond.
    let strongBondEnergy = -12.0

    /// Oracle: barrier to forming or breaking a strong bond. Either
    /// event is first accepted with probability exp(-barrier / kT), and
    /// then with the usual Metropolis probability. (Weak bonds have no
    /// barrier.)
    let strongBondBarrier = 6.0

    /// Gives every atom in the given world a random type. Only valid
    /// for a world without weak bonds, since types determine which
    /// weak bonds are allowed.
    let withRandomTypes (rng : Random) (world : World) =
        assert (Bonds.weakBondMap world).IsEmpty
        let types = [| AtomType.W; AtomType.X; AtomType.Y; AtomType.Z |]
        {
            world with
                AtomMap =
                    world.AtomMap
                        |> Map.map (fun _ atom ->
                            { atom with Type = types[rng.Next types.Length] })
        }

    /// Tries to append atoms at the given locations to the end of the
    /// chain in the given world, bonding each to its predecessor. Fails
    /// if an atom would overlap another, or a bond would cross another.
    let private tryAppend (locs : Point[]) (world : World) =
        let n0 = world.AtomMap.Count
        (Some world, Array.indexed locs)
            ||> Array.fold (fun worldOpt (i, loc) ->
                worldOpt
                    |> Option.bind (World.tryAddAtom (Atom.create loc))
                    |> Option.bind (fun world ->
                        let id = n0 + i
                        if id > 0 then World.tryAddStrongBond (id - 1) id world
                        else Some world))

    /// Atom locations in ID order.
    let private locationsOf (world : World) =
        world.AtomMap
            |> Map.toArray
            |> Array.map (fun (_, atom) -> atom.Location)

    /// Grows a random linear chain of n atoms, sampled uniformly from
    /// all valid conformations (no overlapping atoms and no crossed
    /// bonds). This is the equilibrium ensemble of the athermal model,
    /// so the chain needs no further equilibration.
    ///
    /// Short chains use simple sampling: pick each bond uniformly and
    /// start over on any overlap or crossing. That gets exponentially
    /// slow as n grows, so longer chains use dimerization: join two
    /// independent uniform half-chains with a random bond, and start
    /// over (both halves) on any overlap or crossing. Both methods are
    /// exact.
    let rec grow (rng : Random) n : World =
        if n <= 8 then
            let rec simple () =
                let locs =
                    Array.scan
                        (fun loc _ -> loc + bondVectors[rng.Next bondVectors.Length])
                        (Point.create (0, 0))
                        (Array.zeroCreate<unit> (n - 1))
                match tryAppend locs World.empty with
                    | Some world -> world
                    | None -> simple ()
            simple ()
        else
            let left = grow rng (n / 2)
            let right = locationsOf (grow rng (n - n / 2))
            let last = left.AtomMap[left.AtomMap.Count - 1].Location
            let offset =
                last + bondVectors[rng.Next bondVectors.Length] - right[0]
            match tryAppend (right |> Array.map ((+) offset)) left with
                | Some world -> world
                | None -> grow rng n

    /// Adds n free (unbonded) atoms of random types at random
    /// locations near the origin, packed closely enough that weak
    /// bonds form and break often.
    let addSoup (rng : Random) n (world : World) =
        let types = [| AtomType.W; AtomType.X; AtomType.Y; AtomType.Z |]
        let size = 2 + int (ceil (1.5 * sqrt (float n)))   // half-width of the box
        let rec loop count attempts world =
            if count = 0 then world
            elif attempts > 100000 then failwith "Soup box is too full"
            else
                let loc = Point.create (rng.Next(-size, size + 1), rng.Next(-size, size + 1))
                let atom = { Type = types[rng.Next types.Length]; Location = loc }
                match World.tryAddAtom atom world with
                    | Some world -> loop (count - 1) (attempts + 1) world
                    | None -> loop count (attempts + 1) world
        loop n 0 world

    /// Adds n dimers (two strongly bonded atoms) of random types at
    /// random locations near the origin. Dimers that pair with each
    /// other form stacks, while a dimer whose atoms pair with two
    /// unrelated atoms doesn't, so these exercise both cases.
    let addDimers (rng : Random) n (world : World) =
        let types = [| AtomType.W; AtomType.X; AtomType.Y; AtomType.Z |]
        let size = 4 + int (ceil (2.0 * sqrt (float n)))   // half-width of the box
        let rec loop count attempts world =
            if count = 0 then world
            elif attempts > 100000 then failwith "Dimer box is too full"
            else
                let locA = Point.create (rng.Next(-size, size + 1), rng.Next(-size, size + 1))
                let locB = locA + bondVectors[rng.Next bondVectors.Length]
                let atom loc = { Type = types[rng.Next types.Length]; Location = loc }
                let idA = world.AtomMap.Count
                let worldOpt =
                    Some world
                        |> Option.bind (World.tryAddAtom (atom locA))
                        |> Option.bind (World.tryAddAtom (atom locB))
                        |> Option.bind (World.tryAddStrongBond idA (idA + 1))
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

    /// Performs one Monte Carlo sweep at the given temperature: N
    /// attempted moves for N atoms.
    let sweep kT rng (world : World) =
        let mutable world = world
        for _ in 1 .. world.AtomMap.Count do
            world <- World.update kT rng world
        world

    /// Performs the given number of sweeps at the given temperature.
    let sweeps kT rng count world =
        let mutable world = world
        for _ in 1 .. count do
            world <- sweep kT rng world
        world

    /// Atom locations as floating-point (x, y) pairs, in ID order.
    let private locations (world : World) =
        world.AtomMap
            |> Map.toArray
            |> Array.map (fun (_, atom) ->
                float atom.Location.X, float atom.Location.Y)

    /// Center of mass of all atoms.
    let centerOfMass world =
        let locs = locations world
        Array.averageBy fst locs, Array.averageBy snd locs

    /// Squared distance between the chain's first and last atoms.
    let endToEndSquared (world : World) =
        let first = world.AtomMap[0].Location
        let last = world.AtomMap[world.AtomMap.Count - 1].Location
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
