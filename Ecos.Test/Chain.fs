namespace Ecos

module Atom =

    /// Helper for creating monotype atoms. Type A can't pair with
    /// itself, so worlds built from these atoms never form weak bonds.
    let create loc =
        Ecos.Atom.create AtomType.A loc

namespace Ecos.Test

open System
open Ecos

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
            | AtomType.A, AtomType.B | AtomType.B, AtomType.A
            | AtomType.C, AtomType.D | AtomType.D, AtomType.C -> true
            | _ -> false

    /// Oracle: energy of one weak bond.
    let weakBondEnergy = -1.0

    /// Oracle: probability that World.update attempts a hop rather
    /// than a weak bond toggle.
    let hopProbability = 0.5

    /// Gives every atom in the given world a random type. Only valid
    /// for a world without weak bonds, since types determine which
    /// weak bonds are allowed.
    let withRandomTypes (rng : Random) (world : World) =
        assert world.WeakBondMap.IsEmpty
        let types = [| AtomType.A; AtomType.B; AtomType.C; AtomType.D |]
        {
            world with
                AtomMap =
                    world.AtomMap
                        |> Map.map (fun _ atom ->
                            { atom with Type = types[rng.Next types.Length] })
        }

    /// Tries to append atoms at the given locations to the end of the
    /// chain in the given world, bonding each to its predecessor.
    let private tryAppend (locs : Point[]) (world : World) =
        let n0 = world.AtomMap.Count
        (Some world, Array.indexed locs)
            ||> Array.fold (fun worldOpt (i, loc) ->
                worldOpt
                    |> Option.bind (World.tryAddAtom (Atom.create loc))
                    |> Option.map (fun world ->
                        let id = n0 + i
                        if id > 0 then World.addStrongBond (id - 1) id world
                        else world))

    /// Atom locations in ID order.
    let private locationsOf (world : World) =
        world.AtomMap
            |> Map.toArray
            |> Array.map (fun (_, atom) -> atom.Location)

    /// Grows a random linear chain of n atoms, sampled uniformly from
    /// all valid conformations. This is the equilibrium ensemble of the
    /// athermal model, so the chain needs no further equilibration.
    ///
    /// Short chains use simple sampling: pick each bond uniformly and
    /// start over on any overlap. That gets exponentially slow as n
    /// grows, so longer chains use dimerization: join two independent
    /// uniform half-chains with a random bond, and start over (both
    /// halves) on any overlap. Both methods are exact.
    ///
    /// Note that grown chains may contain crossed bonds, which the
    /// model's moves can neither create nor remove.
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
        let types = [| AtomType.A; AtomType.B; AtomType.C; AtomType.D |]
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

    /// Temperature for tests of the athermal model. Atoms created by
    /// the Atom.create helper above are all type A, which can't form
    /// weak bonds, so every event has zero energy change and the value
    /// doesn't matter.
    let athermalKT = 1.0

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
