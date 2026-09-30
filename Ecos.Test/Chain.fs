namespace Ecos

module Atom =

    /// Helper for creating monotype atoms.
    let create loc =
        Ecos.Atom.create Polar loc

namespace Ecos.Test

open System
open Ecos

/// Helpers for simulating and measuring a single linear chain.
module Chain =

    /// All allowed bond vectors in the 2D bond fluctuation model
    /// (Carmesin & Kremer 1988): integer vectors b with 4 <= |b|^2 <= 13.
    /// This is an independent statement of the rule that
    /// World.tryAddBond enforces, so tests can use it as an oracle.
    let bondVectors =
        [|
            for x in -3 .. 3 do
                for y in -3 .. 3 do
                    let d2 = x * x + y * y
                    if d2 >= 4 && d2 <= 13 then
                        Point.create (x, y)
        |]

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
                        if id > 0 then World.addBond (id - 1) id world
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

    /// Performs one Monte Carlo sweep: N attempted moves for N atoms.
    let sweep rng (world : World) =
        let mutable world = world
        for _ in 1 .. world.AtomMap.Count do
            world <- World.update rng world
        world

    /// Performs the given number of sweeps.
    let sweeps rng count world =
        let mutable world = world
        for _ in 1 .. count do
            world <- sweep rng world
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
