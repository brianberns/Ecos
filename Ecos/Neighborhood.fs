namespace Ecos

open System.Collections.Generic

/// A set of non-negative integer pairs that describe normalized
/// offsets from a center. These are not necessarily (x, y)
/// points.
type Neighborhood = HashSet<int (*max*) * int (*min*)>   // use HashSet for speed

module Neighborhood =

    /// Creates a neighborhood.
    let create (pairs : seq<_>) : Neighborhood =
        HashSet(pairs)

    /// Indicates whether the given neighborhood is valid.
    let isValid (neighborhood : Neighborhood) =
        neighborhood
            |> Seq.forall (fun (x, y) ->
                x >= y && x >= 0 && y >= 0)

    /// Creates full (x, y) deltas from the given normalized (max,
    /// min) neighborhood.
    let toDeltas (neighborhood : Neighborhood) =
        neighborhood
            |> Seq.collect (fun (a, b) ->
                [|
                    (a, b)                            // e.g.  2,  1
                    if a <> 0 then (-a, b)            // e.g. -2,  1
                    if b <> 0 then
                        (a, -b)                       // e.g.  2, -1
                        if a <> 0 then (-a, -b)       // e.g. -2, -1
                    if a <> b then
                        (b, a)                        // e.g.  1,  2
                        if b <> 0 then (-b, a)        // e.g. -1,  2
                        if a <> 0 then
                            (b, -a)                   // e.g.  1, -2
                            if b <> 0 then (-b, -a)   // e.g. -1, -2
                |])
            |> Seq.map Point.create
            |> Seq.toArray

    /// Determines whether a bond between the given atoms is valid
    /// for the given neighborhood.
    let testBond atomA atomB neighborhood =
        assert(isValid neighborhood)
        let diff = abs (atomA.Location - atomB.Location)
        neighborhood.Contains(
            max diff.X diff.Y, min diff.X diff.Y)
