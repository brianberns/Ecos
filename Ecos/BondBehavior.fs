namespace Ecos

/// Specific behavior of a bond type.
type private BondBehavior =
    {
        /// All normalized bond vectors of this type.
        Neighborhood : Set<int (*max*) * int (*min*)>   // not strictly (x, y) points

        /// Denormalized neighborhood, cached for speed.
        Deltas : Point[]

        /// Deltas to check for cross bonds.
        CrossBondDeltas : Set<Point>

        /// Maximum number of bonds of this type an atom can
        /// have.
        Valence : int

        /// Indicates whether the given atom types can form
        /// a bond of this type.
        CanBond : AtomType -> AtomType -> bool

        /// Energy delta of this bond type.
        Energy : int
    }

module private BondBehavior =

    /// Full (x, y) deltas from normalized (max, min) neighborhood.
    let getDeltas neighborhood =
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

module private StrongBond =

    /// All normalized strong bond vectors.
    let neighborhood =
        set [
            2, 0
            2, 1
            2, 2
            3, 0
            3, 1
            3, 2
        ]

    /// Denormalized neighborhood.
    let deltas = BondBehavior.getDeltas neighborhood

    /// Possible cross bond deltas, as determined by Claude.
    let crossBondDeltas =
        neighborhood.Add(3, 3)
            |> BondBehavior.getDeltas
            |> set

    /// Strong bond energy delta.
    let energy = 0   // to-do

    /// Behavior of strong bonds.
    let behavior =
        {
            Neighborhood = neighborhood
            Deltas = deltas
            CrossBondDeltas = crossBondDeltas
            Valence = 2
            CanBond = fun _ _ -> true
            Energy = energy
        }

module private WeakBond =

    /// All normalized weak bond vectors.
    let neighborhood =
        set [
            2, 0
            2, 1
            2, 2
        ]

    /// Denormalized neighborhood.
    let deltas =
        BondBehavior.getDeltas neighborhood

    /// Possible cross bond deltas, as determined by Claude.
    let crossBondDeltas =
        [ (2, 0) ]
            |> BondBehavior.getDeltas
            |> set

    /// Indicates whether the given atom types can form
    /// a weak bond.
    let canBond atomTypeA atomTypeB =
        let atomTypeMin = min atomTypeA atomTypeB
        let atomTypeMax = max atomTypeA atomTypeB
        match atomTypeMin, atomTypeMax with
            | AtomType.W, AtomType.X
            | AtomType.Y, AtomType.Z -> true
            | _ -> false

    /// Weak bond energy delta.
    let energy = 1

    /// Behavior of weak bonds.
    let behavior =
        {
            Neighborhood = neighborhood
            Deltas = deltas
            CrossBondDeltas = crossBondDeltas
            Valence = 1
            CanBond = canBond
            Energy = energy
        }
