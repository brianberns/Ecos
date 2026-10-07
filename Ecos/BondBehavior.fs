namespace Ecos

/// Bond type.
type BondType =

    /// Long-term, long-range bonds.
    | Strong

    /// Short-term, short-range bonds.
    | Weak

module BondType =

    /// All bond types.
    let all = [| Strong; Weak |]

/// Specific behavior of a bond type.
type private BondBehavior =
    {
        /// All normalized bond vectors of this type.
        Neighborhood : Set<int (*max*) * int (*min*)>   // not strictly (x, y) points

        /// Denormalized neighborhood of (x, y) points, cached
        /// for speed.
        Deltas : Point[]

        /// Deltas to check for cross bonds.
        CrossBondDeltas : Set<Point>

        /// Maximum number of bonds of this type an atom can
        /// have.
        Valence : int

        /// Indicates whether the given atom types can form
        /// a bond of this type.
        CanBond : AtomType -> AtomType -> bool

        /// Counts stacks adjacent to the given (existing or 
        /// proposed) bond.
        CountStacks : AtomId -> AtomId -> Map<BondType, BondMap> -> int

        /// Maximum allowed dot product between two bonds of
        /// this type for an atom.
        MaxDotProduct : int
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

    /// Determines orientation of the path connecting
    /// the given points (a-b-c):
    /// * Positive: left turn
    /// * Negative: right turn
    let private orient (a : Point) (b : Point) (c : Point) =
        sign ((b - a) *^ (c - a))

    /// Indicates whether bonds between the given pairs of atoms
    /// cross.
    let testCrossBond (atomA, atomB) (atomC, atomD) =
        let locA = atomA.Location
        let locB = atomB.Location
        let locC = atomC.Location
        let locD = atomD.Location
        let cdOpp =   // C and D are on opposite sides of line AB
            orient locA locB locC * orient locA locB locD < 0
        let abOpp =   // A and B are on opposite sides of line CD
            orient locC locD locA * orient locC locD locB < 0
        cdOpp && abOpp

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
        neighborhood
            |> BondBehavior.getDeltas
            |> set

    /// Counts stacks affected by the given (existing or 
    /// proposed) strong bond:
    ///    A' ═══ B'   ══ strong bond
    ///    ┆      ┆    ┆  weak bond
    ///    A  ═?═ B
    ///    ┆      ┆
    ///    A' ═══ B'
    let countStacks idA idB (bondTypeMap : Map<_, _>) =
        let strongBondMap = bondTypeMap[Strong]
        let weakBondMap = bondTypeMap[Weak]
        let bPrimes = BondMap.getBonds idB weakBondMap
        BondMap.getBonds idA weakBondMap
            |> Seq.where (fun idA' ->
                BondMap.getBonds idA' strongBondMap
                    |> Seq.exists (fun idB' ->
                        bPrimes.Contains(idB')))
            |> Seq.length

    /// Behavior of strong bonds.
    let behavior =
        {
            Neighborhood = neighborhood
            Deltas = deltas
            CrossBondDeltas = crossBondDeltas
            Valence = 2
            CanBond = fun _ _ -> true
            CountStacks = countStacks
            MaxDotProduct = -4
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

    /// Counts stacks adjacent to the given (existing or 
    /// proposed) weak bond:
    ///    A' ══ A ══ A'   ══ strong bond
    ///    ┆     ┆    ┆    ┆  weak bond
    ///    ┆     ?    ┆
    ///    ┆     ┆    ┆
    ///    B' ══ B ══ B'
    let countStacks idA idB (bondTypeMap : Map<_, _>) =
        let strongBondMap = bondTypeMap[Strong]
        let weakBondMap = bondTypeMap[Weak]
        let bPrimes = BondMap.getBonds idB strongBondMap
        BondMap.getBonds idA strongBondMap
            |> Seq.where (fun idA' ->
                BondMap.getBonds idA' weakBondMap
                    |> Seq.exists (fun idB' ->
                        bPrimes.Contains(idB')))
            |> Seq.length

    /// Behavior of weak bonds.
    let behavior =
        {
            Neighborhood = neighborhood
            Deltas = deltas
            CrossBondDeltas = crossBondDeltas
            Valence = 2
            CanBond = canBond
            CountStacks = countStacks
            MaxDotProduct = -3
        }
