namespace Ecos

/// Associates bond types with bond maps. Optimized for performance.
type BondTypeMap =
    {
        /// Bond maps in this bond type map.
        BondMaps : BondMap[]
    }

    /// Strong bond index.
    static member StrongIdx = 0

    /// Weak bond indx.
    static member WeakIdx = 1

    /// Gets the bond map associated with the given bond type.
    member inline this.Item(bondType) =
        let idx =
            match bondType with
                | Strong -> BondTypeMap.StrongIdx
                | Weak -> BondTypeMap.WeakIdx
        this.BondMaps[idx]

module BondTypeMap =

    /// Empty bond type map.
    let empty =
        let bondMaps =
            BondType.all
                |> Array.map (fun _ -> Map.empty)
        { BondMaps = bondMaps }

    /// Sets the given bond type's bond map.
    let setBondMap bondType bondMap (bondTypeMap : BondTypeMap) =
        let bondMaps =
            BondType.all
                |> Array.map (fun bt ->
                    if bt = bondType then bondMap
                    else bondTypeMap[bt])
        { BondMaps = bondMaps }

/// Specific behavior of a bond type.
type private BondBehavior =
    {
        /// All normalized bond vectors of this type.
        Neighborhood : Neighborhood

        /// Denormalized neighborhood of (x, y) points, cached
        /// for speed.
        Deltas : Point[]

        /// Deltas to check for cross bonds.
        CrossBondDeltas : Set<Point>

        /// Determines whether the given atom types can form
        /// a bond of this type.
        AreCompatible : AtomType -> AtomType -> bool

        /// Determines whether the given atom obeys this type's
        /// valence rule (which may involve other bond types as
        /// well, via the given map).
        TestValence : Atom -> BondTypeMap -> bool

        /// Counts stacks adjacent to the given (existing or 
        /// proposed) bond.
        CountStacks : AtomId -> AtomId -> BondTypeMap -> int

        /// Determines whether the angle defined the given vectors
        /// is valid for a bond of this type.
        TestBondAngle : Point -> Point -> bool

        /// Determines whether the given bond of this type can
        /// peel.
        TestPeel : Atom -> Atom -> BondMap -> bool
    }

module private BondBehavior =

    /// Determines orientation of the path connecting
    /// the given points (a-b-c):
    /// * Positive: left turn
    /// * Negative: right turn
    let private orient (a : Point) (b : Point) (c : Point) =
        sign ((b - a) *^ (c - a))

    /// Determines whether bonds between atoms at the given pairs
    /// of locations cross.
    let testCrossBond (locA, locB) (locC, locD) =
        let cdOpp =   // C and D are on opposite sides of line AB
            orient locA locB locC * orient locA locB locD < 0
        let abOpp =   // A and B are on opposite sides of line CD
            orient locC locD locA * orient locC locD locB < 0
        cdOpp && abOpp

    /// Determines whether a bond between the given atoms starts
    /// a new strand.
    let testStrandStart atomA atomB bondMap =
        [
            atomA, atomB
            atomB, atomA
        ]
            |> Seq.forall (fun (atom, other) ->
                (BondMap.getBonds atom.Id bondMap)
                    .Remove(other.Id)
                    .IsEmpty)

module private StrongBond =

    /// All normalized strong bond vectors.
    let neighborhood : Neighborhood =
        Neighborhood.create [
            2, 0
            2, 1
            2, 2
            3, 0
            3, 1
            3, 2
        ]

    /// Denormalized neighborhood, cached for speed.
    let deltas = Neighborhood.toDeltas neighborhood

    /// Possible cross bond deltas, as determined by Claude.
    let crossBondDeltas = set deltas

    /// Determines whether the given atom type acts as a
    /// cap that prevents extension of a strand.
    let private isCap = function
        | AtomType.P | AtomType.Q -> true
        | _ -> false

    /// Determines whether the given atom types can form a
    /// strong bond.
    let areCompatible atomTypeA atomTypeB =
        not (isCap atomTypeA && isCap atomTypeB)   // caps don't join to each other

    /// Strong bond valence test.
    let testValence atom (bondTypeMap : BondTypeMap) =
        let valence =
            if isCap atom.Type then 1 else 2       // caps can have at most one strong bond
        let bonds = BondMap.getBonds atom.Id bondTypeMap[Strong]
        bonds.Count <= valence

    /// Counts stacks affected by the given (existing or 
    /// proposed) strong bond:
    ///    A' ═══ B'   ══ strong bond
    ///    ┆      ┆    ┆  weak bond
    ///    A  ═?═ B
    ///    ┆      ┆
    ///    A' ═══ B'
    let countStacks idA idB (bondTypeMap : BondTypeMap) =
        let strongBondMap = bondTypeMap[Strong]
        let weakBondMap = bondTypeMap[Weak]
        let bPrimes = BondMap.getBonds idB weakBondMap
        BondMap.getBonds idA weakBondMap
            |> Seq.where (fun idA' ->
                BondMap.getBonds idA' strongBondMap
                    |> Seq.exists (fun idB' ->
                        bPrimes.Contains(idB')))
            |> Seq.length

    /// Ensures strong bond stiffness.
    let testBondAngle u v =
        let dot = u .* v
        dot < 0
            && 25 * dot * dot >= 16 * (u .* u) * (v .* v)   // 16/25 = abs((-4/5)^2) in cos <= -0.8 

    /// Strands can peel at their ends.
    let testPeel atomA atomB bondMap =
        assert(BondMap.areBonded atomA.Id atomB.Id bondMap)

        let isExposed atom =
            if isCap atom.Type then false   // cap protects strand
            else
                let bonds = BondMap.getBonds atom.Id bondMap
                assert(bonds.Count >= 1)
                bonds.Count <= 1

        isExposed atomA || isExposed atomB

    /// Behavior of strong bonds.
    let behavior =
        {
            Neighborhood = neighborhood
            Deltas = deltas
            CrossBondDeltas = crossBondDeltas
            AreCompatible = areCompatible
            TestValence = testValence
            CountStacks = countStacks
            TestBondAngle = testBondAngle
            TestPeel = testPeel
        }

module private WeakBond =

    /// All normalized weak bond vectors.
    let neighborhood : Neighborhood =
        Neighborhood.create [
            2, 0
            2, 1
            2, 2
        ]

    /// Denormalized neighborhood, cached for speed.
    let deltas =
        Neighborhood.toDeltas neighborhood

    /// Possible cross bond deltas, as determined by Claude.
    let crossBondDeltas =
        Neighborhood.create [ (2, 0) ]
            |> Neighborhood.toDeltas
            |> set

    /// Determines whether the given atom types can form a
    /// weak bond.
    let areCompatible atomTypeA atomTypeB =
        let atomTypeMin = min atomTypeA atomTypeB
        let atomTypeMax = max atomTypeA atomTypeB
        match atomTypeMin, atomTypeMax with
            | AtomType.W, AtomType.X
            | AtomType.Y, AtomType.Z
            | AtomType.P, AtomType.Q -> true
            | _ -> false

    /// Weak bond valence test. To have a second weak bond,
    /// an atom must have at least one strong bond.
    let testValence atom (bondTypeMap : BondTypeMap) =
        let bonds = BondMap.getBonds atom.Id bondTypeMap[Weak]
        match bonds.Count with
            | 0 | 1 -> true
            | 2 ->
                let strongIds =
                    BondMap.getBonds atom.Id bondTypeMap[Strong]
                strongIds.Count > 0
            | _ -> false

    /// Counts stacks adjacent to the given (existing or 
    /// proposed) weak bond:
    ///    A' ══ A ══ A'   ══ strong bond
    ///    ┆     ┆    ┆    ┆  weak bond
    ///    ┆     ?    ┆
    ///    ┆     ┆    ┆
    ///    B' ══ B ══ B'
    let countStacks idA idB (bondTypeMap : BondTypeMap) =
        let strongBondMap = bondTypeMap[Strong]
        let weakBondMap = bondTypeMap[Weak]
        let bPrimes = BondMap.getBonds idB strongBondMap
        BondMap.getBonds idA strongBondMap
            |> Seq.where (fun idA' ->
                BondMap.getBonds idA' weakBondMap
                    |> Seq.exists (fun idB' ->
                        bPrimes.Contains(idB')))
            |> Seq.length

    /// Ensures an atom’s two weak partners are on opposite
    /// sides of it, and too far apart to be strongly bonded
    /// to each other.
    let testBondAngle u v =
        u .* v <= -3

    /// Weak bonds never peel.
    let testPeel _atomA _atomB _bondMap =
        false

    /// Behavior of weak bonds.
    let behavior =
        {
            Neighborhood = neighborhood
            Deltas = deltas
            CrossBondDeltas = crossBondDeltas
            AreCompatible = areCompatible
            TestValence = testValence
            CountStacks = countStacks
            TestBondAngle = testBondAngle
            TestPeel = testPeel
        }
