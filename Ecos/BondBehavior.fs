namespace Ecos

/// A rational number, such as -1/2.
type Rational =
    {
        Numerator : int
        Denominator : int
    }

module Rational =

    /// Creates a rational number.
    let create numerator denominator =
        {
            Numerator = numerator
            Denominator = denominator
        }

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
        TestValence : BondTypeMap -> Atom -> bool

        /// Counts stacks adjacent to a bond (existing or 
        /// proposed) between the given atoms.
        CountStacks : BondTypeMap -> Atom -> Atom -> int

        /// Determines whether the angle between the given vectors,
        /// which start at the same atom, is valid for a bond of
        /// this type, with the given bend limit.
        TestBend : Rational (*limit*) -> Point -> Point -> bool

        /// Determines whether the given bond of this type can
        /// peel.
        TestPeel : BondMap -> Atom -> Atom -> bool

        /// Determines whether the given bond of this type is
        /// protected from normal hydrolysis.
        TestHydrolysisProtection : BondTypeMap -> Atom -> Atom -> bool
    }

module private BondBehavior =

    /// Determines orientation of the path connecting the given
    /// points (a-b-c):
    /// * Positive: left turn
    /// * Negative: right turn
    ///
    /// A positive cross product indicates that `b` is counter-
    /// clockwise from `a` (on the left, looking along `a`), a
    /// negative value indicates that `b` is counter-clockwise
    /// from `a`, and zero indicates that the two vectors are
    /// parallel or anti-parallel. (The scalar value is actually
    /// the z-coordinate of a vector perpendicular to the 2D
    /// plane of the two vectors, but we don't use it for that
    /// purpose.)
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

    /// Determines whether the angle between the given bond vectors
    /// is allowed: the cosine of the angle between the bonds must
    /// be at most the given limit.
    let testBendLimit limit u v =
        assert(limit.Denominator > 0)
        let dot = u .* v
        let lhs =
            limit.Denominator * limit.Denominator * dot * dot
        let rhs =
            limit.Numerator * limit.Numerator * (u .* u) * (v .* v)
        if limit.Numerator < 0 then
            dot < 0 && lhs >= rhs    // cos <= -|n|/d
        else
            dot <= 0 || lhs <= rhs   // cos <= n/d

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
    let testValence (bondTypeMap : BondTypeMap) atom =
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
    let countStacks (bondTypeMap : BondTypeMap) atomA atomB =
        let strongBondMap = bondTypeMap[Strong]
        let weakBondMap = bondTypeMap[Weak]
        let bPrimes = BondMap.getBonds atomB.Id weakBondMap
        BondMap.getBonds atomA.Id weakBondMap
            |> Seq.where (fun idA' ->
                BondMap.getBonds idA' strongBondMap
                    |> Seq.exists (fun idB' ->
                        bPrimes.Contains(idB')))
            |> Seq.length

    /// Strands can peel at their ends.
    let testPeel bondMap atomA atomB =
        assert(BondMap.areBonded atomA.Id atomB.Id bondMap)

        let isExposed atom =
            if isCap atom.Type then false   // cap protects strand
            else
                let bonds = BondMap.getBonds atom.Id bondMap
                assert(bonds.Count >= 1)
                bonds.Count <= 1

        isExposed atomA || isExposed atomB

    /// A strong bond is protected from hydrolysis when it is 
    /// part of a stack.
    let testHydrolysisProtection bondTypeMap atomA atomB =
        countStacks bondTypeMap atomA atomB > 0

    /// Behavior of strong bonds.
    let behavior =
        {
            Neighborhood = neighborhood
            Deltas = deltas
            CrossBondDeltas = crossBondDeltas
            AreCompatible = areCompatible
            TestValence = testValence
            CountStacks = countStacks
            TestBend = BondBehavior.testBendLimit
            TestPeel = testPeel
            TestHydrolysisProtection = testHydrolysisProtection
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

    /// Weak bond valence test.
    let testValence (bondTypeMap : BondTypeMap) atom =
        let bonds = BondMap.getBonds atom.Id bondTypeMap[Weak]
        bonds.Count <= 1

    /// Counts stacks affected by the given (existing or 
    /// proposed) weak bond:
    ///    A' ══ A ══ A'   ══ strong bond
    ///    ┆     ┆    ┆    ┆  weak bond
    ///    ┆     ?    ┆
    ///    ┆     ┆    ┆
    ///    B' ══ B ══ B'
    let countStacks (bondTypeMap : BondTypeMap) atomA atomB =
        let strongBondMap = bondTypeMap[Strong]
        let weakBondMap = bondTypeMap[Weak]
        let bPrimes = BondMap.getBonds atomB.Id strongBondMap
        BondMap.getBonds atomA.Id strongBondMap
            |> Seq.where (fun idA' ->
                BondMap.getBonds idA' weakBondMap
                    |> Seq.exists (fun idB' ->
                        bPrimes.Contains(idB')))
            |> Seq.length

    /// Weak bonds can't bend, because they can't form a chain.
    let testBend (_ : Rational) (_ : Point) (_ : Point) =
        true

    /// Weak bonds never peel.
    let testPeel _bondMap _atomA _atomB =
        false

    /// Weak bonds need no hydrolysis protection.
    let testHydrolysisProtection _bondTypeMap _atomA _atomB =
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
            TestBend = testBend
            TestPeel = testPeel
            TestHydrolysisProtection = testHydrolysisProtection
        }

/// Each bond type's behavior, optimized for speed.
type private BehaviorMap =
    {
        /// Strong-bond behavior.
        StrongBehavior : BondBehavior

        /// Weak-bond behavior.
        WeakBehavior : BondBehavior
    }

    /// Gets the behavior of the given bond type.
    member inline this.Item(bondType) =
        match bondType with
            | Strong -> this.StrongBehavior
            | Weak -> this.WeakBehavior

module private BehaviorMap =

    /// Standard behavior of each bond type.
    let standard =
        {
            StrongBehavior = StrongBond.behavior
            WeakBehavior = WeakBond.behavior
        }
