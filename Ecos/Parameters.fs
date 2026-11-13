namespace Ecos

open System

/// Bond type-specific parameters.
///
///  Free energy
///       │
///       │            ___
///       │           /   \
///       │          /     \
///       │         /   B   \
///       │        /         \
///     0 ┼───────/           \
///       │  unbonded          \
///       │                     \
///       │                      \_______  bonded: −E + kT·s
///       │                          ↑
///       │                          │ kT·s: entropy lift, grows with heat
///       │                          │
///    −E ┼ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─┴─ ─ ─  energy alone
///       │
///       └──────────────────────────────────────→
///          unbonded    in-between    bonded
///
/// E is the bond's energy: its type's energy (or pair energy), plus
/// the stack energy per stack, minus any bend energy. s is the
/// entropy the bond loses, which lifts the bonded state by kT·s, so
/// the bonded state is favored only while kT < E/s:
///
///   bonded/unbonded = e^(E/kT − s)
///
/// With s = 0, a bond never melts. B is the barrier: the hump's height
/// above the higher of the two states. It is the barrier, less the
/// reduction per stack (never below 0), plus the start barrier if the
/// bond starts a strand. Per attempt:
///
///   form  = e^(−B/kT) · min(1, e^(E/kT − s))
///   break = e^(−B/kT) · min(1, e^(s − E/kT))
///
/// So the barrier affects how fast bonds change, not which state is
/// favored.
type BondTypeParameters =
    {
        /// Default energy delta of this bond type (S).
        Energy : float

        /// Bidirectional atom type-specific energy deltas, which
        /// override the default.
        PairEnergyMap : Map<AtomType * AtomType, float>

        /// Energy barrier to bond formation (E_b).
        Barrier : float

        /// Barrier reduction for completed stacks (δ).
        BarrierStackReduction : float

        /// Barrier to starting a strand of this bond type.
        StartBarrier : float

        /// Bidirectional atom type-specific start barriers, which
        /// override the default.
        PairStartBarrierMap : Map<AtomType * AtomType, float>

        /// Implicit entropy in this bond type (s).
        Entropy : float

        /// Rate at which unprotected bonds of this type break due
        /// to hydrolysis.
        HydrolysisRate : float

        /// Rate at which protected bonds of this type break due
        /// to hydrolysis.
        ProtectedHydrolysisRate : float

        /// Sharpest bend allowed at an atom between two bonds of
        /// this type: the cosine of the angle between the bonds
        /// must be at most this value.
        BendLimit : Rational

        /// Energy cost of a bend at an atom between two bonds of
        /// this type (kappa): a bend whose two bonds make an angle
        /// phi costs kappa * (1 + cos phi), which is zero when
        /// they are in line.
        BendStiffness : float

        /// Atom-type-specific bend stiffnesses, which override the
        /// default.
        BendStiffnessMap : Map<AtomType, float>
    }

module BondTypeParameters =

    /// Gets the energy delta for the given pairing.
    let getEnergy atomTypeA atomTypeB parms =
        let pairEnergyOpt =
            Map.tryFind (atomTypeA, atomTypeB) parms.PairEnergyMap
        assert(pairEnergyOpt =
            Map.tryFind (atomTypeB, atomTypeA) parms.PairEnergyMap)
        Option.defaultValue parms.Energy pairEnergyOpt

    /// Adds the given energy delta for the given pair. This is
    /// a bidirectional mapping.
    let addPairEnergy atomTypeA atomTypeB energy parms =
        {
            parms with
                PairEnergyMap =
                    parms.PairEnergyMap
                        |> Map.add (atomTypeA, atomTypeB) energy
                        |> Map.add (atomTypeB, atomTypeA) energy
        }

    /// Gets the start barrier for the given pairing.
    let getStartBarrier atomTypeA atomTypeB parms =
        let startBarrierOpt =
            Map.tryFind (atomTypeA, atomTypeB) parms.PairStartBarrierMap
        assert(startBarrierOpt =
            Map.tryFind (atomTypeB, atomTypeA) parms.PairStartBarrierMap)
        Option.defaultValue parms.StartBarrier startBarrierOpt

    /// Adds the given start barrier for the given pair. This is
    /// a bidirectional mapping.
    let addStartBarrier atomTypeA atomTypeB energy parms =
        {
            parms with
                PairStartBarrierMap =
                    parms.PairStartBarrierMap
                        |> Map.add (atomTypeA, atomTypeB) energy
                        |> Map.add (atomTypeB, atomTypeA) energy
        }

    /// Gets the bend stiffness for the given atom type.
    let getBendStiffness atomType parms =
        parms.BendStiffnessMap
            |> Map.tryFind atomType
            |> Option.defaultValue parms.BendStiffness

    /// Determines whether the given bond type has any bend
    /// stiffness.
    let hasBendStiffness parms =
        parms.BendStiffness <> 0
            || not parms.BendStiffnessMap.IsEmpty

/// Each bond type's parameters, optimized for speed.
type BondTypeParameterMap =
    {
        /// Strong-bond parameters.
        StrongParameters : BondTypeParameters

        /// Weak-bond parameters.
        WeakParameters : BondTypeParameters
    }

    /// Gets the parameters of the given bond type.
    member inline this.Item(bondType) =
        match bondType with
            | Strong -> this.StrongParameters
            | Weak -> this.WeakParameters

module BondTypeParameterMap =

    /// Creates a bond type parameter map from the given plain
    /// map.
    let ofMap (plain : Map<_, _>) =
        {
            StrongParameters = plain[Strong]
            WeakParameters = plain[Weak]
        }

    /// Converts the given bond type parameter map into a plain
    /// map.
    let toMap bondTypeParmMap =
        Map [
            Strong, bondTypeParmMap.StrongParameters
            Weak, bondTypeParmMap.WeakParameters
        ]

    /// Adds the given bond type parameters to the given bond
    /// type parameter map, replacing the existing parameters.
    let add bondType bondTypeParms bondTypeParmMap =
        match bondType with
            | Strong ->
                { bondTypeParmMap with
                    StrongParameters = bondTypeParms }
            | Weak ->
                { bondTypeParmMap with
                    WeakParameters = bondTypeParms }

/// Configurable update parameters.
type Parameters =
    {
        /// Unnormalized frequency of attempts to move an atom.
        MoveAtomWeight : float

        /// Unnormalized frequency of attempts to toggle a strong
        /// bond.
        ToggleStrongBondWeight : float

        /// Unnormalized frequency of attempts to toggle a weak
        /// bond.
        ToggleWeakBondWeight : float

        /// Temperature at the given location, scaled by Boltzmann's
        /// constant (kT).
        GetTemperature : Point -> float

        /// Bond type-specific parameters.
        BondTypeParameterMap : BondTypeParameterMap

        /// Additional energy from a stacked weak bond.
        StackEnergy : float

        /// Determines how often non-cap peeling happens. +Infinity
        /// prevents non-cap peeling altogether.
        PeelBarrier : float

        /// Determines how often cap peeling happens. +Infinity
        /// prevents cap peeling altogether.
        CapPeelBarrier : float
    }

module Parameters =

    /// Gets a uniform temperature, regardless of location.
    let getUniformTemperature (kT : float) (_ : Point) =
        kT

    /// Gets the average temperature between the given atoms.
    let getAvgTemperature atomA atomB parms =
        let tempA = parms.GetTemperature atomA.Location
        let tempB = parms.GetTemperature atomB.Location
        (tempA + tempB) / 2.0
