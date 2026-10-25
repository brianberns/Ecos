namespace Ecos

open System

/// Bond type.
type BondType =

    /// Long-term, long-range bonds.
    | Strong

    /// Short-term, short-range bonds.
    | Weak

module BondType =

    /// All bond types.
    let all = [| Strong; Weak |]

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

/// Bond type-specific parameters.
/// 
/// Energy
///   |            ----  <- barrier: in-between state
///   |           /    \
///   |   E_b    /      \
///   |  --------        \
///   |  unbonded         \      E_b + S
///   |                    \
///   |                     -------- bonded (lower by S)
///   |
///    ----------------------------------
/// 
/// The barrier affects how fast the system moves between the
/// states, not which one it favors.
/// 
/// (Drawing by Claude, with my edits.)
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
        /// Temperature at the given location, scaled by Boltzmann's
        /// constant (kT).
        GetTemperature : Point -> float

        /// Bond type-specific parameters.
        BondTypeParameterMap : BondTypeParameterMap

        /// Determines how often peeling happens. +Infinity
        /// prevents peeling altogether.
        PeelBarrier : float
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
