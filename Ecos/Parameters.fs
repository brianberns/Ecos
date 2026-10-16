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

        /// Atom type-specific energy deltas, which override
        /// the default.
        PairEnergyMap : Map<AtomType * AtomType, float>

        /// Energy barrier to bond formation (E_b).
        Barrier : float

        /// Barrier reduction for completed stacks (δ).
        BarrierStackReduction : float

        /// Barrier to starting a strand of this bond type.
        StartBarrier : float

        /// Implicit entropy in this bond type (s).
        Entropy : float
    }

module BondTypeParameters =

    /// Gets the energy delta for the given pairing.
    let getEnergy atomTypeA atomTypeB parms =
        let pairEnergyOpt =
            Map.tryFind (atomTypeA, atomTypeB) parms.PairEnergyMap
        assert(pairEnergyOpt =
            Map.tryFind (atomTypeB, atomTypeA) parms.PairEnergyMap)
        Option.defaultValue parms.Energy pairEnergyOpt

/// Configurable update parameters.
type Parameters =
    {
        /// Temperature scaled by Boltzmann's constant (kT).
        Temperature : float

        /// Bond type-specific parameters.
        BondParameterMap : Map<BondType, BondTypeParameters>

        /// Determines how often non-cap peeling happens.
        /// +Infinity prevents peeling altogether.
        PeelBarrier : float

        /// Determines how often cap peeling happens.
        CapPeelBarrier : float
    }
