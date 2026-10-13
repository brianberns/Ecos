namespace Ecos

open System

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
        /// Energy delta of this bond type (S).
        Energy : float

        /// Energy barrier to bond formation (E_b).
        Barrier : float

        /// Barrier reduction for completed stacks (δ).
        BarrierStackReduction : float

        /// Implicit entropy in this bond type (s).
        Entropy : float
    }

/// Configurable update parameters.
type Parameters =
    {
        /// Temperature scaled by Boltzmann's constant (kT).
        Temperature : float

        /// Bond type-specific parameters.
        BondParameterMap : Map<BondType, BondTypeParameters>

        /// Determines how often peeling happens. +Infinity
        /// prevents peeling altogether.
        PeelBarrier : float
    }
