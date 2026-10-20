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

/// Configurable update parameters.
type Parameters =
    {
        /// Temperature at the given location, scaled by Boltzmann's
        /// constant (kT).
        GetTemperature : Point -> float

        /// Bond type-specific parameters.
        BondParameterMap : Map<BondType, BondTypeParameters>

        /// Determines how often peeling happens. +Infinity
        /// prevents peeling altogether.
        PeelBarrier : float

        /// Determines how often bonds break due to hydrolysis.
        HydrolysisRate : float
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
