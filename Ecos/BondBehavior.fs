namespace Ecos

open System

type private BondBehavior =
    {
        /// All normalized bond vectors of this type.
        Neighborhood : Set<int * int>

        /// Maximum number of bonds of this typ an atom can
        /// have.
        Valence : int

        /// Indicates whether the given atom types can form
        /// a bond.
        CanBond : AtomType -> AtomType -> bool

        CheckCrossBonds : bool
    }

module private StrongBond =

    let neighborhood =
        set [
            2, 0
            2, 1
            2, 2
            3, 0
            3, 1
            3, 2
        ]

    let behavior =
        {
            Neighborhood = neighborhood
            Valence = 2
            CanBond = fun _ _ -> true
            CheckCrossBonds = false
        }

module private WeakBond =

    let neighborhood =
        set [
            2, 0
            2, 1
            2, 2
        ]

    /// Indicates whether the given atom types can form
    /// a weak bond.
    let canBond atomTypeA atomTypeB =
        let atomTypeMin = min atomTypeA atomTypeB
        let atomTypeMax = max atomTypeA atomTypeB
        match atomTypeMin, atomTypeMax with
            | AtomType.W, AtomType.X
            | AtomType.Y, AtomType.Z -> true
            | _ -> false

    let behavior =
        {
            Neighborhood = neighborhood
            Valence = 1
            CanBond = canBond
            CheckCrossBonds = true
        }
