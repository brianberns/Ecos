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

/// Associates bond types with bond maps, optimized for speed.
type BondTypeMap =
    {
        /// Strong-bond map.
        StrongBondMap : BondMap

        /// Weak-bond map.
        WeakBondMap : BondMap
    }

    /// Gets the bond map associated with the given bond type.
    member inline this.Item(bondType) =
        match bondType with
            | Strong -> this.StrongBondMap
            | Weak -> this.WeakBondMap

    /// All bond maps in this bond type map.
    member this.BondMaps =
        seq {
            this.StrongBondMap
            this.WeakBondMap
        }

module BondTypeMap =

    /// Empty bond type map.
    let empty =
        {
            StrongBondMap = Map.empty
            WeakBondMap = Map.empty
        }

    /// Sets the given bond type's bond map.
    let setBondMap bondType bondMap (bondTypeMap : BondTypeMap) =
        match bondType with
            | Strong -> { bondTypeMap with StrongBondMap = bondMap }
            | Weak -> { bondTypeMap with WeakBondMap = bondMap }
