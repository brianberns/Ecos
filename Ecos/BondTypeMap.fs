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

/// Directional bond map.
type BondDirectionMap =
    Map<AtomId (*predecessor*), AtomId (*successor*)>

/// Associates bond types with bond maps.
type BondTypeMap =
    {
        /// Strong-bond map.
        StrongBondMap : BondMap

        /// Weak-bond map.
        WeakBondMap : BondMap

        /// Direction of strong bonds.
        StrongBondDirectionMap : BondDirectionMap
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

    /// Empty bond-type map.
    let empty =
        {
            StrongBondMap = Map.empty
            WeakBondMap = Map.empty
            StrongBondDirectionMap = Map.empty
        }

    /// Sets the given bond type's bond map.
    let private setBondMap
        bondType bondMap (bondTypeMap : BondTypeMap) =
        match bondType with
            | Strong -> { bondTypeMap with StrongBondMap = bondMap }
            | Weak -> { bondTypeMap with WeakBondMap = bondMap }

    /// Determines whether a strong bond can be added from atom A to
    /// atom B without branching a strand: A must not already have a
    /// successor, and B must not already have a predecessor.
    let private testStrongBondDirection
        atomA atomB (bondTypeMap : BondTypeMap) =

            // get strong-bond map
        let strongBondMap = bondTypeMap[Strong]
        assert(
            BondMap.areBonded atomA.Id atomB.Id strongBondMap
                |> not)

            // get strong-bond direction map
        let dirMap = bondTypeMap.StrongBondDirectionMap

        /// Does the given atom have a successor?
        let hasSuccessor atom =
            dirMap.ContainsKey(atom.Id)

        /// Does the given atom have a predecessor?
        let hasPredecessor atom =
            let nBonds =
                (BondMap.getBonds atom.Id strongBondMap).Count
            nBonds > (if hasSuccessor atom then 1 else 0)   // extra strong bond must come from predecessor

        not (hasSuccessor atomA) && not (hasPredecessor atomB)

    /// Adds a bond of the given type between the given atoms
    /// to the given bond-type map, if it is valid.
    let tryAddBond (bondType : BondType) atomA atomB bondTypeMap =

            // update direction map?
        let dirMapOpt =
            if bondType.IsStrong then
                let isValid =
                    testStrongBondDirection atomA atomB bondTypeMap
                if isValid then
                    Map.add
                        atomA.Id atomB.Id
                        bondTypeMap.StrongBondDirectionMap
                        |> Some
                else None
            else
                Some bondTypeMap.StrongBondDirectionMap

            // update bond map?
        dirMapOpt
            |> Option.map (fun dirMap ->
                let bondMap =
                    BondMap.createBond
                        atomA.Id atomB.Id bondTypeMap[bondType]
                { setBondMap bondType bondMap bondTypeMap with
                    StrongBondDirectionMap = dirMap })

    /// Removes the bidirectional bond of the given type between
    /// the given atoms from the given bond-type map.
    let removeBond
        (bondType : BondType) atomA atomB (bondTypeMap : BondTypeMap) =

            // update direction map
        let dirMap =
            if bondType.IsStrong then
                Map.remove
                    atomA.Id bondTypeMap.StrongBondDirectionMap
            else
                bondTypeMap.StrongBondDirectionMap

            // update bond map
        let bondMap =
            bondTypeMap[bondType]
                |> BondMap.removeBond atomA.Id atomB.Id

        { setBondMap bondType bondMap bondTypeMap with
            StrongBondDirectionMap = dirMap }
