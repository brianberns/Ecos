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
        idA idB (bondTypeMap : BondTypeMap) =

            // get strong-bond maps
        let strongBondMap = bondTypeMap[Strong]
        let dirMap = bondTypeMap.StrongBondDirectionMap

        /// Does the given atom have a successor?
        let hasSuccessor id =
            dirMap.ContainsKey(id)

        /// Does the given atom have a predecessor?
        let hasPredecessor id =
            let nBonds =
                (BondMap.getBonds id strongBondMap).Count
            nBonds > (if hasSuccessor id then 1 else 0)   // extra strong bond must come from predecessor

        not (hasSuccessor idA) && not (hasPredecessor idB)

    /// Adds a bond of the given type between the atoms with the
    /// given IDs to the given bond-type map, if it is valid.
    let tryAddBond
        (bondType : BondType) idA idB (bondTypeMap : BondTypeMap) =

            // can't bond atoms that are already bonded
        let areBonded =
            bondTypeMap.BondMaps
                |> Seq.exists (BondMap.areBonded idA idB)
        if areBonded then None
        else
                // update direction map?
            let dirMapOpt =
                if bondType.IsStrong then
                    let isValid =
                        testStrongBondDirection idA idB bondTypeMap
                    if isValid then
                        Map.add idA idB
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
                            idA idB bondTypeMap[bondType]
                    { setBondMap bondType bondMap bondTypeMap with
                        StrongBondDirectionMap = dirMap })

    /// Removes the bidirectional bond of the given type between
    /// the given atoms from the given bond-type map, without
    /// assuming which one is the predecessor.
    let removeBond
        (bondType : BondType) atomA atomB (bondTypeMap : BondTypeMap) =

            // update direction map
        let dirMap =
            let dirMap = bondTypeMap.StrongBondDirectionMap
            if bondType.IsStrong then
                let idPred =
                    if Map.tryFind atomA.Id dirMap = Some atomB.Id then
                        atomA.Id
                    else
                        assert(Map.tryFind atomB.Id dirMap = Some atomA.Id)
                        atomB.Id
                Map.remove idPred dirMap
            else dirMap

            // update bond map
        let bondMap =
            bondTypeMap[bondType]
                |> BondMap.removeBond atomA.Id atomB.Id

        { setBondMap bondType bondMap bondTypeMap with
            StrongBondDirectionMap = dirMap }
