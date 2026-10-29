namespace Ecos

/// Bidirectional bonds between atoms.
type BondMap = Map<AtomId, Set<AtomId>>

module BondMap =

    /// Answers the IDs of all atoms bonded with the given atom.
    let getBonds id (bondMap : BondMap) =

        let impl id =
            bondMap
                |> Map.tryFind id
                |> Option.defaultValue Set.empty

        let result = impl id
        assert(Seq.forall (impl >> Set.contains id) result)
        result

    /// Indicates whether the atoms with the given IDs are already
    /// bonded.
    let areBonded idA idB bondMap =

        let impl idA idB =
            bondMap
                |> getBonds idA
                |> Set.contains idB

        let result = impl idA idB
        assert(impl idB idA = result)
        result

    /// Creates a bidirectional bond between the given atoms. If
    /// the bond already exists, this is a no-op.
    let createBond idA idB bondMap =

        let impl idA idB (bondMap : BondMap) : BondMap =
            let ids =
                Map.tryFind idA bondMap
                    |> Option.defaultValue Set.empty
                    |> Set.add idB
            Map.add idA ids bondMap

        bondMap
            |> impl idA idB
            |> impl idB idA

    /// Removes the bidirectional bond between the given atoms. If
    /// bond does not exist, this is a no-op.
    let removeBond idA idB bondMap =

        let impl idA idB (bondMap : BondMap) : BondMap =
            let ids = bondMap[idA].Remove(idB)
            if ids.IsEmpty then
                Map.remove idA bondMap
            else
                Map.add idA ids bondMap

        bondMap
            |> impl idA idB
            |> impl idB idA
