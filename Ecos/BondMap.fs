namespace Ecos

/// Unique ID of an atom in a world.
type AtomId = int

/// Bidirectional bonds between atoms.
type BondMap =
    private {
        Data : Map<AtomId, Set<AtomId>>
    }

    member this.ContainsKey(id) =
        this.Data.ContainsKey(id)

    member this.Keys =
        this.Data.Keys

    member this.IsEmpty =
        this.Data.IsEmpty

module BondMap =

    /// An empty bond map.
    let empty = { Data = Map.empty }

    let toSeq bondMap =
        Map.toSeq bondMap.Data

    let forall predicate bondMap =
        Map.forall predicate bondMap.Data

    /// Answers the IDs of all atoms bonded with the given atom.
    let getBonds id (bondMap : BondMap) =
        let ids =
            bondMap.Data
                |> Map.tryFind id
                |> Option.defaultValue Set.empty
        assert(
            Seq.forall (fun idB ->
                bondMap.Data[idB].Contains(id))
                ids)
        ids

    /// Indicates whether the atoms with the given IDs are already
    /// bonded.
    let areBonded idA idB bondMap =
        assert(
            (getBonds idA bondMap).Contains(idB) =
                (getBonds idB bondMap).Contains(idA))
        (getBonds idA bondMap).Contains(idB)

    /// Creates a bidirectional bond between the given atoms. If
    /// the bond already exists, this is a no-op.
    let createBond idA idB bondMap =

        let impl idA idB (bondMap : BondMap) : BondMap =
            let ids =
                Map.tryFind idA bondMap.Data
                    |> Option.defaultValue Set.empty
                    |> Set.add idB
            { Data = Map.add idA ids bondMap.Data }

        bondMap
            |> impl idA idB
            |> impl idB idA

    /// Removes the bidirectional bond between the given atoms. If
    /// bond does not exist, this is a no-op.
    let remove idA idB bondMap =

        let impl idA idB (bondMap : BondMap) : BondMap =
            let ids = bondMap.Data[idA].Remove(idB)
            if ids.IsEmpty then
                { Data = Map.remove idA bondMap.Data }
            else
                { Data = Map.add idA ids bondMap.Data }

        bondMap
            |> impl idA idB
            |> impl idB idA
