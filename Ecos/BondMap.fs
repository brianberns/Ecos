namespace Ecos

open System.Collections
open System.Collections.Generic
open System.Collections.Immutable

/// Bidirectional bonds between atoms.
type BondMap =
    private {

        /// Sets of bonded atoms, indexed by atom ID.
        BondSets : ImmutableArray<Set<AtomId>>
    }

    /// Answers IDs of atoms bound to the given atom.
    member bondMap.Item(atomId : AtomId) =
        bondMap.BondSets[atomId]

    /// Indicates whether the given bond map is empty.
    member bondMap.IsEmpty =
        Seq.isEmpty bondMap

    /// Indicates whether the given atom is bonded to any other
    /// atoms.
    member bondMap.ContainsKey(atomId : AtomId) =
        atomId < bondMap.BondSets.Length
            && not bondMap[atomId].IsEmpty

    /// IDs of all bonded atoms.
    member bondMap.Keys =
        Seq.map (fun (KeyValue(id, _)) -> id) bondMap

    /// Enumerates non-empty contents.
    member bondMap.GetEnumerator() =
        let pairs =
            seq {
                for (id : AtomId), ids in Seq.indexed bondMap.BondSets do
                    if not ids.IsEmpty then
                        KeyValuePair(id, ids)
            }
        pairs.GetEnumerator()

    interface IEnumerable<KeyValuePair<AtomId, Set<AtomId>>> with

        /// Enumerates non-empty contents.
        member this.GetEnumerator() =
            this.GetEnumerator()

    interface IEnumerable with

        /// Enumerates non-empty contents.
        member this.GetEnumerator() =
            this.GetEnumerator()

module BondMap =

    /// Answers the IDs of all atoms bonded with the given atom.
    let getBonds id (bondMap : BondMap) =

        let impl id =
            if id < bondMap.BondSets.Length then
                bondMap[id]
            else
                Set.empty

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

    /// Ensures that the given bond map can contain bonds for at
    /// least the given number of atoms.
    let private expand capacity (bondMap : BondMap) : BondMap =
        let nNew = capacity - bondMap.BondSets.Length
        if nNew > 0 then
            let builder = ImmutableArray.CreateBuilder(capacity)
            builder.AddRange(bondMap.BondSets)
            Seq.replicate nNew Set.empty |> builder.AddRange
            { BondSets = builder.ToImmutable() }
        else bondMap

    /// Creates a bidirectional bond between the given atoms. If
    /// the bond already exists, this is a no-op.
    let createBond idA idB bondMap =

        let impl idA idB (bondMap : BondMap) : BondMap =
            let ids = bondMap[idA] |> Set.add idB
            { BondSets = bondMap.BondSets.SetItem(idA, ids) }

        bondMap
            |> expand ((max idA idB) + 1)
            |> impl idA idB
            |> impl idB idA

    /// Removes the bidirectional bond between the given atoms. If
    /// bond does not exist, this is a no-op.
    let removeBond idA idB bondMap =

        let impl idA idB (bondMap : BondMap) : BondMap =
            let ids = bondMap[idA].Remove(idB)
            { BondSets = bondMap.BondSets.SetItem(idA, ids) }

        bondMap
            |> impl idA idB
            |> impl idB idA

    /// Empty bond map.
    let empty : BondMap =
        { BondSets = ImmutableArray.Create() }

    /// Converts the given bond map to a sequence.
    let toSeq (bondMap : BondMap) =
        Seq.map (fun (KeyValue(pair)) -> pair) bondMap

    /// Determines whether the given predicate is true for all
    /// entries in the given bond map.
    let forall pred (bondMap : BondMap) =
        Seq.forall (fun (KeyValue(key, value)) ->
            pred key value)
            bondMap
