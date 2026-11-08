namespace Ecos

open System.Collections
open System.Collections.Generic
open System.Collections.Immutable

/// Bidirectional bonds between atoms.
type BondMap =
    private {
        Data : ImmutableArray<Set<AtomId>>
    }

    member bondMap.Item(atomId : AtomId) =
        bondMap.Data[atomId]

    member bondMap.IsEmpty =
        Seq.isEmpty bondMap

    member bondMap.ContainsKey(atomId : AtomId) =
        atomId <= bondMap.Data.Length
            && not bondMap[atomId].IsEmpty

    member bondMap.Keys =
        Seq.map (fun (KeyValue(id, _)) -> id) bondMap

    member bondMap.GetEnumerator() =
        let pairs =
            seq {
                for (id : AtomId), ids in Seq.indexed bondMap.Data do
                    if not ids.IsEmpty then
                        KeyValuePair(id, ids)
            }
        pairs.GetEnumerator()

    interface IEnumerable<KeyValuePair<AtomId, Set<AtomId>>> with
        member this.GetEnumerator() =
            this.GetEnumerator()

    interface IEnumerable with
        member this.GetEnumerator() =
            this.GetEnumerator()

module BondMap =

    /// Answers the IDs of all atoms bonded with the given atom.
    let getBonds id (bondMap : BondMap) =

        let impl id =
            if id < bondMap.Data.Length then
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

    let private expand capacity (bondMap : BondMap) : BondMap =
        let nNew = capacity - bondMap.Data.Length
        if nNew > 0 then
            let builder =
                ImmutableArray.CreateBuilder(capacity)
            builder.AddRange(bondMap.Data)
            Seq.replicate nNew Set.empty |> builder.AddRange
            { Data = builder.ToImmutable() }
        else bondMap

    /// Creates a bidirectional bond between the given atoms. If
    /// the bond already exists, this is a no-op.
    let createBond idA idB bondMap =

        let impl idA idB (bondMap : BondMap) : BondMap =
            let bondMap = expand (idA + 1) bondMap
            let ids = bondMap[idA] |> Set.add idB
            { Data = bondMap.Data.SetItem(idA, ids) }

        bondMap
            |> impl idA idB
            |> impl idB idA

    /// Removes the bidirectional bond between the given atoms. If
    /// bond does not exist, this is a no-op.
    let removeBond idA idB bondMap =

        let impl idA idB (bondMap : BondMap) : BondMap =
            let ids = bondMap[idA].Remove(idB)
            { Data = bondMap.Data.SetItem(idA, ids) }

        bondMap
            |> impl idA idB
            |> impl idB idA

    let empty : BondMap = { Data = ImmutableArray.Empty }
    
    let toSeq (bondMap : BondMap) =
        Seq.map (fun (KeyValue(pair)) -> pair) bondMap

    let forall pred (bondMap : BondMap) =
        Seq.forall (fun (KeyValue(key, value)) ->
            pred key value)
            bondMap
