namespace Ecos

open System.Collections.Generic

/// Unique ID of an atom in a world.
type AtomId = int

type IBondMap =
    abstract member ContainsKey : AtomId -> bool
    abstract member Keys : ICollection<AtomId> with get
    abstract member IsEmpty : bool with get
    abstract member ToSeq : seq<AtomId * Set<AtomId>> with get
    abstract member ForAll : (AtomId -> Set<AtomId> -> bool) -> bool
    abstract member GetBonds : AtomId -> Set<AtomId>
    abstract member AreBonded : AtomId * AtomId -> bool
    abstract member CreateBond : AtomId * AtomId -> IBondMap
    abstract member RemoveBond : AtomId * AtomId -> IBondMap

module BondMap =

    let toSeq (bondMap : IBondMap) =
        bondMap.ToSeq

    let forall predicate (bondMap : IBondMap)  =
        bondMap.ForAll(predicate)

    let getBonds id (bondMap : IBondMap)  =
        bondMap.GetBonds(id)

    let areBonded idA idB (bondMap : IBondMap)  =
        bondMap.AreBonded(idA, idB)

    let createBond idA idB (bondMap : IBondMap)  =
        bondMap.CreateBond(idA, idB)

    let removeBond idA idB (bondMap : IBondMap)  =
        bondMap.RemoveBond(idA, idB)

type private StrongBond =
    {
        PredecessorOpt : Option<AtomId>
        SuccessorOpt : Option<AtomId>
    }

module private StrongBond =

    let toSet bond =
        set [
            match bond.PredecessorOpt with
                | Some id -> id
                | None -> ()
            match bond.SuccessorOpt with
                | Some id -> id
                | None -> ()
        ]
        
type internal StrongBondMap =
    private {
        Data : Map<AtomId, StrongBond>
    }

    member this.ContainsKey(id) =
        this.Data.ContainsKey(id)

    member this.Keys =
        this.Data.Keys

    member this.IsEmpty =
        this.Data.IsEmpty

    member this.ToSeq =
        this.Data
            |> Map.toSeq
            |> Seq.map (fun (id, bond) ->
                id, StrongBond.toSet bond)

    member this.ForAll predicate =
        this.Data
            |> Map.forall (fun id bond ->
                predicate id (StrongBond.toSet bond))

    /// Answers the IDs of all atoms bonded with the given atom.
    member this.GetBonds(id) =

        let impl id =
            this.Data
                |> Map.tryFind id
                |> Option.map StrongBond.toSet
                |> Option.defaultValue Set.empty

        let ids = impl id
        assert(
            Seq.forall (fun idB ->
                (impl idB).Contains(id))
                ids)
        ids

    /// Indicates whether the atoms with the given IDs are already
    /// bonded.
    member this.AreBonded(idA, idB) =

        let impl idA idB =
            this.GetBonds(idA).Contains(idB)

        let result = impl idA idB
        assert(impl idB idA = result)
        result

    /// Creates a directional bond between the given atoms.
    member this.CreateBond(idPred, idSucc) =

        let setSucc idPred idSucc bondMap =
            let bond =
                match Map.tryFind idPred bondMap.Data with
                    | Some bond ->
                        assert(bond.SuccessorOpt.IsNone)
                        { bond with SuccessorOpt = Some idSucc }
                    | None ->
                        { PredecessorOpt = None; SuccessorOpt = Some idSucc }
            { Data = Map.add idPred bond bondMap.Data }

        let setPred idPred idSucc bondMap =
            let bond =
                match Map.tryFind idSucc bondMap.Data with
                    | Some bond ->
                        assert(bond.PredecessorOpt.IsNone)
                        { bond with PredecessorOpt = Some idPred }
                    | None ->
                        { PredecessorOpt = Some idPred; SuccessorOpt = None }
            { Data = Map.add idPred bond bondMap.Data }

        this
            |> setSucc idPred idSucc
            |> setPred idPred idSucc

    /// Removes the directional bond between the given atoms.
    member this.RemoveBond(idPred, idSucc) =

        let removeSucc idPred idSucc bondMap =
            match Map.tryFind idPred bondMap.Data with
                | Some bond ->
                    assert(bond.SuccessorOpt = Some idSucc)
                    if bond.PredecessorOpt.IsSome then
                        let bond = { bond with SuccessorOpt = None }
                        { Data = Map.add idPred bond bondMap.Data }
                    else
                        { Data = Map.remove idPred bondMap.Data }
                | None ->
                    assert(false)
                    bondMap

        let removePred idPred idSucc bondMap =
            match Map.tryFind idSucc bondMap.Data with
                | Some bond ->
                    assert(bond.PredecessorOpt = Some idPred)
                    if bond.SuccessorOpt.IsSome then
                        let bond = { bond with PredecessorOpt = None }
                        { Data = Map.add idPred bond bondMap.Data }
                    else
                        { Data = Map.remove idPred bondMap.Data }
                | None ->
                    assert(false)
                    bondMap

        this
            |> removeSucc idPred idSucc
            |> removePred idPred idSucc

    interface IBondMap with
        member this.ContainsKey(id) = this.ContainsKey(id)
        member this.Keys = this.Keys
        member this.IsEmpty = this.IsEmpty
        member this.ToSeq = this.ToSeq
        member this.ForAll(predicate) = this.ForAll(predicate)
        member this.GetBonds(id) = this.GetBonds(id)
        member this.AreBonded(idA, idB) = this.AreBonded(idA, idB)
        member this.CreateBond(idPred, idSucc) = this.CreateBond(idPred, idSucc)
        member this.RemoveBond(idPred, idSucc) = this.RemoveBond(idPred, idSucc)

module internal StrongBondMap =

    let empty = { Data = Map.empty }

module private WeakBond =

    let toSet = Set.singleton

type WeakBondMap =
    private {
        Data : Map<AtomId, AtomId (*partner*)>
    }

    member this.ContainsKey(id) =
        this.Data.ContainsKey(id)

    member this.Keys =
        this.Data.Keys

    member this.IsEmpty =
        this.Data.IsEmpty

    member this.ToSeq =
        this.Data
            |> Map.toSeq
            |> Seq.map (fun (idA, idB) ->
                idA, WeakBond.toSet idB)

    member this.ForAll(predicate) =
        this.Data
            |> Map.forall (fun idA idB ->
                predicate idA (WeakBond.toSet idB))

    /// Answers the IDs of all atoms bonded with the given atom.
    member this.GetBonds(id) =

        let impl id =
            this.Data
                |> Map.tryFind id
                |> Option.map WeakBond.toSet
                |> Option.defaultValue Set.empty

        let ids = impl id
        assert(
            Seq.forall (fun idB ->
                (impl idB).Contains(id))
                ids)
        ids

    /// Indicates whether the atoms with the given IDs are already
    /// bonded.
    member this.AreBonded(idA, idB) =

        let impl idA idB =
            this.GetBonds(idA).Contains(idB)

        let result = impl idA idB
        assert(impl idB idA = result)
        result

    /// Creates a bidirectional bond between the given atoms.
    member this.CreateBond(idA, idB) =
        { Data =
            this.Data
                |> Map.add idA idB
                |> Map.add idB idA }

    /// Removes the bidirectional bond between the given atoms.
    member this.RemoveBond(idA, idB) =
        { Data =
            this.Data
                |> Map.remove idA
                |> Map.remove idB }

    interface IBondMap with
        member this.ContainsKey(id) = this.ContainsKey(id)
        member this.Keys = this.Keys
        member this.IsEmpty = this.IsEmpty
        member this.ToSeq = this.ToSeq
        member this.ForAll(predicate) = this.ForAll(predicate)
        member this.GetBonds(id) = this.GetBonds(id)
        member this.AreBonded(idA, idB) = this.AreBonded(idA, idB)
        member this.CreateBond(idA, idB) = this.CreateBond(idA, idB)
        member this.RemoveBond(idA, idB) = this.RemoveBond(idA, idB)

module internal WeakBondMap =

    let empty = { Data = Map.empty }
