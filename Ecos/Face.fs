namespace Ecos

module Face =

    /// Gets a strong-bond strand's tangent direction at the given
    /// atom, if it exists.
    ///
    /// Pred ══ A ══ Succ   ══ strong bond
    ///    --tangent->
    let private tryGetTangent atom grid (bondTypeMap : BondTypeMap) =

            // get this atom's strand partners
        let ids = BondMap.getBonds atom.Id bondTypeMap[Strong]
        assert(ids.Count <= 2)

            // tangent is the sum of the atom's strong-bond vectors
        if ids.IsEmpty then None
        else
                // get this atom's successor, if any
            let succOpt =
                Map.tryFind atom.Id bondTypeMap.StrongBondDirectionMap
            ids
                |> Seq.map (fun id ->
                    let partner = grid.Atoms[id]
                    if succOpt = Some id then
                        AtomGrid.getDisplacement atom partner grid    // A -> successor
                    else
                        AtomGrid.getDisplacement partner atom grid)   // predecessor -> A
                |> Seq.reduce (+)
                |> Some

    /// Determines whether the given atom's weak partner, if any,
    /// is on its "face" (as determined by the direction of the
    /// atom's strong bonds).
    ///
    ///         B           ══ strong bond
    ///         ┆           ┆  weak bond
    /// Pred ══ A ══ Succ
    let private testWeakBondDirection grid bondTypeMap atomA atomB =
        tryGetTangent atomA grid bondTypeMap
            |> Option.map (fun tangent ->
                let delta =
                    AtomGrid.getDisplacement atomA atomB grid
                tangent *^ delta > 0)   // B is on the strand face?
            |> Option.defaultValue true

    /// Determines whether the given atom's weak partner, if any,
    /// is on its "face" (as determined by the direction of the
    /// atom's strong bonds), and vice versa.
    let test grid (bondTypeMap : BondTypeMap) atom =

        let testOne =
            testWeakBondDirection grid bondTypeMap

        bondTypeMap[Weak]
            |> BondMap.getBonds atom.Id
            |> Set.forall (fun idB ->
                let atomB = grid.Atoms[idB]
                testOne atom atomB
                    && testOne atomB atom)
