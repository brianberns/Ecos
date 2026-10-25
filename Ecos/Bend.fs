namespace Ecos

module private Bend =

    /// Tests the bend between bonds AB and AC:
    ///
    /// A -- B
    ///  \
    ///   \
    ///    C
    let test behavior bendLimit grid atomA atomB atomC =
        let deltaAB = AtomGrid.getDisplacement atomA atomB grid   // vector from A to B
        let deltaAC = AtomGrid.getDisplacement atomA atomC grid   // vector from A to C
        behavior.TestBend bendLimit deltaAB deltaAC

    /// Tests multiple bends for bond AB:
    ///
    /// A -?- B
    ///  \
    ///   \
    ///    C
    let testMany behavior bendLimit grid atomA atomB idsC =
        Seq.forall (fun idC ->
            let atomC = grid.Atoms[idC]
            test behavior bendLimit grid atomA atomB atomC)
            idsC

    /// Energy of a bend between the given bond vectors, which
    /// start at the same atom: zero when they point in opposite
    /// directions (a straight strand).
    let private getEnergy kappa u v =
        let dot = float (u .* v)
        kappa * (1.0 + dot / sqrt (float (u .* u) * float (v .* v)))

    /// Bend energy of a bond between atoms A and B.
    let getBondEnergy kappa grid bondMap atomA atomB =

        let at atomA atomB =

            let ids = BondMap.getBonds atomA.Id bondMap
            assert(ids.Count <= 2)

                // find other bonded atom, assuming there's not more than one
            let atomCOpt =
                if ids.IsEmpty then None
                elif ids.MinimumElement <> atomB.Id then
                    Some grid.Atoms[ids.MinimumElement]
                elif ids.MaximumElement <> atomB.Id then
                    Some grid.Atoms[ids.MaximumElement]
                else None
            atomCOpt
                |> Option.map (fun atomC ->
                    getEnergy kappa
                        (AtomGrid.getDisplacement atomA atomB grid)
                        (AtomGrid.getDisplacement atomA atomC grid))
                |> Option.defaultValue 0.0

        at atomA atomB + at atomB atomA

    /// Change in the bend energy of bonds from moving an atom:
    /// at the atom itself, and at each of its partners.
    let getEnergyDelta
        kappa grid bondMap atomBefore atomAfter =

        /// Computes bend at a partner, B, between the moved atom
        /// and B's other partner, if any.
        let atPartner idB =

                // get B's partner IDs (one of which is the moved atom)
            let idsB = BondMap.getBonds idB bondMap
            assert(idsB.Contains(atomBefore.Id))
            assert(idsB.Count <= 2)

            if idsB.Count < 2 then 0.0
            else
                    // find other bonded atom, assuming there's not more than one
                let idC =
                    if idsB.MinimumElement = atomBefore.Id then
                        idsB.MaximumElement
                    else
                        assert(idsB.MaximumElement = atomBefore.Id)
                        idsB.MinimumElement

                    // compute the before/after energy delta
                let atomB = grid.Atoms[idB]
                let deltaBC =
                    AtomGrid.getDisplacement atomB grid.Atoms[idC] grid
                let getEnergy atomA =
                    let deltaAB =
                        AtomGrid.getDisplacement atomB atomA grid
                    getEnergy kappa deltaAB deltaBC
                getEnergy atomAfter - getEnergy atomBefore

        let idsA = BondMap.getBonds atomBefore.Id bondMap
        assert(idsA.Count <= 2)

        if idsA.IsEmpty then 0.0   // no bend
        else
                // assume atom has no more than two partners
            let idB = idsA.MinimumElement
            if idsA.Count = 1 then
                atPartner idB   // partner's bend only
            else
                    // bend at the moved atom, between its two partners
                let idC = idsA.MaximumElement
                let atomB = grid.Atoms[idB]
                let atomC = grid.Atoms[idC]
                let at atom =
                    getEnergy kappa
                        (AtomGrid.getDisplacement atom atomB grid)
                        (AtomGrid.getDisplacement atom atomC grid)
                at atomAfter - at atomBefore
                    + atPartner idB + atPartner idC
