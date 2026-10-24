namespace Ecos

/// Grid of cells, with tracked atoms and walls.
type private AtomGrid =
    {
        /// Atoms in the grid, indexed by their IDs.
        Atoms : Atom[]

        /// Locations of walls in the world.
        Walls : Point[]

        /// Occupant of each cell.
        Cells : Grid
    }

module private AtomGrid =

    /// Creates an atom grid.
    let create size =
        {
            Atoms = Array.empty
            Walls = Array.empty
            Cells = Grid.create size
        }

    /// The shortest vector from atom A to atom B in the given
    /// grid, accounting for possible edge wrapping.
    let getDisplacement atomA atomB grid =
        Grid.getDisplacement
            atomA.Location
            atomB.Location
            grid.Cells

    /// Copies the given grid.
    let copy grid =
        {
            grid with
                Atoms = Array.copy grid.Atoms
                Cells = Grid.copy grid.Cells
        }

    /// Adds the given atom to the given grid at the given location.
    let addAtom atom location grid =
        Grid.setAtom location atom.Id grid.Cells   // MUTATION
        { grid with
            Atoms = [| yield! grid.Atoms; yield atom |] }

    /// Adds a wall to the given grid at the given location.
    let addWall location grid =
        Grid.setWall location grid.Cells   // MUTATION
        { grid with
            Walls = [| yield! grid.Walls; yield location |] }

module private Bend =

    /// Tests the bend between bonds AB and AC:
    ///
    /// A -- B
    ///  \
    ///   \
    ///    C
    let test behavior bendLimit atomA atomB atomC world =
        let deltaAB = AtomGrid.getDisplacement atomA atomB world   // vector from A to B
        let deltaAC = AtomGrid.getDisplacement atomA atomC world   // vector from A to C
        behavior.TestBend bendLimit deltaAB deltaAC

    /// Tests multiple bends for bond AB:
    ///
    /// A -?- B
    ///  \
    ///   \
    ///    C
    let testMany behavior bendLimit atomA atomB idsC grid =
        Seq.forall (fun idC ->
            let atomC = grid.Atoms[idC]
            test behavior bendLimit atomA atomB atomC grid)
            idsC

    /// Energy of a bend between the given bond vectors, which
    /// start at the same atom: zero when they point in opposite
    /// directions (a straight strand).
    let private getEnergy kappa u v =
        let dot = float (u .* v)
        kappa * (1.0 + dot / sqrt (float (u .* u) * float (v .* v)))

    /// Bend energy that a bond of the given type between atoms A
    /// and B adds to the given world, at A and at B, whether or
    /// not the bond exists yet.
    let getBondEnergy kappa bondMap atomA atomB grid =

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

    /// Change in the bend energy of bonds of the given type from
    /// moving an atom: at the atom itself, and at each of its
    /// partners.
    let getEnergyDelta
        kappa bondMap atomBefore atomAfter grid =

        /// Computes bend at a partner, B, between the moved atom
        /// and B's other partner, if any.
        let atPartner idB =

            let idsB = BondMap.getBonds idB bondMap
            assert(idsB.Count <= 2)

            if idsB.Count < 2 then 0.0
            else
                let idC =
                    if idsB.MinimumElement = atomBefore.Id then
                        idsB.MaximumElement
                    else idsB.MinimumElement
                let atomB = grid.Atoms[idB]
                let deltaBC =
                    AtomGrid.getDisplacement atomB grid.Atoms[idC] grid
                let getEnergy atomA =
                    let deltaAB =
                        AtomGrid.getDisplacement atomB atomA grid
                    getEnergy kappa deltaAB deltaBC
                getEnergy atomAfter - getEnergy atomBefore

        let ids = BondMap.getBonds atomBefore.Id bondMap
        assert(ids.Count <= 2)

        if ids.IsEmpty then 0.0
        else
            let idB = ids.MinimumElement
            if ids.Count = 1 then atPartner idB
            else
                    // bend at the moved atom, between its two partners
                let idC = ids.MaximumElement
                let atomB = grid.Atoms[idB]
                let atomC = grid.Atoms[idC]
                let at atom =
                    getEnergy kappa
                        (AtomGrid.getDisplacement atom atomB grid)
                        (AtomGrid.getDisplacement atom atomC grid)
                at atomAfter - at atomBefore
                    + atPartner idB + atPartner idC
