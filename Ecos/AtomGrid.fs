namespace Ecos

/// Grid of cells, with tracked atoms and walls.
type AtomGrid =
    {
        /// Atoms in the grid, indexed by their IDs.
        Atoms : Atom[]

        /// Locations of walls in the grid.
        Walls : Point[]

        /// Occupant of each cell in the grid.
        Cells : Grid
    }

module AtomGrid =

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
