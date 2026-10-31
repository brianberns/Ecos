namespace Ecos

/// Types of cells in a grid.
[<Struct; RequireQualifiedAccess>]
type Cell =

    /// Atom with the given unique ID.
    | Atom of AtomId

    /// Immovable wall.
    | Wall

    /// Empty space.
    | Empty

/// Grid of cells, with tracked atoms and walls.
type private AtomGrid =
    {
        /// Atoms in the grid, indexed by their IDs.
        Atoms : Atom[]

        /// Locations of walls in the grid.
        Walls : Point[]

        /// Occupant of each cell in the grid.
        Cells : Grid<Cell>
    }

module private AtomGrid =

    /// Creates an atom grid.
    let create size =
        {
            Atoms = Array.empty
            Walls = Array.empty
            Cells = Grid.create size Cell.Empty
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

    /// Adds the given atom to the given grid.
    let addAtom atom grid =
        grid.Cells[atom.Location] <- Cell.Atom atom.Id   // MUTATION
        { grid with
            Atoms = [| yield! grid.Atoms; yield atom |] }

    /// Adds a wall to the given grid at the given location.
    let addWall location grid =
        grid.Cells[location] <- Cell.Wall   // MUTATION
        { grid with
            Walls = [| yield! grid.Walls; yield location |] }

    /// Moves the given atom's location in the given grid.
    let moveAtom atomBefore atomAfter grid =
        assert(atomBefore.Id = atomAfter.Id)
        assert(grid.Atoms[atomBefore.Id] = atomBefore)
        assert(
            grid.Cells[atomBefore.Location] = Cell.Atom atomBefore.Id)
        assert(grid.Cells[atomAfter.Location] = Cell.Empty)

            // MUTATION
        let id = atomBefore.Id
        grid.Atoms[id] <- atomAfter   // replaces `atomBefore`
        grid.Cells[atomBefore.Location] <- Cell.Empty
        grid.Cells[atomAfter.Location] <- Cell.Atom id
