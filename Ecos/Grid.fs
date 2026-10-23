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

/// A 2D grid of mutable cells. Edges wrap around (it's a torus),
/// so coordinates are mapped to the range (0,0) to (size.X - 1,
/// size.Y - 1), inclusive.
type Grid =
    private {
        Cells : Cell[(*x*), (*y*)]
    }

    /// Size of this grid.
    member inline this.Size =  
        Point.create(
            this.Cells.GetLength(0),
            this.Cells.GetLength(1))

    /// Wraps the given coordinate into [-size/2, size/2).
    static member inline private WrapCoord(coord, size) =
        let half = size / 2
        let c = (coord + half) % size
        (if c < 0 then c + size else c) - half

    /// Wraps the given location onto this grid.
    member this.Wrap(location) =

        let wrap limit value =
            let value = value % limit
            if value < 0 then value + limit
            else value

        let size = this.Size
        Point.create (
            wrap size.X location.X,
            wrap size.Y location.Y)

    /// Gets the cell at the given location.
    member this.Item(location) =
        let loc = this.Wrap(location)
        this.Cells[loc.X, loc.Y]

    /// Gets the cell at the given location.
    member this.Item(x, y) =
        this[Point.create(x, y)]

    /// Sets the cell at the given location. This mutates the grid.
    member internal this.SetValue(location, cell) =
        let loc = this.Wrap(location)
        this.Cells[loc.X, loc.Y] <- cell

module Grid =

    /// An empty grid of the given size.
    let create size =
        {
            Cells = Array2D.create size.X size.Y Cell.Empty
        }

    /// The shortest vector from location A to location B in the
    /// given grid. Since the grid has no boundary, this vector
    /// might wrap around the edges.
    let getDisplacement locA locB (grid : Grid) =
        grid.Wrap(locB - locA)

    /// Sets the given location to contain an atom with the given
    /// ID. This mutates the grid.
    let setAtom location id (grid : Grid) =
        grid.SetValue(location, Cell.Atom id)
        grid

    /// Sets the given location to contain a wall. This mutates the grid.
    let setWall location (grid : Grid) =
        grid.SetValue(location, Cell.Wall)
        grid

    /// Sets the given location to be an empty space. This mutates the
    /// grid.
    let setEmpty location (grid : Grid) =
        grid.SetValue(location, Cell.Empty)
        grid

    /// Copies the given grid.
    let copy grid =
        { Cells = Array2D.copy grid.Cells }
