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

/// A 2D grid of mutable cells: a torus of a fixed (even) size, centered
/// on the origin, whose opposite edges meet. Coordinates run from -size/2
/// up to, but not including, size/2, and any coordinate outside that
/// range is wrapped into it.
type Grid =
    private {
        Cells : Cell[(*x*), (*y*)]
    }

    /// Width of the grid.
    member this.Width = this.Cells.GetLength(0)

    /// Height of the grid.
    member this.Height = this.Cells.GetLength(1)

    /// Wraps the given coordinate into [-size/2, size/2).
    static member inline private WrapCoord(coord, size) =
        let half = size / 2
        let c = (coord + half) % size
        (if c < 0 then c + size else c) - half

    /// Wraps the given location onto this grid.
    member this.Wrap(x, y) =
        Grid.WrapCoord(x, this.Width), Grid.WrapCoord(y, this.Height)

    /// Gets the cell at the given location.
    member this.Item(x, y) =
        let x, y = this.Wrap(x, y)
        this.Cells[x, y]

    /// Gets the cell at the given location.
    member this.Item(location) =
        this[location.X, location.Y]

    /// Sets the cell at the given location. This mutates the grid.
    member internal this.SetValue(location, cell) =
        let x, y = this.Wrap(location.X, location.Y)
        this.Cells[x, y] <- cell

module Grid =

    /// An empty grid of the given (even) size.
    let create width height =
        assert(width > 0 && width % 2 = 0)
        assert(height > 0 && height % 2 = 0)
        {
            Cells =
                Array2D.createBased
                    (-width / 2) (-height / 2) width height Cell.Empty
        }

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
