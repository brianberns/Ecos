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

/// A 2D grid of mutable cells. It is either infinite, or a torus of a
/// fixed size, centered on the origin, whose opposite edges meet.
type Grid =
    private {
        Cells : Cell[(*x*), (*y*)]

        /// Width and height of a torus, or zero for an infinite grid.
        Width : int
        Height : int
    }

    /// Whether this grid is a torus.
    member inline this.IsTorus = this.Width > 0

    /// Current min/max bounds of the grid for which memory
    /// backing exists.
    member inline internal this.Bounds =
        (this.Cells.GetLowerBound(0), this.Cells.GetLowerBound(1)),   // (minX, minY)
        (this.Cells.GetUpperBound(0), this.Cells.GetUpperBound(1))    // (maxX, maxY)

    /// Wraps the given coordinate into [-size/2, size/2).
    static member inline internal WrapCoord(coord, size) =
        let half = size / 2
        let c = (coord + half) % size
        (if c < 0 then c + size else c) - half

    /// Wraps the given location onto this grid, if it is a torus.
    member this.Wrap(x, y) =
        if this.IsTorus then
            Grid.WrapCoord(x, this.Width), Grid.WrapCoord(y, this.Height)
        else x, y

    /// Gets the cell at the given location. If the location
    /// has no backing, it is assumed to be empty.
    member this.Item(x, y) =
        let x, y = this.Wrap(x, y)
        let (minX, minY), (maxX, maxY) = this.Bounds
        if x < minX || x > maxX
            || y < minY || y > maxY then
            Cell.Empty
        else
            this.Cells[x, y]

    /// Gets the cell at the given location. If the location
    /// has no backing, it is assumed to be empty.
    member this.Item(location) =
        this[location.X, location.Y]

    /// Sets the cell at the given location. The location
    /// must have backing. This mutates the grid.
    member internal this.SetValue(x, y, cell) =
        this.Cells[x, y] <- cell

module Grid =

    /// An empty, infinite grid.
    let empty =
        {
            Cells = Array2D.create 0 0 Cell.Empty
            Width = 0
            Height = 0
        }

    /// An empty torus of the given (even) size, centered on the origin.
    let createTorus width height =
        assert(width > 0 && width % 2 = 0)
        assert(height > 0 && height % 2 = 0)
        {
            Cells =
                Array2D.createBased
                    (-width / 2) (-height / 2) width height Cell.Empty
            Width = width
            Height = height
        }

    /// Expands the given grid's backing to cover the given
    /// extent.
    let private expand
        (newBaseX, newBaseY) (newSizeX, newSizeY) (grid : Grid) =
        let (minX, minY), (maxX, maxY) = grid.Bounds
        let cells =
            Array2D.initBased
                newBaseX newBaseY newSizeX newSizeY (fun x y ->
                    if x >= minX && x <= maxX
                        && y >= minY && y <= maxY then
                        grid[x, y]
                    else Cell.Empty)
        { grid with Cells = cells }

    /// Sets the value at the given location in the given grid,
    /// expanding backing if necessary.
    let setValue location value (grid : Grid) =

            // a torus has all the backing it needs
        let x, y = grid.Wrap(location.X, location.Y)
        let grid =
            if grid.IsTorus then grid
            else

                    // an array with a dimension of size 0 reports a lower
                    // bound of 0 and an upper bound of -1 for that dimension,
                    // no matter what base it was created with
                let len = grid.Cells.Length

                    // expand grid backing?
                let (minX, minY), (maxX, maxY) = grid.Bounds
                if len > 0
                    && x >= minX && x <= maxX
                    && y >= minY && y <= maxY then
                    grid
                else
                    let newMinX, newMaxX =
                        if len = 0 then x, x
                        else min minX x, max maxX x
                    let newMinY, newMaxY =
                        if len = 0 then y, y
                        else min minY y, max maxY y
                    expand
                        (newMinX, newMinY)
                        (newMaxX - newMinX + 1, newMaxY - newMinY + 1)
                        grid

        grid.SetValue(x, y, value)   // MUTATION

        grid

    /// Sets the given location to contain an atom with the given
    /// ID.
    let setAtom location id grid =
        setValue location (Cell.Atom id) grid

    /// Sets the given location to contain a wall.
    let setWall location grid =
        setValue location Cell.Wall grid

    /// Sets the given location to be an empty space.
    let setEmpty location grid =
        setValue location Cell.Empty grid

    /// Copies the given grid.
    let copy grid =
        { grid with Cells = Array2D.copy grid.Cells }
