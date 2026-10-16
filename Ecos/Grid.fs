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

/// An infinite 2D grid of mutable cells.
type Grid =
    private {
        Cells : Cell[(*x*), (*y*)]
    }

    /// Current min/max bounds of the grid for which memory
    /// backing exists.
    member internal this.Bounds =
        (this.Cells.GetLowerBound(0), this.Cells.GetLowerBound(1)),   // (minX, minY)
        (this.Cells.GetUpperBound(0), this.Cells.GetUpperBound(1))    // (maxX, maxY)

    /// Gets the cell at the given location. If the location
    /// has no backing, it is assumed to be empty.
    member this.Item(x, y) =
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

    /// An empty grid.
    let empty =
        { Cells = Array2D.create 0 0 Cell.Empty }

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
        { Cells = cells }

    /// Sets the value at the given location in the given grid,
    /// expanding backing if necessary.
    let setValue location value grid =

            // an array with a dimension of size 0 reports a lower
            // bound of 0 and an upper bound of -1 for that dimension,
            // no matter what base it was created with
        let len = grid.Cells.Length

            // expand grid backing?
        let x, y = location.X, location.Y
        let grid =
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
        {
            Cells = Array2D.copy grid.Cells
        }
