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

/// A 2D grid of mutable cells. The grid is a torus (i.e. edges
/// wrap around), so coordinates are normalized to the range
/// (0,0) to (size.X - 1, size.Y - 1), inclusive.
type Grid =
    private {
        Cells : Cell[(*x*), (*y*)]
    }

    /// Size of this grid.
    member inline this.Size =  
        Point.create(
            this.Cells.GetLength(0),
            this.Cells.GetLength(1))

    /// Normalizes the given location onto this grid.
    member internal this.NormalizeLocation(location) =

        /// Normalizes the given coordinate into [0, limit).
        let norm limit coord =
            let result =
                if coord >= limit then coord - limit
                elif coord < 0 then coord + limit
                else coord
            assert(result >= 0)
            assert(result < limit)
            result

        let size = this.Size
        Point.create (
            norm size.X location.X,
            norm size.Y location.Y)

    /// Gets the cell at the given location.
    member this.Item(location) =
        let loc = this.NormalizeLocation(location)
        this.Cells[loc.X, loc.Y]

    /// Gets the cell at the given location.
    member this.Item(x, y) =
        this[Point.create(x, y)]

    /// Sets the cell at the given location. This mutates the
    /// grid.
    member internal this.SetValue(location, cell) =
        let loc = this.NormalizeLocation(location)
        this.Cells[loc.X, loc.Y] <- cell

    /// Normalizes the given vector onto this grid, taking care
    /// not to change its direction.
    member internal this.NormalizeVector(location) =

        /// Normalizes the given coordinate into [-limit/2, limit/2).
        let norm limit coord =
            let half = limit / 2
            let coord = coord + half
            let coord =
                if coord >= limit then coord - limit
                elif coord < 0 then coord + limit
                else coord
            let result = coord - half
            assert(result >= -half)
            assert(result < half)
            result

        let size = this.Size
        Point.create (
            norm size.X location.X,
            norm size.Y location.Y)

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
        grid.NormalizeVector(locB - locA)

    /// Sets the given location to contain an atom with the given
    /// ID. This mutates the grid.
    let setAtom location id (grid : Grid) =
        grid.SetValue(location, Cell.Atom id)

    /// Sets the given location to contain a wall. This mutates the grid.
    let setWall location (grid : Grid) =
        grid.SetValue(location, Cell.Wall)

    /// Sets the given location to be an empty space. This mutates the
    /// grid.
    let setEmpty location (grid : Grid) =
        grid.SetValue(location, Cell.Empty)

    /// Copies the given grid.
    let copy grid =
        { Cells = Array2D.copy grid.Cells }
