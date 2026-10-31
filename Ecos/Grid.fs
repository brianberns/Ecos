namespace Ecos

/// A 2D grid of mutable cells. The grid is a torus (i.e. edges
/// wrap around), so coordinates are normalized to the range
/// (0,0) to (size.X - 1, size.Y - 1), inclusive.
type Grid<'t> =
    private {
        Cells : 't[(*x*), (*y*)]
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

    /// Gets or sets the cell at the given location.
    member this.Item
        with get(location) =
            let loc = this.NormalizeLocation(location)
            this.Cells[loc.X, loc.Y]
        and set location value =
            let loc = this.NormalizeLocation(location)
            this.Cells[loc.X, loc.Y] <- value   // MUTATION

    /// Gets or sets the cell at the given location.
    member this.Item
        with get (x, y) =
            this[Point.create(x, y)]
        and set (x, y) value =
            this[Point.create(x, y) ] <- value   // MUTATION

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

    /// Creates a grid of the given size with all cells
    /// initialized to the given value.
    let create size value =
        {
            Cells = Array2D.create size.X size.Y value
        }

    /// The shortest vector from location A to location B in the
    /// given grid. Since the grid has no boundary, this vector
    /// might wrap around the edges.
    let getDisplacement locA locB (grid : Grid<_>) =
        grid.NormalizeVector(locB - locA)

    /// Copies the given grid.
    let copy grid =
        { Cells = Array2D.copy grid.Cells }
