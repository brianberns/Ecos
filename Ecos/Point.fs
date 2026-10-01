namespace Ecos

/// Lattice point.
[<Struct; StructuredFormatDisplay("{String}")>]
type Point =
    {
        /// X-coordinate.
        X : int

        /// Y-coordinate.
        Y : int
    }

    /// Display string.
    member this.String = $"({this.X}, {this.Y})"

    /// Display string.
    override this.ToString() = this.String

    /// Origin.
    static member val zero = { X = 0; Y = 0 }

    /// Origin.
    static member Zero = Point.zero

    /// Adds two points.
    static member (+)(a, b) =
        {
            X = a.X + b.X
            Y = a.Y + b.Y
        }

    /// Subtracts one point from another.
    static member (-)(a, b) =
        {
            X = a.X - b.X
            Y = a.Y - b.Y
        }

    /// Absolute value of a point.
    static member Abs(pt) =
        {
            X = abs pt.X
            Y = abs pt.Y
        }

    /// Cross product.
    static member (.*)(a, b) =
        (a.X * b.Y) - (a.Y * b.X)

module Point =

    /// Creates a point.
    let create (x, y) =
        { X = x; Y = y }
