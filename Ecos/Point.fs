namespace Ecos

open System
open System.Collections.Generic

/// Lattice point.
[<Struct; CustomEquality; CustomComparison>]
[<StructuredFormatDisplay("{String}")>]
type Point =
    {
        /// X-coordinate.
        X : int

        /// Y-coordinate.
        Y : int
    }

    /// Compares two points for equality.
    member inline this.Equals(other) =
        this.X = other.X
            && this.Y = other.Y

    /// Compares two points for equality.
    override this.Equals(obj) =
        match obj with
            | :? Point as other -> this.Equals(other)
            | _ -> false

    /// Hashes the point.
    override this.GetHashCode() =
        HashCode.Combine(this.X, this.Y)

    interface IEquatable<Point> with

        /// Compares two points for equality.
        member this.Equals(other) =
            this.Equals(other)

    /// Compares two points.
    member inline this.CompareTo(other) =
        let x = compare this.X other.X
        if x = 0 then
            compare this.Y other.Y
        else x

    interface IComparable<Point> with

        /// Compares two points.
        member this.CompareTo(other) =
            this.CompareTo(other)

    interface IComparable with

        /// Compares two points.
        member this.CompareTo(obj) =
            this.CompareTo(obj :?> Point)

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

    /// Dot product.
    static member (.*)(a, b) =
        (a.X * b.X) + (a.Y * b.Y)

    /// Cross product. A positive value indicates that `b` is
    /// counterclockwise from `a` (on the left, looking along
    /// `a`), a negative value indicates that `b` is counter-
    /// clockwise from `a`, and zero indicates that the two
    /// vectors are parallel or anti-parallel. (The scalar value
    /// is actually the z-coordinate of a vector perpendicular
    /// to the 2D plane of the two vectors, but we don't use
    /// it for that purpose.) 
    static member ( *^)(a, b) =
        (a.X * b.Y) - (a.Y * b.X)

module Point =

    /// Creates a point.
    let create (x, y) =
        { X = x; Y = y }
