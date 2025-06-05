namespace Ecos.Engine

/// 2D point.
[<StructuredFormatDisplay("{String}")>]
type Point =
    {
        /// X-coordinate.
        X : float

        /// Y-coordinate.
        Y : float
    }

    /// Origin.
    static member val zero = { X = 0; Y = 0 }

    /// Origin.
    static member Zero = Point.zero

    /// Negates a point.
    static member inline (~-)(a) =
        { X = -a.X; Y = -a.Y }
    
    /// Adds two points component-wise.
    static member inline (+)(p1, p2) =
        { X = p1.X + p2.X; Y = p1.Y + p2.Y }
    
    /// Subtracts one point from another component-wise.
    static member inline (-)(p1, p2) =
        { X = p1.X - p2.X; Y = p1.Y - p2.Y }
    
    /// Multiplies a point by a scalar.
    static member inline (*)(p, a) =
        { X = p.X * a; Y = p.Y * a }
    
    /// Multiplies a scalar by a point.
    static member inline (*)(a, p) =
        { X = a * p.X; Y = a * p.Y }
    
    /// Multiplies one point by another component-wise.
    static member inline (*)(p1, p2) =
        { X = p1.X * p2.X; Y = p1.Y * p2.Y }
    
    /// Divides a point by a scalar.
    static member inline (/)(p, a) =
        { X = p.X / a; Y = p.Y / a }

    /// Computes the length of a point when considered
    /// as a vector.
    member this.Length =
        sqrt (this.X * this.X + this.Y * this.Y)

    /// Display string.
    member this.String =
        $"(%0.3g{this.X}, %0.3g{this.Y})"

    /// Display string.
    override this.ToString() = 
        this.String

module Point =

    /// Creates a point.
    let create x y =
        { X = x; Y = y }
