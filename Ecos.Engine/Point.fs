namespace Ecos.Engine

/// 2D point.
[<StructuredFormatDisplay("{String}")>]
[<Struct>]
type Point =

    /// X-coordinate.
    val X : float

    /// Y-coordinate.
    val Y : float

    /// Constructor.
    new(x : float, y : float) = { X = x; Y = y }

#if !FABLE_COMPILER
    /// Origin.
    static member val zero = Point(0, 0)
#endif

    /// Origin.
    static member Zero =
#if FABLE_COMPILER
        Point(0, 0)
#else
        Point.zero
#endif

    /// Negates a point.
    static member inline (~-)(p : Point) =
        Point(-p.X, -p.Y)
    
    /// Adds two points component-wise.
    static member inline (+)(p1 : Point, p2 : Point) =
        Point(p1.X + p2.X, p1.Y + p2.Y)
    
    /// Subtracts one point from another component-wise.
    static member inline (-)(p1 : Point, p2 : Point) =
        Point(p1.X - p2.X, p1.Y - p2.Y)
    
    /// Multiplies a point by a scalar.
    static member inline (*)(p : Point, a) =
        Point(p.X * a, p.Y * a)
    
    /// Multiplies a scalar by a point.
    static member inline (*)(a, p : Point) =
        Point(a * p.X, a * p.Y)
    
    /// Multiplies one point by another component-wise.
    static member inline (*)(p1 : Point, p2 : Point) =
        Point(p1.X * p2.X, p1.Y * p2.Y)
    
    /// Divides a point by a scalar.
    static member inline (/)(p : Point, a) =
        Point(p.X / a, p.Y / a)

    /// Dot product.
    static member ( *.)(p1 : Point, p2 : Point) =
        p1.X * p2.X + p1.Y * p2.Y

    /// Computes the length of a point when considered
    /// as a vector.
    member this.Length =
        sqrt (this *. this)

    /// Display string.
    member this.String =
        $"(%0.3g{this.X}, %0.3g{this.Y})"

    /// Display string.
    override this.ToString() = 
        this.String

module Point =

    /// Creates a point.
    let inline create x y =
        Point(x, y)

#if FABLE_COMPILER
    /// Origin.
    let zero =
        Point.Zero
#endif
