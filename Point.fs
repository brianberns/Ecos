namespace Ecos

[<Struct>]
type Point =
    {
        X : int
        Y : int
    }

    static member (+)(a, b) =
        {
            X = a.X + b.X
            Y = a.Y + b.Y
        }

    static member (-)(a, b) =
        {
            X = a.X - b.X
            Y = a.Y - b.Y
        }

    static member Abs(pt) =
        {
            X = abs pt.X
            Y = abs pt.Y
        }

module Point =

    let create (x, y) =
        { X = x; Y = y }
