namespace Ecos

[<RequireQualifiedAccess>]
type Cell =
    | Atom of AtomId
    | Wall
    | Empty

/// A grid of cells.
type Grid =
    private {
        Cells : Cell[(*x*), (*y*)]
    }

    member internal this.Bounds =
        (this.Cells.GetLowerBound(0), this.Cells.GetLowerBound(1)),   // (minX, minY)
        (this.Cells.GetUpperBound(0), this.Cells.GetUpperBound(1))    // (maxX, maxY)

    member this.Item(x, y) =
        let (minX, minY), (maxX, maxY) = this.Bounds
        if x < minX || x > maxX
            || y < minY || y > maxY then
            Cell.Empty
        else
            this.Cells[x, y]

    member this.Item(location) =
        this[location.X, location.Y]

    member internal this.SetValue(x, y, cell) =
        this.Cells[x, y] <- cell

module Grid =

    let empty =
        { Cells = Array2D.create 0 0 Cell.Empty }

    // SLOW
    let getWalls (grid : Grid) =
        let (minX, minY), (maxX, maxY) = grid.Bounds
        seq {
            for x in [ minX .. maxX ] do
                for y in [ minY .. maxY ] do
                    match grid[x, y] with
                        | Cell.Wall -> yield Point.create(x, y)
                        | _ -> ()
        }

    let private adjust
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

    // an array with a dimension of size 0 reports a lower bound of 0 and an upper bound of -1 for that dimension, no matter what base it was created with
    let private setValue location value (grid : Grid) =

        let x, y = location.X, location.Y

        let grid =
            let (minX, minY), (maxX, maxY) = grid.Bounds
            let len = grid.Cells.Length
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
                adjust
                    (newMinX, newMinY)
                    (newMaxX - newMinX + 1, newMaxY - newMinY + 1)
                    grid

        grid.SetValue(x, y, value)   // MUTATION

        grid

    let setAtom location id grid =
        setValue location (Cell.Atom id) grid

    let setWall location grid =
        setValue location Cell.Wall grid

    let setEmpty location grid =
        setValue location Cell.Empty grid
