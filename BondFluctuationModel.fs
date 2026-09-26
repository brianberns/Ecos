namespace Ecos

type AtomId = int

[<Struct>]
type Point =
    {
        X : int
        Y : int
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

    let create x y =
        { X = x; Y = y }

    let ofTuple (x, y) =
        create x y

type Atom =
    {
        Location : Point
    }

module Atom =

    let create location =
        {
            Location = location
        }

type World =
    {
        Atoms : Atom[]
        Bonds : Map<AtomId, AtomId>
    }

module World =

    let initialize atoms =
        {
            Atoms = Seq.toArray atoms
            Bonds = Map.empty
        }

    let tryAddBond idxA idxB world =
        let diff =
            let diff =
                let a = world.Atoms[idxA]
                let b = world.Atoms[idxB]
                abs (a.Location - b.Location)
            Point.create
                (max diff.X diff.Y)
                (min diff.X diff.Y)
        match diff.X, diff.Y with
            | 2,0
            | 2,1
            | 2,2
            | 3,0 ->
                Some {
                    world with
                        Bonds =
                            world.Bonds
                                |> Map.add idxA idxB
                                |> Map.add idxB idxA
                }
            | _ -> None

    let addBond idxA idxB world =
        match tryAddBond idxA idxB world with
            | Some world -> world
            | None -> failwith "Invalid bond"

    let getBonds world =
        world.Bonds
            |> Map.toSeq
            |> Seq.map (fun (idxA, idxB) ->
                world.Atoms[idxA], world.Atoms[idxB])
