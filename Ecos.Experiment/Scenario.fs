namespace Ecos.Experiment

open System
open Ecos

type Scenario =
    {
        World : World
        Random : Random
    }

module Scenario =

    let private addVerticalWall x yStart yEnd world =
        assert(x % 2 = 0)
        assert(yStart % 2 = 0)
        assert(yEnd % 2 = 0)
        (world, [ yStart .. 2.. yEnd ])
            ||> Seq.fold (fun world y ->
                let loc = Point.create (x, y)
                World.addWall loc world)

    let private addHorizontalWall xStart xEnd y world =
        assert(xStart % 2 = 0)
        assert(xEnd % 2 = 0)
        assert(y % 2 = 0)
        (world, [ xStart .. 2 .. xEnd ])
            ||> Seq.fold (fun world x ->
                let loc = Point.create (x, y)
                World.addWall loc world)

    let private addAtom
        (rng : Random) (atomTypes : _[]) x y id bond world =
        assert(x % 2 = 0)
        assert(y % 2 = 0)
        let atomType =
            atomTypes[rng.Next(atomTypes.Length)]
        let loc = Point.create (x, y)
        let atom = Atom.create atomType loc
        let world = World.addAtom atom world
        if bond then
            World.addStrongBond (id - 1) id world
        else world

    let twoStrands =

        let rng = Random(0)
        let n = 40
        let dist = 6

        let world =
            let width = 2 * n + 6 * dist
            let height = 2 * n
            let height' = height - 2 * 2
            World.empty
                |> addHorizontalWall -(width/2) (width/2) -(height/2)
                |> addHorizontalWall -(width/2) (width/2) (height/2)
                |> addVerticalWall -(width/2) -(height'/2) (height'/2)
                |> addVerticalWall (width/2) -(height'/2) (height'/2)

            // first strand
        let world =
            (world, [ 0 .. n - 1 ])
                ||> Seq.fold (fun world i ->
                    let x = 2 * i - n
                    let atomTypes = [| AtomType.W; AtomType.Y |]
                    addAtom rng atomTypes x -dist i (i > 0) world)

            // second strand
        let world =
            (world, [ 0 .. n - 1 ])
                ||> Seq.fold (fun world i ->
                    let x = 2 * i - n
                    let atomTypes = [| AtomType.X; AtomType.Z |]
                    addAtom rng atomTypes x dist (n + i) (i > 0) world)

        {
            World = world
            Random = rng
        }
