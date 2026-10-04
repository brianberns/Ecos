namespace Ecos.Experiment

open System
open Ecos

type Scenario =
    {
        World : World
        Template : AtomId[]
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

    let private addBox width height world =
        let height' = height - 2 * 2
        world
            |> addHorizontalWall -(width/2) (width/2) -(height/2)
            |> addHorizontalWall -(width/2) (width/2) (height/2)
            |> addVerticalWall -(width/2) -(height'/2) (height'/2)
            |> addVerticalWall (width/2) -(height'/2) (height'/2)

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
            addBox width height World.empty

            // first strand
        let world =
            let atomTypes = [| AtomType.W; AtomType.Y |]
            (world, [ 0 .. n - 1 ])
                ||> Seq.fold (fun world i ->
                    let x = 2 * i - n
                    addAtom rng atomTypes x -dist i (i > 0) world)

            // second strand
        let world =
            let atomTypes = [| AtomType.X; AtomType.Z |]
            (world, [ 0 .. n - 1 ])
                ||> Seq.fold (fun world i ->
                    let x = 2 * i - n
                    addAtom rng atomTypes x dist (n + i) (i > 0) world)

        {
            World = world
            Template = Array.empty
            Random = rng
        }

    let private createExperiment1or2 withTemplate seed =

        let rng = Random(seed)
        let nTemplate = 10
        let dist = 6

        let world =
            let width = 2 * nTemplate + 6 * dist
            let height = width
            addBox width height World.empty

            // free positive
        let world =
            let atomTypes = [| AtomType.X  |]
            (world, [ 0 .. nTemplate - 1 ])
                ||> Seq.fold (fun world i ->
                    let x = 2 * i - nTemplate
                    addAtom rng atomTypes x dist (nTemplate + i) false world)

            // free negative
        let world =
            let atomTypes = [| AtomType.Z |]
            (world, [ 0 .. nTemplate - 1 ])
                ||> Seq.fold (fun world i ->
                    let x = 2 * i - nTemplate
                    addAtom rng atomTypes x -dist (nTemplate + i) false world)

            // template
        let world, template =
            if withTemplate then
                let atomTypes = [| AtomType.W; AtomType.Y |]
                let world =
                    (world, [ 0 .. nTemplate - 1 ])
                        ||> Seq.fold (fun world i ->
                            let x = 2 * i - nTemplate
                            addAtom rng atomTypes x 0 i (i > 0) world)
                let template =
                    world.AtomMap
                        |> Map.toSeq
                        |> Seq.where (fun (id, atom) ->
                            Array.contains atom.Type atomTypes)
                        |> Seq.map fst
                        |> Seq.toArray
                world, template
            else
                world, Array.empty

        {
            World = world
            Template = template
            Random = rng
        }

    let createExperiment1 seed =
        createExperiment1or2 true seed

    let createExperiment2 () =
        createExperiment1or2 false 0   // seed is irrelevant
