namespace Ecos.Experiment

open System
open Ecos

type Phase =
    {
        /// Temperature scaled by Boltzmann's constant (kT).
        Temperature : float

        /// Number of sweeps to run at this temperature.
        NumSweeps : int
    }

type TemperatureType =

    /// Only the initial temperature is specified.
    | Adjustable of float

    /// Phases to run in a loop.
    | Cycle of Phase[]

type Scenario =
    {
        World : World
        Template : AtomId[]
        TemperatureType : TemperatureType
        BondParameterMap : Map<BondType, BondTypeParameters>
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

    let private addAtom atomType x y withBond world =
        assert(x % 2 = 0)
        assert(y % 2 = 0)
        let id = world.AtomMap.Count
        let loc = Point.create (x, y)
        let atom = Atom.create atomType loc
        let world = World.addAtom atom world
        if withBond then
            World.addStrongBond (id - 1) id world
        else world

    let private addAtomRnd
        (rng : Random) (atomTypes : _[]) x y withBond world =
        let atomType =
            atomTypes[rng.Next(atomTypes.Length)]
        addAtom atomType x y withBond world

    let private createScenario withTemplate seed =

        let rng = Random(seed)
        let nTemplate = 10
        let dist = 6

        let world = addBox 160 160 World.empty

            // template
        let world, template =
            if withTemplate then

                    // cap on start
                let world =
                    addAtom AtomType.P (2 * -1 - nTemplate) 0 false world

                    // random sequence
                let atomTypes = [| AtomType.W; AtomType.Y |]
                let world =
                    (world, [ 0 .. nTemplate - 1 ])
                        ||> Seq.fold (fun world i ->
                            let x = 2 * i - nTemplate
                            addAtomRnd rng atomTypes x 0 true world)

                    // cap on end
                let world =
                    addAtom AtomType.P (2 * nTemplate - nTemplate) 0 true world

                    // gather IDs
                let template =
                    world.AtomMap
                        |> Map.toSeq
                        |> Seq.map fst
                        |> Seq.toArray
                world, template
            else
                world, Array.empty

            // free weak partners w/ positive y-coord
        let nFree = 3 * nTemplate
        let world =
            (world, [ 0 .. nFree - 1 ])
                ||> Seq.fold (fun world i ->
                    let x = 2 * i - nFree
                    addAtom AtomType.X x dist false world)
        let world =
            (world, [ 0 .. nFree - 1 ])
                ||> Seq.fold (fun world i ->
                    let x = 2 * i - nFree
                    addAtom AtomType.X x (2 * dist) false world)

            // free weak partners w/ negative y-coord
        let world =
            (world, [ 0 .. nFree - 1 ])
                ||> Seq.fold (fun world i ->
                    let x = 2 * i - nFree
                    addAtom AtomType.Z x -dist false world)
        let world =
            (world, [ 0 .. nFree - 1 ])
                ||> Seq.fold (fun world i ->
                    let x = 2 * i - nFree
                    addAtom AtomType.Z x -(2 * dist) false world)

            // free strong partners w/ positive y-coord
        let world =
            (world, [ 0 .. nFree - 1 ])
                ||> Seq.fold (fun world i ->
                    let x = 2 * i - nFree
                    addAtom AtomType.Y x (3 * dist) false world)
        let world =
            (world, [ 0 .. nFree - 1 ])
                ||> Seq.fold (fun world i ->
                    let x = 2 * i - nFree
                    addAtom AtomType.Y x (4 * dist) false world)

            // free strong partners w/ negative y-coord
        let world =
            (world, [ 0 .. nFree - 1 ])
                ||> Seq.fold (fun world i ->
                    let x = 2 * i - nFree
                    addAtom AtomType.W x -(3 * dist) false world)
        let world =
            (world, [ 0 .. nFree - 1 ])
                ||> Seq.fold (fun world i ->
                    let x = 2 * i - nFree
                    addAtom AtomType.W x -(4 * dist) false world)

            // free caps w/ positive y-coord
        let world =
            (world, [ 0 .. nFree - 1 ])
                ||> Seq.fold (fun world i ->
                    let x = 2 * i - nFree
                    addAtom AtomType.P x (5 * dist) false world)

            // free caps w/ negative y-coord
        let world =
            (world, [ 0 .. nFree - 1 ])
                ||> Seq.fold (fun world i ->
                    let x = 2 * i - nFree
                    addAtom AtomType.Q x -(5 * dist) false world)

        let bondParamMap =
            Map [
                Strong, {
                    Energy = 17
                    Barrier = 3
                    BarrierStackReduction = 3
                }
                Weak, {
                    Energy = 1
                    Barrier = 0
                    BarrierStackReduction = 0
                }
            ]

        {
            World = world
            Template = template
            TemperatureType = Adjustable 0.25
            BondParameterMap = bondParamMap
            Random = rng
        }

    let createExperiment1 seed =
        createScenario true seed

    let createExperiment2 seed =
        createScenario false seed

    let createExperiment3 seed =

        let scenario = createExperiment1 seed

        let tempType =
            Cycle [|
                { Temperature = 0.3; NumSweeps = 100_000 }   // cold
                { Temperature = 3.0; NumSweeps = 100_000 }   // hot
            |]

        let bondParamMap =
            Map [
                Strong, {
                    Energy = 34
                    Barrier = 40
                    BarrierStackReduction = 40
                }
                Weak, {
                    Energy = 1
                    Barrier = 0
                    BarrierStackReduction = 0
                }
            ]

        { scenario with
            TemperatureType = tempType
            BondParameterMap = bondParamMap }
