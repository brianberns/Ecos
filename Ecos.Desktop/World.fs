namespace Ecos.Desktop

open System
open Ecos.Engine

module World =

    /// Initial cluster tightness.
    let tightness = 2.0

    let hydrogen =
        AtomType.brushMap.Keys
            |> Seq.where (fun typ -> typ.Id = 1)
            |> Seq.exactlyOne

    let oxygen =
        AtomType.brushMap.Keys
            |> Seq.where (fun typ -> typ.Id = 2)
            |> Seq.exactlyOne

    /// Creates atoms.
    let createAtoms (random : Random) extentMin extentMax =
        let xDim = 2
        let yDim = 6
        let offset = Point.create (World.bondDistance / 2.0) 0.0

        let createType typ extentMin extentMax =
            let extentMin, extentMax =
                let offset = ((extentMax : Point) - (extentMin : Point)) / 5.0
                extentMin + offset, extentMax - offset
            [|
                for i = 0 to xDim do
                    for j = 0 to yDim do
                        let center =
                            let x =
                                (extentMax.X - extentMin.X)
                                    * (float i / float xDim)
                                    + extentMin.X
                            let y =
                                (extentMax.Y - extentMin.Y)
                                    * (float j / float yDim)
                                    + extentMin.Y
                            Point.create x y
                        let velocity =
                            Point.create
                                (random.NextDouble() - 0.5)
                                (random.NextDouble() - 0.5)
                        Atom.create typ (center + offset) velocity
                        Atom.create typ (center - offset) velocity
            |]

        [|
            yield! createType
                hydrogen
                extentMin
                (Point.create
                    (extentMin.X + 0.5 * (extentMax.X - extentMin.X))
                    extentMax.Y)
            yield! createType
                oxygen
                (Point.create
                    (extentMin.X + 0.5 * (extentMax.X - extentMin.X))
                    extentMin.Y)
                extentMax
        |]

    /// Creates a world.
    let create random extentMin extentMax =

            // create atoms
        let atoms = createAtoms random extentMin extentMax

            // create and animate world
        World.create extentMin extentMax atoms

    /// Draws the given world.
    let draw ctx world =

            // draw atoms
        Array.iter
            (Atom.draw ctx)
            world.Atoms

            // draw bonds
        for i = 1 to world.Atoms.Length - 1 do
            let bondRow = world.Bonds[i]
            for j = 0 to i - 1 do
                assert(i > j)
                if bondRow[j] then
                    let atomA = world.Atoms[i]
                    let atomB = world.Atoms[j]
                    Atom.drawBond ctx atomA atomB
