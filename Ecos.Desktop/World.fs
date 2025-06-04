namespace Ecos.Desktop

open System
open Ecos.Engine

module World =

    let hydrogen = AtomType.all[0]
    let oxygen = AtomType.all[1]

    /// Creates atoms.
    let createAtoms (random : Random) extentMin extentMax numAtoms =
        let extent = extentMax - extentMin
        let yDim = (float numAtoms / (extent.X / extent.Y)) |> sqrt |> int
        let xDim = numAtoms / yDim
        let mid = Point.create 0.5 0.5
        let box = Point.create (extent.X / float xDim) (extent.Y / float yDim)
        [|
            for x = 0 to xDim - 1 do
                for y = 0 to yDim - 1 do
                    let location =
                        let pt = Point.create (float x) (float y)
                        ((pt + mid) * box) + extentMin
                    let atomType =
                        if x < 2 * xDim / 3 then hydrogen
                        else oxygen
                    let velocity =
                        Point.create
                            (random.NextDouble())
                            (random.NextDouble())
                            - mid
                    Atom.create atomType location velocity
        |]

    /// Creates a world.
    let create random extentMin extentMax numAtoms =

            // create atoms
        let atoms =
            createAtoms random extentMin extentMax numAtoms

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
