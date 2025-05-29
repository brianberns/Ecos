namespace Ecos.Desktop

open Avalonia.Media

open Ecos.Engine

module World =

    /// Initial cluster tightness.
    let tightness = 2.0

    let hydrogen =
        AtomType.brushMap.Keys
            |> Seq.where (fun typ -> typ.Valence = 1)
            |> Seq.exactlyOne

    let oxygen =
        AtomType.brushMap.Keys
            |> Seq.where (fun typ -> typ.Valence = 2)
            |> Seq.exactlyOne

    /// Creates atoms.
    let createAtoms random extent numAtoms =
        let scale =
            let factor = (min extent.X extent.Y) / tightness
            Point.create factor factor
        [|
            yield! Atom.makeAtoms
                random
                hydrogen
                (2 * numAtoms / 3)
                scale
                Point.Zero
            yield! Atom.makeAtoms
                random
                oxygen
                (numAtoms / 3)
                scale
                Point.Zero
        |]

    /// Creates a world.
    let create random extentMin extentMax numAtoms =

            // create atoms
        let atoms =
            let extent = extentMax - extentMin
            createAtoms random extent numAtoms

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
