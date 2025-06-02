namespace Ecos.Desktop

open System
open Ecos.Engine

module World =

    /// Initial cluster tightness.
    let tightness = 2.0

    /// Creates atoms.
    let createAtoms (random : Random) extentMin extentMax numAtoms =
        let atomTypes = Seq.toArray AtomType.brushMap.Keys
        Array.init numAtoms (fun _ ->
            let pt =
                Point.create
                    (random.NextDouble())
                    (random.NextDouble())
            let atomType =
                if pt.X < 0.5 then atomTypes[0]
                else atomTypes[1]
            let location =
                (extentMax - extentMin) * pt + extentMin
            Atom.create atomType location Point.Zero)

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
