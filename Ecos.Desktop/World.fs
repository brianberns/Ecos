namespace Ecos.Desktop

open System
open Ecos.Engine

module World =

    let hydrogen =
        AtomType.brushMap.Keys
            |> Seq.where (fun typ -> typ.Valence = 1)
            |> Seq.exactlyOne

    let oxygen =
        AtomType.brushMap.Keys
            |> Seq.where (fun typ -> typ.Valence = 2)
            |> Seq.exactlyOne

    /// Creates atoms.
    let createAtoms (random : Random) extentMin extentMax numAtoms =
        [|
            Atom.create hydrogen (Point.create -2.0 0.0) (Point.create 0.1 0.0)
            Atom.create hydrogen (Point.create 2.0 0.0) (Point.create -0.1 0.0)
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
