namespace Ecos.Desktop

open System
open Ecos.Engine

module World =

    let hydrogen = AtomType.all[0]
    let oxygen = AtomType.all[1]

    /// Creates atoms.
    let createAtoms
        (random : Random)
        (extentMin : Point)
        (extentMax : Point)
        numAtoms =
        let extent = extentMax - extentMin
        let numPairs = numAtoms / 2
        let yDim = (float numPairs / (extent.X / extent.Y)) |> sqrt |> int
        let xDim = numPairs / yDim
        let mid = Point.create 0.5 0.5
        let box = Point.create (extent.X / float xDim) (extent.Y / float yDim)
        let offset = Point.create 0.6 0.0
        let atoms =
            [|
                for x = 0 to xDim - 1 do
                    for y = 0 to yDim - 1 do
                        let location =
                            let pt = Point.create (float x) (float y)
                            ((pt + mid) * box) + extentMin
                        let atomType =
                            if random.Next(3) = 0 then oxygen
                            else hydrogen
                        let velocity =
                            Point.create
                                (random.NextDouble())
                                (random.NextDouble())
                                - mid
                        Atom.createBound atomType (location - offset) velocity 1
                        Atom.createBound atomType (location + offset) velocity 1
            |]
        let bonds =
            Array.init atoms.Length (fun i ->
                Array.init i (fun j ->
                    assert(i >= j)
                    if i = j + 1 then 1
                    else 0))
        atoms, bonds

    /// Creates a world.
    let create random extentMin extentMax numAtoms =

            // create atoms
        let atoms, bonds =
            createAtoms random extentMin extentMax numAtoms

            // create and animate world
        World.createBound extentMin extentMax atoms bonds

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
                let nBonds = bondRow[j]
                if nBonds > 0 then
                    let atomA = world.Atoms[i]
                    let atomB = world.Atoms[j]
                    Atom.drawBond ctx atomA atomB nBonds

            // draw photons
        Array.iter
            (Photon.draw ctx)
            world.Photons
