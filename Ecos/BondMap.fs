namespace Ecos

/// Unique ID of an atom in a world.
type AtomId = int

/// Bidirectional bonds between atoms.
type BondMap = Map<AtomId, Set<AtomId>>

module BondMap =

    /// Answers the IDs of all atoms bonded with the given atom.
    let getBonds id (bondMap : BondMap) =
        let ids =
            bondMap
                |> Map.tryFind id
                |> Option.defaultValue Set.empty
        assert(
            Seq.forall (fun idB ->
                bondMap[idB].Contains(id))
                ids)
        ids

    /// Indicates whether the atoms with the given IDs are already
    /// bonded.
    let areBonded idA idB bondMap =
        assert(
            (getBonds idA bondMap).Contains(idB) =
                (getBonds idB bondMap).Contains(idA))
        (getBonds idA bondMap).Contains(idB)

    /// Determines whether a bond between the given atoms is valid.
    let testBond neighborhood atomA atomB =
        assert(
            Seq.forall (fun (x, y) ->
                x >= 0 && y >= 0)
                neighborhood)
        let diff = abs (atomA.Location - atomB.Location)
        Set.contains
            (max diff.X diff.Y, min diff.X diff.Y)
            neighborhood

    /// Tests validity of the given atom's bonds.
    let testBonds
        neighborhood id atom (atomMap : Map<_, _>) bondMap =
        getBonds id bondMap
            |> Seq.forall (fun idB ->
                let atomB = atomMap[idB]
                testBond neighborhood atom atomB)

    /// Creates a bidirectional bond between the given atoms.
    let createBond idA idB bondMap =
        assert(not (areBonded idA idB bondMap))

        let impl idA idB bondMap =
            let ids =
                Map.tryFind idA bondMap
                    |> Option.defaultValue Set.empty
                    |> Set.add idB
            Map.add idA ids bondMap

        bondMap
            |> impl idA idB
            |> impl idB idA
