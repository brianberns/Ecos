namespace Ecos

open System

/// Atom type.
[<RequireQualifiedAccess>]
type AtomType = W | X | Y | Z

module AtomType =

    /// Indicates whether the given atom types can form
    /// a weak bond.
    let canPair atomTypeA atomTypeB =
        let atomTypeMin = min atomTypeA atomTypeB
        let atomTypeMax = max atomTypeA atomTypeB
        match atomTypeMin, atomTypeMax with
            | AtomType.W, AtomType.X
            | AtomType.Y, AtomType.Z -> true
            | _ -> false

/// An indivisible atom.
type Atom =
    {
        /// Atom type.
        Type : AtomType

        /// Location of the atom in the world.
        Location : Point
    }

module Atom =

    /// Creates an atom.
    let create typ location =
        {
            Type = typ
            Location = location
        }
