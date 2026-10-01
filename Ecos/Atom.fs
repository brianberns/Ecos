namespace Ecos

open System

/// Atom type.
[<RequireQualifiedAccess>]
type AtomType = A | B | C | D

module AtomType =

    /// Indicates whether the given atom types can form
    /// a weak bond.
    let canPair atomTypeA atomTypeB =
        let atomTypeMin = min atomTypeA atomTypeB
        let atomTypeMax = max atomTypeA atomTypeB
        match atomTypeMin, atomTypeMax with
            | AtomType.A, AtomType.B
            | AtomType.C, AtomType.D -> true
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
