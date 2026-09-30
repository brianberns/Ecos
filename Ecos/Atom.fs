namespace Ecos

open System

/// Atom type.
type AtomType =

    /// Atoms that stick together.
    | Hydrophobic

    /// Non-sticky atoms.
    | Polar

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
