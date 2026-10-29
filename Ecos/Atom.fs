namespace Ecos

open System

/// Atom type.
[<RequireQualifiedAccess>]
type AtomType =
    | W | X | Y | Z   // coding atoms within a strand
    | P | Q           // non-coding "cap" atoms at each end of complete strand

/// An indivisible atom.
type Atom =
    {
        /// Atom type.
        Type : AtomType

        /// Unique identifier of the atom in the world.
        Id : int

        /// Location of the atom in the world.
        Location : Point
    }

module Atom =

    /// Creates an atom.
    let create id typ location =
        {
            Id = id
            Type = typ
            Location = location
        }

/// Unique ID of an atom in a world.
type AtomId = int
