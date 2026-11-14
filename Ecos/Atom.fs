namespace Ecos

open System

/// Atom type.
[<RequireQualifiedAccess>]
type AtomType =
    | W | X | Y | Z   // coding atoms within a strand
    | P | Q           // non-coding "cap" atoms at each end of complete strand

/// Unique ID of an atom in a world.
type AtomId = int

/// An indivisible atom.
type Atom =
    {
        /// Unique identifier of the atom in the world.
        Id : AtomId

        /// Atom type.
        Type : AtomType

        /// Location of the atom in the world.
        Location : Point

        /// Indicates whether the atom can move.
        IsMobile : bool
    }

module Atom =

    /// Creates an atom.
    let create id typ location isMobile =
        {
            Id = id
            Type = typ
            Location = location
            IsMobile = isMobile
        }
