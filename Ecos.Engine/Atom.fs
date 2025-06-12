namespace Ecos.Engine

open System

/// Atom type.
[<CustomComparison; CustomEquality>]
type AtomType =
    {
        /// Unique 0-based index.
        Index : int

        /// Mass of atoms of this type.
        Mass : float

        /// Number of bonds an atom of this type is capable
        /// of making.
        Valence : int
    }

    (* An atom type is identified by its unique index *)

    member this.Equals(other) =
        this.Index = other.Index

    member this.CompareTo(other) =
        compare this.Index other.Index

    override this.Equals(other) =
        this.Equals(other :?> AtomType)

    override this.GetHashCode() = 
        this.Index.GetHashCode()

    interface IEquatable<AtomType> with
        member this.Equals(other) =
            this.Equals(other)

    interface IComparable with
        member this.CompareTo(other) =
            this.CompareTo(other :?> AtomType)

    interface IComparable<AtomType> with
        member this.CompareTo(other) =
            this.CompareTo(other)

module AtomType =

    /// Creates an atom type.
    let create index valence =
        {
            Index = index
            Mass = 1.0
            Valence = valence
        }

/// An atom.
type Atom =
    {
        /// Atom type.
        Type : AtomType

        /// Atom location.
        Location : Point

        /// Atom velocity vector.
        Velocity : Point

        /// Atom acceleration vector.
        Acceleration : Point

        /// Number of bonds this atom currently has.
        NumBonds : int
    }

module Atom =

    //// Creates an atom.
    let createBound typ location velocity numBonds =
        assert(numBonds <= typ.Valence)
        {
            Type = typ
            Location = location
            Velocity = velocity
            Acceleration = Point.zero
            NumBonds = numBonds
        }

    //// Creates an atom.
    let create typ location velocity =
        createBound typ location velocity 0

    /// Resets an atom to have no bonds.
    let resetBonds atom =
        { atom with NumBonds = 0 }

    /// Tries to bond the given atoms.
    let tryBond atomA atomB =
        let nBonds =
            min
                (atomA.Type.Valence - atomA.NumBonds)
                (atomB.Type.Valence - atomB.NumBonds)
        if nBonds > 0 then
            { atomA with
                NumBonds = atomA.NumBonds + nBonds },
            { atomB with
                NumBonds = atomB.NumBonds + nBonds },
            nBonds
        else atomA, atomB, nBonds

    /// Updates an atom's velocity by a half-step.
    let updateHalfStepVelocity dt atom =
        let velocity =
            atom.Velocity + atom.Acceleration * (dt / 2.0)
        { atom with Velocity = velocity }

    /// Updates an atom's location based on its velocity.
    let updateLocation (dt : float) atom =
        let location =
            atom.Location + atom.Velocity * dt
        { atom with Location = location }
