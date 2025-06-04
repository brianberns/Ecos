namespace Ecos.Engine

open System

/// Atom type.
[<CustomComparison; CustomEquality>]
type AtomType =
    {
        /// Number of bonds an atom of this type is capable
        /// of making.
        Valence : int
    }

    override this.Equals(other) =
        this.Valence = (other :?> AtomType).Valence

    override this.GetHashCode() = 
        this.Valence.GetHashCode()

    interface IEquatable<AtomType> with
        member this.Equals(other) =
            this.Valence = other.Valence

    interface IComparable with
        member this.CompareTo(other) =
            compare this.Valence (other :?> AtomType).Valence

    interface IComparable<AtomType> with
        member this.CompareTo(other) =
            compare this.Valence other.Valence

module AtomType =

    /// Creates an atom type.
    let create valence =
        {
            Valence = valence
        }

/// An atom.
type Atom =
    {
        /// Atom type.
        Type : AtomType

        /// Number of bonds this atom currently has.
        NumBonds : int

        /// Atom location.
        Location : Point

        /// Atom velocity vector.
        Velocity : Point

        /// Atom acceleration vector.
        Acceleration : Point
    }

module Atom =

    //// Creates an atom.
    let create typ location velocity =
        {
            Type = typ
            NumBonds = 0
            Location = location
            Velocity = velocity
            Acceleration = Point.Zero
        }

    /// Resets an atom to have no bonds.
    let resetBonds atom =
        { atom with NumBonds = 0 }

    /// Bonds the given atoms.
    let bond a b =
        assert(a.NumBonds < a.Type.Valence)
        assert(b.NumBonds < b.Type.Valence)
        let nBonds =
            min
                (a.Type.Valence - a.NumBonds)
                (b.Type.Valence - b.NumBonds)
        { a with
            NumBonds = a.NumBonds + nBonds },
        { b with
            NumBonds = b.NumBonds + nBonds }

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
