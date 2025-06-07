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

    member this.Equals(other) =
        this.Valence = other.Valence

    member this.CompareTo(other) =
        compare this.Valence other.Valence

    override this.Equals(other) =
        this.Equals(other :?> AtomType)

    override this.GetHashCode() = 
        this.Valence.GetHashCode()

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
            Acceleration = Point.zero
        }

    /// Resets an atom to have no bonds.
    let resetBonds atom =
        { atom with NumBonds = 0 }

    /// Elasticity of bond collision.
    let private elasticity = 0.0

    /// Bonds the given atoms.
    let bond atomA atomB radiate =
        assert(atomA.NumBonds < atomA.Type.Valence)
        assert(atomB.NumBonds < atomB.Type.Valence)
        let nBonds =
            min
                (atomA.Type.Valence - atomA.NumBonds)
                (atomB.Type.Valence - atomB.NumBonds)
        { atomA with
            NumBonds = atomA.NumBonds + nBonds
            Velocity =
                if radiate then
                    ((atomA.Velocity * (1.0 - elasticity))
                        + (atomB.Velocity * (1.0 + elasticity)))
                        / 2.0
                else atomA.Velocity },
        { atomB with
            NumBonds = atomB.NumBonds + nBonds
            Velocity =
                if radiate then
                    ((atomA.Velocity * (1.0 + elasticity))
                        + (atomB.Velocity * (1.0 - elasticity)))
                        / 2.0
                else atomB.Velocity },
        nBonds

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
