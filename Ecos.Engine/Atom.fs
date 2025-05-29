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

        /// Atom velocity.
        Velocity : Point
    }

module Atom =

    //// Creates an atom.
    let create typ location velocity =
        {
            Type = typ
            NumBonds = 0
            Location = location
            Velocity = velocity
        }

    /// Resets an atom to have no bonds.
    let resetBonds atom =
        { atom with NumBonds = 0 }

    /// Elasticity of bond collision.
    let private elasticity = 0.0

    /// Bonds the given atoms.
    let bond a b radiate =
        assert(a.NumBonds < a.Type.Valence)
        assert(b.NumBonds < b.Type.Valence)
        { a with
            NumBonds = a.NumBonds + 1
            Velocity =
                if radiate then
                    ((a.Velocity * (1.0 - elasticity))
                        + (b.Velocity * (1.0 + elasticity)))
                        / 2.0
                else a.Velocity },
        { b with
            NumBonds = b.NumBonds + 1
            Velocity =
                if radiate then
                    ((a.Velocity * (1.0 + elasticity))
                        + (b.Velocity * (1.0 - elasticity)))
                        / 2.0
                else b.Velocity }
