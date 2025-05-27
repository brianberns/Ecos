namespace Ecos

open System

/// Particle type.
[<CustomComparison; CustomEquality>]
type ParticleType =
    {
        /// Number of bonds a particle of this type is capable
        /// of making.
        Valence : int

        /// Color of this particle type.
        Color : string
    }

    override this.Equals(other) =
        this.Color = (other :?> ParticleType).Color

    override this.GetHashCode() = 
        this.Color.GetHashCode()

    interface IEquatable<ParticleType> with
        member this.Equals(other) =
            this.Color = other.Color

    interface IComparable with
        member this.CompareTo(other) =
            compare this.Color (other :?> ParticleType).Color

    interface IComparable<ParticleType> with
        member this.CompareTo(other) =
            compare this.Color other.Color

module ParticleType =

    /// Creates a particle type.
    let create valence color =
        {
            Valence = valence
            Color = color
        }

/// A particle.
type Particle =
    {
        /// Particle type.
        Type : ParticleType

        /// Number of bonds this particle currently has.
        NumBonds : int

        /// Particle location.
        Location : Point

        /// Particle velocity.
        Velocity : Point
    }

module Particle =

    //// Creates a particle.
    let create typ location velocity =
        {
            Type = typ
            NumBonds = 0
            Location = location
            Velocity = velocity
        }

    /// Resets a particle to have no bonds.
    let resetBonds particle =
        { particle with NumBonds = 0 }

    /// Bonds the given particles.
    let bond a b =
        assert(a.NumBonds < a.Type.Valence)
        assert(b.NumBonds < b.Type.Valence)
        let nBonds =
            min
                (a.Type.Valence - a.NumBonds)
                (b.Type.Valence - b.NumBonds)
        { a with
            NumBonds = a.NumBonds + nBonds
            Velocity = Point.Zero },
        { b with
            NumBonds = b.NumBonds + nBonds
            Velocity = Point.Zero }
