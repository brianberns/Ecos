namespace Ecos.Engine

open System

/// Particle type.
[<CustomComparison; CustomEquality>]
type ParticleType =
    {
        /// Number of bonds a particle of this type is capable
        /// of making.
        Valence : int
    }

    override this.Equals(other) =
        this.Valence = (other :?> ParticleType).Valence

    override this.GetHashCode() = 
        this.Valence.GetHashCode()

    interface IEquatable<ParticleType> with
        member this.Equals(other) =
            this.Valence = other.Valence

    interface IComparable with
        member this.CompareTo(other) =
            compare this.Valence (other :?> ParticleType).Valence

    interface IComparable<ParticleType> with
        member this.CompareTo(other) =
            compare this.Valence other.Valence

module ParticleType =

    /// Creates a particle type.
    let create valence =
        {
            Valence = valence
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

    /// Elasticity of bond collision.
    let private elasticity = 0.0

    /// Bonds the given particles.
    let bond a b radiate =
        assert(a.NumBonds < a.Type.Valence)
        assert(b.NumBonds < b.Type.Valence)
        let nBonds =
            min
                (a.Type.Valence - a.NumBonds)
                (b.Type.Valence - b.NumBonds)
        { a with
            NumBonds = a.NumBonds + nBonds
            Velocity =
                if radiate then
                    ((a.Velocity * (1.0 - elasticity))
                        + (b.Velocity * (1.0 + elasticity)))
                        / 2.0
                else a.Velocity },
        { b with
            NumBonds = b.NumBonds + nBonds
            Velocity =
                if radiate then
                    ((a.Velocity * (1.0 + elasticity))
                        + (b.Velocity * (1.0 - elasticity)))
                        / 2.0
                else b.Velocity }
