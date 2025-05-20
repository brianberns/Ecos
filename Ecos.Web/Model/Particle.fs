namespace Ecos

/// A particle.
type Particle =
    {
        /// Number of bonds this particle is capable of making.
        Valence : int

        /// Number of bonds this particle currently has.
        NumBonds : int

        /// Particle location.
        Location : Point

        /// Particle momentum.
        Momentum : Point
    }

module Particle =

    //// Creates a particle.
    let create valence location =
        {
            Valence = valence
            NumBonds = 0
            Location = location
            Momentum = Point.Zero
        }

    /// Resets a particle to have no bonds.
    let resetBonds particle =
        { particle with NumBonds = 0 }

    /// Bonds the given particles.
    let bond a b =
        assert(a.NumBonds < a.Valence)
        assert(b.NumBonds < b.Valence)
        { a with NumBonds = a.NumBonds + 1 },
        { b with NumBonds = b.NumBonds + 1 }
