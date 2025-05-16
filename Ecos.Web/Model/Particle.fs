namespace Ecos

type Particle =
    {
        Valence : int
        NumBonds : int
        Location : Point
    }

module Particle =

    let create valence location =
        {
            Valence = valence
            NumBonds = 0
            Location = location
        }

    let resetBonds particle =
        { particle with NumBonds = 0 }

    let bond particleA particleB =
        assert(particleA.NumBonds < particleA.Valence)
        assert(particleB.NumBonds < particleB.Valence)
        { particleA with
            NumBonds = particleA.NumBonds + 1 },
        { particleB with
            NumBonds = particleB.NumBonds + 1 }
