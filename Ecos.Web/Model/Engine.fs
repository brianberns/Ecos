namespace Ecos

/// World of objects to animate.
type World =
    {
        Particles : Point[]
    }

module World =

    /// Creates a world.
    let create particles =
        {
            Particles = particles
        }

module Engine =

    /// Moves the particles in the given world one time step
    /// forward.
    let step world =
        (world : World)
