namespace Ecos.Engine

/// A photon.
type Photon =
    {
        /// Photon location.
        Location : Point

        /// Photon velocity vector.
        Velocity : Point

        /// Photon energy.
        Energy : float
    }

module Photon =

    /// Constant speed of light.
    let speed = 1.0

    /// Creates a photon.
    let create location (direction : Point) energy =
        {
            Location = location
            Velocity =
                (direction / direction.Length)
                    * speed
            Energy = energy
        }
