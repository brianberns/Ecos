namespace Ecos.Engine

type Photon =
    {
        /// Photon location.
        Location : Point

        /// Photon velocity vector.
        Velocity : Point
    }

module Photon =

    /// Constant speed of light.
    let speed = 1.0

    /// Creates a photon.
    let create location (direction : Point) =
        {
            Location = location
            Velocity =
                (direction / direction.Length)
                    * speed
        }