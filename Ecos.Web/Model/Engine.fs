namespace Ecos

type Particle = Point

/// World of objects to animate.
type World =
    {
        Particles : Particle[]
    }

module World =

    /// Creates a world.
    let create particles =
        {
            Particles = particles
        }

module Engine =

    /// Calculates the repulsion between two particles.
    let getRepulsion particleA particleB =
        assert(particleA <> particleB)
        let vector : Point = particleA - particleB
        let length = vector.Length
        if length < 1.0 then
            (1.0 - length) * (vector / length)   // strength * unit vector
        else Point.Zero

    let dt = 0.1

    /// Moves the particles in the given world one time step
    /// forward.
    let step world =

            // compute the upper triangle of the lookup table
        let particles = world.Particles
        let nParticles = particles.Length
        let upper =
            Array.init nParticles (fun i ->
                let particle = particles[i]
                Array.init (nParticles - i) (fun offset ->
                    if offset = 0 then Point.Zero
                    else
                        getRepulsion particle particles[i + offset]))

            // full lookup table
        let lookup i j =
            if i <= j then upper[i][j - i]
            else -upper[j][i - j]

        let particles =
            Array.init nParticles (fun i ->
                let delta =
                    Array.init nParticles (lookup i)
                        |> Array.sum
                particles[i] + (delta * dt))

        { Particles = particles }
