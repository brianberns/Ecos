namespace Ecos.Web

open Ecos

module World =

    /// Initial cluster tightness.
    let tightness = 4.0

    /// Creates particles.
    let createParticles random extent numParticles =

            // initial particle locations
        let scale =
            let factor = (min extent.X extent.Y) / tightness
            Point.create factor factor
        [|
            yield! Particle.makeParticles
                random
                ParticleType.all[0]
                (numParticles / 2)
                scale
                (Point.create -(extent.X / 4.0) 0.0)
            yield! Particle.makeParticles
                random
                ParticleType.all[1]
                (numParticles / 2)
                scale
                (Point.create (extent.X / 4.0) 0.0)
        |]

    /// Creates a world.
    let create random extent numParticles =

            // create particles
        let particles =
            createParticles random extent numParticles

            // create and animate world
        World.create extent particles
