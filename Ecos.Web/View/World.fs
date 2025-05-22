namespace Ecos.Web

open Ecos

module World =

    /// Initial cluster tightness.
    let tightness = 4.0

    let hydrogen = ParticleType.all[0]
    assert(hydrogen.Valence = 1)

    let oxygen = ParticleType.all[1]
    assert(oxygen.Valence = 2)

    /// Creates particles.
    let createParticles random extent numParticles =

            // initial particle locations
        let scale =
            let factor = (min extent.X extent.Y) / tightness
            Point.create factor factor
        [|
            yield! Particle.makeParticles
                random
                hydrogen
                (2 * numParticles / 3)
                scale
                (Point.create -(extent.X / 4.0) 0.0)
            yield! Particle.makeParticles
                random
                oxygen
                (numParticles / 3)
                scale
                (Point.create (extent.X / 4.0) 0.0)
        |]

    /// Creates a world.
    let create random extent numParticles =

        let bondTypePriorityMap =
            Map [
                (hydrogen, oxygen), 1
                (oxygen, hydrogen), 1
            ]

            // create particles
        let particles =
            createParticles random extent numParticles

            // create and animate world
        World.create extent particles bondTypePriorityMap
