namespace Ecos.Web

open Ecos

module World =

    /// Initial cluster tightness.
    let tightness = 2.0

    let hydrogen = ParticleType.all[0]
    assert(hydrogen.Valence = 1)

    let oxygen = ParticleType.all[1]
    assert(oxygen.Valence = 2)

    /// Creates particles.
    let createParticles random extent numParticles =
        [|
            Particle.create hydrogen (Point.create -1.5 0.0) (Point.create  0.1 0.0)
            Particle.create oxygen   (Point.create  1.5 0.0) (Point.create -0.1 0.0)
        |]

    /// Creates a world.
    let create random extentMin extentMax numParticles =

        let bondTypePriorityMap =
            Map [
                (hydrogen, oxygen), 1
                (oxygen, hydrogen), 1
            ]

            // create particles
        let particles =
            let extent = extentMax - extentMin
            createParticles random extent numParticles

            // create and animate world
        World.create
            extentMin extentMax
            particles bondTypePriorityMap
