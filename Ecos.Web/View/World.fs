namespace Ecos.Web

open Ecos

module World =

    /// Initial cluster tightness.
    let tightness = 2.5

    /// Creates particles.
    let createParticles random extent numParticles =

            // initial particle locations
        let scale =
            let factor = (min extent.X extent.Y) / tightness
            Point.create factor factor
        Particle.makeParticles random numParticles
            scale Point.Zero

    /// Creates a world.
    let create random extentMin extentMax numParticles =

            // create particles
        let particles =
            let extent = extentMax - extentMin
            createParticles random extent numParticles

            // create and animate world
        World.create extentMin extentMax particles
