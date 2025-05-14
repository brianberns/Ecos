namespace Ecos.Web

open Ecos

module World =

    /// Creates particles.
    let createParticles random extent numParticles =

            // initial particle locations
        let factor = (min extent.X extent.Y) / 4.0
        let scale = Point.create factor factor
        Particle.makeParticles random numParticles
            scale Point.Zero

    /// Creates a world.
    let create random extent numParticles =

            // create particles
        let particles =
            createParticles random extent numParticles

            // create and animate world
        World.create extent particles
