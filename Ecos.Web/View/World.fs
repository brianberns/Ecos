namespace Ecos.Web

open System
open Browser
open Ecos

module World =

    /// Creates particles.
    let createParticles random width height numParticles =

            // initial particle locations
        let factor = (min width height) / 4.0
        let scale = Point.create factor factor
        Particle.makeParticles random numParticles
            scale Point.Zero

    /// Creates a world.
    let create random width height numParticles =

            // create particles
        let particles =
            createParticles random width height numParticles

            // create and animate world
        World.create particles
