namespace Ecos.Web

open System
open Browser
open Ecos

module World =

    /// Creates particles.
    let createParticles width height numParticles =

            // random number generator
        let random =
            let seed = DateTime.Now.Millisecond
            console.log($"Random seed: {seed}")
            Random(seed)

            // initial particle locations
        let factor = (min width height) / 4.0
        let scale = Point.create factor factor
        Particle.makeParticles random numParticles
            scale Point.Zero

    /// Creates a world.
    let create width height numParticles =

            // create particles
        let particles =
            createParticles width height numParticles

            // create and animate world
        World.create particles
