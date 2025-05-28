namespace Ecos.Desktop

open Avalonia.Media

open Ecos.Engine

module World =

    /// Initial cluster tightness.
    let tightness = 2.0

    let hydrogen = ParticleType.all[0]
    assert(hydrogen.Valence = 1)

    let oxygen = ParticleType.all[1]
    assert(oxygen.Valence = 2)

    /// Creates particles.
    let createParticles random extent numParticles =
        let scale =
            let factor = (min extent.X extent.Y) / tightness
            Point.create factor factor
        Particle.makeParticles
            random
            hydrogen
            numParticles
            scale
            Point.Zero

    /// Creates a world.
    let create random extentMin extentMax numParticles =

            // create particles
        let particles =
            let extent = extentMax - extentMin
            createParticles random extent numParticles

            // create and animate world
        World.create extentMin extentMax particles

    /// Draws the given world.
    let draw ctx world =

            // draw particles
        Array.iter
            (Particle.draw ctx)
            world.Particles

            // draw bonds
        let pen = Pen(Brushes.Black, thickness = 0.05)
        Set.iter (fun (i, j) ->
            let a = world.Particles[i]
            let b = world.Particles[j]
            ctx.DrawLine(
                pen,
                a.Location.ToAvalonia(),
                b.Location.ToAvalonia())
            ) world.Bonds
