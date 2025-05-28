namespace Ecos.Web

open Browser.Types
open Ecos.Engine

module World =

    /// Initial cluster tightness.
    let tightness = 2.0

    let hydrogen =
        ParticleType.colorMap.Keys
            |> Seq.where (fun typ -> typ.Valence = 1)
            |> Seq.exactlyOne

    let oxygen =
        ParticleType.colorMap.Keys
            |> Seq.where (fun typ -> typ.Valence = 2)
            |> Seq.exactlyOne

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

    /// Draws a bond between the given particles.
    let drawBond (ctx : CanvasRenderingContext2D) a b =
        ctx.beginPath()
        ctx.moveTo(a.Location.X, a.Location.Y)
        ctx.lineTo(b.Location.X, b.Location.Y)
        ctx.stroke()

    /// Draws the given world.
    let draw ctx world =

            // draw particles
        Array.iter
            (Particle.draw ctx)
            world.Particles

            // draw bonds
        for i = 1 to world.Particles.Length - 1 do
            for j = 0 to i - 1 do
                assert(i > j)
                if world.Bonds[i][j] then
                    let a = world.Particles[i]
                    let b = world.Particles[j]
                    drawBond ctx a b
