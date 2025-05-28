namespace Ecos.Desktop

open Avalonia.Media

open Ecos.Engine

module World =

    /// Initial cluster tightness.
    let tightness = 2.0

    let hydrogen =
        ParticleType.brushMap.Keys
            |> Seq.where (fun typ -> typ.Valence = 1)
            |> Seq.exactlyOne

    let oxygen =
        ParticleType.brushMap.Keys
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
    let drawBond (ctx : DrawingContext) a b =
        let pen = Pen(Brushes.Black, thickness = 0.05)
        ctx.DrawLine(
            pen,
            a.Location.ToAvalonia(),
            b.Location.ToAvalonia())

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
