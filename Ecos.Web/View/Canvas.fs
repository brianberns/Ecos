namespace Ecos.Web

open System

open Browser
open Browser.Types

open Ecos.Engine

module Canvas =

    /// Number of engine time steps per frame.
    let stepsPerFrame = 1

    /// Drawing canvas.
    let canvas =
        document.getElementById("canvas")
            :?> HTMLCanvasElement

    /// Drawing context.
    let ctx = canvas.getContext_2d()

    /// Extent of the world.
    let extentMin, extentMax =
        let width = 40.0
        let height =
            canvas.height * width / canvas.width
        let pt = (Point.create width height) / 2.0
        -pt, pt

    /// Reset world on next frame?
    let mutable reset = false

    /// Reset button.
    let btnReset =
        let btn =
            document.getElementById("reset")
                :?> HTMLButtonElement
        btn.onclick <- (fun _ -> reset <- true)
        btn

    /// Number of particles input.
    let txtNumParticles =
        document.getElementById("numParticles")
            :?> HTMLInputElement

    /// Creates a world.
    let createWorld random =
        let numParticles =
            Int32.Parse txtNumParticles.value
        Ecos.Web.World.create
            random extentMin extentMax numParticles

    /// Animates one frame.
    let animateFrame world =

            // advance world
        let world =
            (world, [1 .. stepsPerFrame])
                ||> Seq.fold (fun world _ ->
                    World.step world)

            // prepare to draw
        ctx.clearRect(0, 0, canvas.width, canvas.height)
        ctx.translate(canvas.width / 2.0, canvas.height / 2.0)
        let scale = canvas.width / (extentMax.X - extentMin.X)
        ctx.scale(scale, scale)
        ctx.lineWidth <- 1.0 / scale

            // draw the world
        World.draw ctx world

            // reset transform
        ctx.setTransform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)

        world

    /// Random number generator.
    let random =
        let seed = DateTime.Now.Millisecond
        console.log($"Random seed: {seed}")
        Random(seed)

    /// Log framerate.
    let logFramerate check iFrame prev cur =
        if iFrame % check = 0 then
            console.log(
                $"%.3f{float check * 1000.0 / (cur - prev)} frames/sec")
            cur
        else prev

    /// Animates a world.
    let animate () =

        /// Animation loop.
        let rec loop iFrame prev world =
            window.requestAnimationFrame(fun timestamp ->

                    // track framerate
                let cur =
                    logFramerate 100 iFrame prev timestamp

                    // reset world?
                let world =
                    if reset then
                        reset <- false
                        createWorld random
                    else world

                    // animate a frame and then loop
                animateFrame world
                    |> loop (iFrame + 1) cur)

                |> ignore

        loop 1 0.0 (createWorld random)
