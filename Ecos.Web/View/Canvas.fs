namespace Ecos.Web

open System

open Browser
open Browser.Types

open Ecos

module Canvas =

    /// Number of engine time steps per frame.
    let stepsPerFrame = 1

        // initialize canvas
    let canvas =
        document.getElementById "canvas"
            :?> HTMLCanvasElement

        // initialize drawing context
    let ctx = canvas.getContext_2d()
    ctx.lineWidth <- 0.05

    /// Extent of the world.
    let extentMin, extentMax =
        let width = 40.0
        let height =
            canvas.height * width / canvas.width
        let pt = (Point.create width height) / 2.0
        -pt, pt

        // initialize reset button
    let mutable reset = false
    let btnReset =
        document.getElementById "reset"
            :?> HTMLButtonElement
    btnReset.onclick <- (fun _ -> reset <- true)

        // initialize number of particles button
    let txtNumParticles =
        document.getElementById "numParticles"
            :?> HTMLInputElement

    /// Animates one frame.
    let animateFrame world =

            // move particles
        let world =
            (world, [1 .. stepsPerFrame])
                ||> Seq.fold (fun world _ ->
                    World.step world)

            // prepare to draw
        ctx.clearRect(0, 0, canvas.width, canvas.height)
        ctx.translate(canvas.width / 2.0, canvas.height / 2.0)
        let s = canvas.width / (extentMax.X - extentMin.X)
        ctx.scale(s, s)

            // draw the world
        World.draw ctx world

            // reset transform
        ctx.setTransform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)

        world

    /// Log framerate.
    let logFramerate check iFrame prev cur =
        if iFrame % check = 0 then
            console.log(
                $"%.3f{float check * 1000.0 / (cur - prev)} frames/sec")
            cur
        else prev

    /// Animation loop.
    let animate () =

            // random number generator
        let random =
            let seed = DateTime.Now.Millisecond
            console.log($"Random seed: {seed}")
            Random(seed)

        let createWorld () =
            let numParticles =
                System.Int32.Parse txtNumParticles.value
            Web.World.create random extentMin extentMax numParticles

        let rec loop iFrame prev world =
            window.requestAnimationFrame(fun timestamp ->
                let cur =
                    logFramerate 100 iFrame prev timestamp
                let world =
                    if reset then
                        reset <- false
                        createWorld ()
                    else world
                animateFrame world
                    |> loop (iFrame + 1) cur)
                |> ignore

        loop 1 0.0 (createWorld ())
