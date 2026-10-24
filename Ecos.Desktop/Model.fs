namespace Ecos

open System
open System.Diagnostics
open System.IO

open Elmish

open Avalonia
open Avalonia.Controls
open Avalonia.Media.Imaging

open Ecos.Experiment

/// Elmish model.
type Model =
    {
        /// Index of the selected group of scenarios in the registry.
        GroupIndex : int

        /// Index of the selected scenario within its group.
        ScenarioIndex : int

        /// RNG seed.
        Seed : int

        /// The selected scenario, being run.
        Run : Run

        /// Indicates whether updates are enabled.
        IsRunning : bool

        /// Indicates whether frames are being saved.
        IsRecording : bool

        /// Number of sweeps per timer tick.
        Speed : int

        /// Number of frames rendered per second.
        FrameRate : float
    }

/// Elmish messages.
type Message =

    /// Timer tick.
    | Tick

    /// Selects a group of scenarios.
    | SelectGroup of int

    /// Selects a scenario within the current group.
    | SelectScenario of int

    /// Sets the temperature by hand.
    | SetTemperature of float

    /// Turns updates on or off.
    | SetIsRunning of bool

    /// Turns frame recording on or off.
    | SetIsRecording of bool

    /// Sets speed.
    | SetSpeed of int

    /// Sets RNG seed.
    | SetSeed of int

    /// Resets model.
    | Reset

module Model =

    /// Creates a model that runs the given scenario from the start.
    let create groupIndex scenarioIndex seed speed =
        let scenario =
            Registry.groups[groupIndex].Scenarios[scenarioIndex]
        {
            GroupIndex = groupIndex
            ScenarioIndex = scenarioIndex
            Seed = seed
            Run = Run.create seed scenario
            IsRunning = false
            IsRecording = false
            Speed = speed
            FrameRate = 0
        }

    /// Initializes a model.
    let init () =
        create 0 0 0 200

    let private stopwatch = Stopwatch()

    let mutable private lastElapsed = stopwatch.Elapsed

    /// Runs one tick's worth of sweeps.
    let handleTick model =
        if model.IsRunning then
            let run = Run.advance model.Speed model.Run
            let frameRate =
                let prev = lastElapsed
                lastElapsed <- stopwatch.Elapsed
                1.0 / (lastElapsed - prev).TotalSeconds
            { model with
                Run = run
                FrameRate = frameRate }
        else model

    /// Handles selection of a group of scenarios.
    let handleSelectGroup groupIndex model =
        if groupIndex = model.GroupIndex then model
        else create groupIndex 0 model.Seed model.Speed

    /// Handles selection of a scenario.
    let handleSelectScenario scenarioIndex model =
        if scenarioIndex = model.ScenarioIndex then model
        else create model.GroupIndex scenarioIndex model.Seed model.Speed

    /// Handles temperature change.
    let handleSetTemperature kT (model : Model) =
        { model with
            Run = { model.Run with Schedule = Constant kT } }

    /// Handles IsRunning flag change.
    let handleSetIsRunnning isRunning model =
        if isRunning then
            stopwatch.Start()
            lastElapsed <- stopwatch.Elapsed
        else stopwatch.Stop()
        { model with IsRunning = isRunning }

    /// Handles IsRecording flag change.
    let handleSetIsRecording isRecording model =
        { model with IsRecording = isRecording }

    /// Handles speed change.
    let handleSetSpeed speed model =
        { model with Speed = speed }

    /// Handles RNG seed change.
    let handleSetSeed seed model =
        { model with Seed = seed }

    /// Handles model reset.
    let handleReset model =
        create model.GroupIndex model.ScenarioIndex model.Seed model.Speed

    /// Handles a message.
    let update msg model =
        match msg with
            | Tick -> handleTick model
            | SelectGroup index -> handleSelectGroup index model
            | SelectScenario index -> handleSelectScenario index model
            | SetTemperature kT -> handleSetTemperature kT model
            | SetIsRunning flag -> handleSetIsRunnning flag model
            | SetIsRecording flag -> handleSetIsRecording flag model
            | SetSpeed speed -> handleSetSpeed speed model
            | SetSeed seed -> handleSetSeed seed model
            | Reset -> handleReset model

    /// Saves an image of the given control to the given file.
    let private saveImage (control : Control) (path : string) =
        let size = control.Bounds.Size
        let pixels = PixelSize(int size.Width, int size.Height)
        use bitmap = new RenderTargetBitmap(pixels, Vector(96.0, 96.0))
        bitmap.Render(control)
        let options = PngBitmapEncoderOptions()
        bitmap.Save(path, options)   // PNG

    /// Directory in which to save frames.
    let private imagesDirName =
        let name = "Images"
        Directory.CreateDirectory(name) |> ignore
        name

    /// Saves a frame.
    let private saveFrame control frameNum =
        let path =
            Path.Combine(
                imagesDirName, $"Ecos{frameNum:D6}.png")
        saveImage control path

    /// One tick before each rendered frame.
    let frames (topLevel : TopLevel) isRecording : Subscribe<_> =
        fun dispatch ->
            let mutable running = true
            let mutable iFrame = 0
            let rec onFrame (_ : TimeSpan) =
                if running then
                    if isRecording then
                        saveFrame topLevel iFrame
                        iFrame <- iFrame + 1
                    dispatch Tick
                    topLevel.RequestAnimationFrame(onFrame)
            topLevel.RequestAnimationFrame(onFrame)
            { new IDisposable with
                member _.Dispose() = running <- false }

    /// Frame subscription.
    let subscribe topLevel (model : Model) : Sub<_> =
        let isRecording =
            model.IsRunning && model.IsRecording
        [ [ "frames"; string isRecording ], frames topLevel isRecording ]
