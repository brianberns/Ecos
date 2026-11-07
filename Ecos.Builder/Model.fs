namespace Ecos.Builder

open System
open System.Diagnostics
open System.IO

open Elmish

open Avalonia.Controls

open Ecos
open Ecos.Experiment

/// Elmish model. The spec is what the editor shows; the run is the spec
/// as last built. Any change to the spec rebuilds the run from the start.
type Model =
    {
        /// Scenario being edited.
        Spec : ScenarioSpec

        /// Why the spec can't be built, if it can't. The run is then
        /// the last one that could be.
        Error : string option

        /// The spec, being run.
        Run : Run

        /// RNG seed.
        Seed : int

        /// Number of sweeps per frame.
        Speed : int

        /// Whether the run is advancing.
        IsRunning : bool

        /// Frames shown per second.
        FrameRate : float

        /// Incremented whenever a different spec is loaded, so that the
        /// editor is built afresh.
        LoadCount : int

        /// Names of the specs saved in the specs folder.
        SavedNames : string[]

        /// Latest message for the status bar, e.g. "Saved".
        Status : string
    }

/// Elmish messages.
type Message =

    /// Frame tick.
    | Tick

    /// Starts or stops the run.
    | SetIsRunning of bool

    /// Sets the number of sweeps per frame.
    | SetSpeed of int

    /// Sets the RNG seed, and restarts.
    | SetSeed of int

    /// Restarts the run from the start.
    | Restart

    /// Changes the spec, and restarts if it changed.
    | Edit of (ScenarioSpec -> ScenarioSpec)

    /// Renames the spec, without restarting.
    | Rename of string

    /// Starts a new spec, with the default values.
    | New

    /// Loads the saved spec with the given name.
    | LoadSaved of string

    /// A saved spec was read, or couldn't be.
    | Loaded of Result<ScenarioSpec, string>

    /// Saves the spec under its name.
    | Save

    /// The spec was saved, or couldn't be.
    | Saved of Result<string, string>

module Model =

    /// A new spec: a 160 x 160 soup of W, X, Y, Z, P and Q, each 2% of
    /// the sites, with no strands, the standard physics (scenario A's),
    /// and a constant kT of 0.25.
    let defaultSpec : ScenarioSpec =
        {
            Name = "untitled"
            Width = 160
            Height = 160
            Walls = [||]
            Strands = [||]
            WeakBonds = [||]
            Occupancy =
                Map [ "W", 2.0; "X", 2.0; "Y", 2.0; "Z", 2.0; "H", 0.0; "P", 2.0; "Q", 2.0 ]
            FoodWithin = None
            Parameters = ScenarioSpec.ofParameters (Scenario.createScenarioA false).Parameters
            Schedule = Constant 0.25
        }

    /// Folder in which specs are saved: "specs" in the repository (the
    /// folder holding the solution file), if this runs from within one,
    /// otherwise in the current folder.
    let specsFolder =
        let rec findRoot (dir : DirectoryInfo) =
            if isNull dir then None
            elif File.Exists(Path.Combine(dir.FullName, "Ecos.slnx")) then Some dir.FullName
            else findRoot dir.Parent
        let root =
            findRoot (DirectoryInfo(AppContext.BaseDirectory))
                |> Option.defaultValue Environment.CurrentDirectory
        Path.Combine(root, "specs")

    /// Path of the saved spec with the given name.
    let private getPath name =
        Path.Combine(specsFolder, name + ".json")

    /// Names of the saved specs.
    let private getSavedNames () =
        if Directory.Exists specsFolder then
            Directory.GetFiles(specsFolder, "*.json")
                |> Array.map Path.GetFileNameWithoutExtension
                |> Array.sort
        else Array.empty

    /// A name that can be used as a file name.
    let toFileName (name : string) =
        let invalid = Path.GetInvalidFileNameChars()
        let name = String(name.Trim() |> Seq.map (fun c -> if Array.contains c invalid then '_' else c) |> Seq.toArray)
        if String.IsNullOrWhiteSpace name then "untitled" else name

    /// Builds a run of the given spec, or says why it can't be built.
    let private tryCreateRun seed spec =
        try Ok (Run.create seed (ScenarioSpec.toScenario spec))
        with ex -> Error ex.Message

    /// Model with the given spec, rebuilt from the start, keeping the
    /// current run if the spec can't be built.
    let private rebuild spec model =
        match tryCreateRun model.Seed spec with
            | Ok run -> { model with Spec = spec; Run = run; Error = None }
            | Error msg -> { model with Spec = spec; Error = Some msg; IsRunning = false }

    /// Model with a newly loaded spec.
    let private load spec status model =
        { rebuild spec model with
            LoadCount = model.LoadCount + 1
            Status = status }

    /// Initializes a model.
    let init () =
        let spec = defaultSpec
        let model =
            {
                Spec = spec
                Error = None
                Run = Run.create 0 (ScenarioSpec.toScenario spec)
                Seed = 0
                Speed = 200
                IsRunning = false
                FrameRate = 0.0
                LoadCount = 0
                SavedNames = getSavedNames ()
                Status = "New spec"
            }
        model, Cmd.none

    let private stopwatch = Stopwatch()

    let mutable private lastElapsed = stopwatch.Elapsed

    /// Runs one frame's worth of sweeps.
    let private handleTick model =
        if model.IsRunning then
            let run = Run.advance model.Speed model.Run
            let frameRate =
                let prev = lastElapsed
                lastElapsed <- stopwatch.Elapsed
                1.0 / (lastElapsed - prev).TotalSeconds
            { model with Run = run; FrameRate = frameRate }
        else model

    /// Starts or stops the run.
    let private handleSetIsRunning isRunning model =
        if isRunning then
            stopwatch.Start()
            lastElapsed <- stopwatch.Elapsed
        else stopwatch.Stop()
        { model with IsRunning = isRunning && model.Error.IsNone }

    /// Saves the given spec under its name.
    let private saveSpec (spec : ScenarioSpec) =
        Directory.CreateDirectory(specsFolder) |> ignore
        let name = toFileName spec.Name
        ScenarioSpec.save (getPath name) spec
        name

    /// Handles a message.
    let update msg model =
        match msg with
            | Tick -> handleTick model, Cmd.none
            | SetIsRunning flag -> handleSetIsRunning flag model, Cmd.none
            | SetSpeed speed -> { model with Speed = max 1 speed }, Cmd.none
            | SetSeed seed ->
                if seed = model.Seed then model, Cmd.none
                else rebuild model.Spec { model with Seed = seed }, Cmd.none
            | Restart -> rebuild model.Spec model, Cmd.none
            | Edit f ->
                let spec =
                    try Ok (f model.Spec)
                    with ex -> Error ex.Message
                match spec with
                    | Ok spec when spec = model.Spec -> model, Cmd.none
                    | Ok spec -> rebuild spec model, Cmd.none
                    | Error msg -> { model with Status = msg }, Cmd.none
            | Rename name -> { model with Spec = { model.Spec with Name = name } }, Cmd.none
            | New -> load defaultSpec "New spec" model, Cmd.none
            | LoadSaved name ->
                let read () = ScenarioSpec.load (getPath name)
                model, Cmd.OfFunc.either read () (Ok >> Loaded) (fun ex -> Loaded (Error $"Can't load {name}: {ex.Message}"))
            | Loaded (Ok spec) -> load spec $"Loaded {spec.Name}" model, Cmd.none
            | Loaded (Error msg) -> { model with Status = msg }, Cmd.none
            | Save ->
                model, Cmd.OfFunc.either saveSpec model.Spec (Ok >> Saved) (fun ex -> Saved (Error ex.Message))
            | Saved (Ok name) ->
                { model with
                    SavedNames = getSavedNames ()
                    Status = $"Saved {getPath name}" }, Cmd.none
            | Saved (Error msg) -> { model with Status = $"Can't save: {msg}" }, Cmd.none

    /// One tick before each rendered frame.
    let private frames (topLevel : TopLevel) : Subscribe<_> =
        fun dispatch ->
            let mutable running = true
            let rec onFrame (_ : TimeSpan) =
                if running then
                    dispatch Tick
                    topLevel.RequestAnimationFrame(onFrame)
            topLevel.RequestAnimationFrame(onFrame)
            { new IDisposable with
                member _.Dispose() = running <- false }

    /// Frame subscription, while running.
    let subscribe topLevel (model : Model) : Sub<_> =
        if model.IsRunning then [ [ "frames" ], frames topLevel ]
        else []
