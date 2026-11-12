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

        /// Strong-bond color of each family in the run, as the regular
        /// expression of a strand's pattern and a color.
        FamilyColors : (Text.RegularExpressions.Regex * string)[]

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

        /// Incremented with each edit. The run is rebuilt only once the
        /// edits pause, so a burst of them costs one rebuild.
        EditCount : int

        /// Occupancies when the slider that scales them all began to
        /// move, so that dragging it keeps their ratios although each is
        /// rounded to a whole percent.
        SliderBase : Map<string, float> option

        /// Magnification of the world view (1 fits the window).
        Zoom : float

        /// Offset of the magnified world view, in view coordinates.
        Offset : float * float

        /// How far the world has been scrolled around the torus, in
        /// world units.
        Shift : float * float

        /// Where a drag to scroll the world last was, if one is under way.
        DragFrom : (float * float) option
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

    /// Resets the run to its start, without running it.
    | Reset

    /// Changes the spec, and restarts once the edits pause.
    | Edit of (ScenarioSpec -> ScenarioSpec)

    /// Rebuilds the run if there has been no edit since the given one.
    | RebuildAfter of int

    /// Scales every type's occupancy to the given total, rounding each
    /// to a tenth of a percent.
    | ScaleOccupancy of float

    /// Zooms the view by the given number of wheel steps (in, if
    /// positive) about the given point.
    | ZoomAt of float * float * float

    /// Fits the world to the view again.
    | ResetZoom

    /// Starts a drag to scroll the world, at the given point.
    | DragStart of float * float

    /// Continues a drag to the given point, in a view of the given
    /// width and height.
    | DragTo of float * float * float * float

    /// Ends a drag.
    | DragEnd

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
    /// the sites, with no strands, scenario C's physics (strong bonds that survive the heat),
    /// and a cycle of cold (kT 0.25, 200,000 sweeps) and hot (kT 3,
    /// 100,000 sweeps).
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
            Parameters = ScenarioSpec.ofParameters (Scenario.createScenarioC false).Parameters
            Schedule =
                Cycle [|
                    { Name = Some "cold"; Temperature = 0.25; NumSweeps = 200_000; HydrolysisRate = None; CapPeelBarrier = None }
                    { Name = Some "hot"; Temperature = 3.0; NumSweeps = 100_000; HydrolysisRate = None; CapPeelBarrier = None }
                |]
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

    /// Family colors of the given spec's strands, in order (so the first
    /// strand's color wins where two families overlap).
    let private getFamilyColors (spec : ScenarioSpec) =
        spec.Strands
            |> Array.choose (fun st ->
                match st.Color, ScenarioSpec.tryParsePattern st.Pattern with
                    | Some color, Ok terms -> Some (ScenarioSpec.toRegex terms, color)
                    | _ -> None)

    /// Builds a run of the given spec, or says why it can't be built.
    let private tryCreateRun seed spec =
        try Ok (Run.create seed (ScenarioSpec.toScenario spec))
        with ex -> Error ex.Message

    /// Model with the given spec, rebuilt from the start, keeping the
    /// current run if the spec can't be built.
    let private rebuild spec model =
        match tryCreateRun model.Seed spec with
            | Ok run -> { model with Spec = spec; Run = run; FamilyColors = getFamilyColors spec; Error = None }
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
                FamilyColors = getFamilyColors spec
                Seed = 0
                Speed = 200
                IsRunning = false
                FrameRate = 0.0
                LoadCount = 0
                SavedNames = getSavedNames ()
                Status = "New spec"
                EditCount = 0
                SliderBase = None
                Zoom = 1.0
                Offset = (0.0, 0.0)
                Shift = (0.0, 0.0)
                DragFrom = None
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

    /// Time to wait after an edit before rebuilding, in case another
    /// follows.
    let private rebuildDelay = TimeSpan.FromMilliseconds 250.0

    /// Command to rebuild after the given edit, if no other follows.
    let private rebuildLater editCount : Cmd<Message> =
        Cmd.ofEffect (fun dispatch ->
            Avalonia.Threading.DispatcherTimer.RunOnce(
                (fun () -> dispatch (RebuildAfter editCount)),
                rebuildDelay) |> ignore)

    /// Model with the given spec, to be rebuilt once edits pause.
    let private editSpec spec model =
        let editCount = model.EditCount + 1
        { model with Spec = spec; EditCount = editCount }, rebuildLater editCount

    /// Largest magnification.
    let private maxZoom = 40.0

    /// Model zoomed by the given number of wheel steps about the given
    /// point, which stays where it is.
    let private zoomAt x y steps model =
        let zoom = model.Zoom * Math.Pow(1.2, steps) |> max 1.0 |> min maxZoom
        if zoom = 1.0 then { model with Zoom = 1.0; Offset = (0.0, 0.0) }
        else
            let ox, oy = model.Offset
            let factor = zoom / model.Zoom
            { model with
                Zoom = zoom
                Offset = (x - (x - ox) * factor, y - (y - oy) * factor) }

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
            | Reset -> rebuild model.Spec model, Cmd.none
            | Edit f ->
                let spec =
                    try Ok (f model.Spec)
                    with ex -> Error ex.Message
                match spec with
                    | Ok spec when spec = model.Spec -> model, Cmd.none
                    | Ok spec -> editSpec spec { model with SliderBase = None }
                    | Error msg -> { model with Status = msg }, Cmd.none
            | RebuildAfter editCount ->
                if editCount = model.EditCount then rebuild model.Spec model, Cmd.none
                else model, Cmd.none
            | ScaleOccupancy total ->
                let baseOccupancy = model.SliderBase |> Option.defaultValue model.Spec.Occupancy
                let baseTotal = baseOccupancy |> Map.toSeq |> Seq.sumBy snd
                let total = total |> max 0.0 |> min 100.0
                let occupancy =
                    if baseTotal > 0.0 then
                        baseOccupancy |> Map.map (fun _ p -> Math.Round(p * total / baseTotal, 1))
                    else model.Spec.Occupancy
                let spec = { model.Spec with Occupancy = occupancy }
                let model = { model with SliderBase = Some baseOccupancy }
                if spec = model.Spec then model, Cmd.none
                else editSpec spec model
            | ZoomAt (x, y, steps) -> zoomAt x y steps model, Cmd.none
            | ResetZoom -> { model with Zoom = 1.0; Offset = (0.0, 0.0); Shift = (0.0, 0.0) }, Cmd.none
            | DragStart (x, y) -> { model with DragFrom = Some (x, y) }, Cmd.none
            | DragTo (x, y, viewWidth, viewHeight) ->
                match model.DragFrom with
                    | Some (x0, y0) ->
                        let size = model.Run.World.Size
                        let scale =   // view units per world unit
                            model.Zoom * min (viewWidth / float size.X) (viewHeight / float size.Y)
                        let sx, sy = model.Shift
                        { model with
                            Shift = (sx - (x - x0) / scale, sy - (y - y0) / scale)
                            DragFrom = Some (x, y) }, Cmd.none
                    | None -> model, Cmd.none
            | DragEnd -> { model with DragFrom = None }, Cmd.none
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
