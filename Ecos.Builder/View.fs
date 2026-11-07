namespace Ecos.Builder

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media

open Ecos
open Ecos.Experiment
open Fields

/// The window: the spec (its name, where it came from, and its editor)
/// on the left; the run, with its controls above it and its status
/// below it, on the right.
module View =

    /// Name, and saved specs.
    let private renderSpecBar (model : Model) dispatch =
        column [
            row [
                label "Name" 60.0
                textBox "spec.name" 220.0 model.Spec.Name (fun text ->
                    if text <> model.Spec.Name then dispatch (Rename text))
                button "spec.save" "Save" (fun () -> dispatch Save)
                button "spec.new" "New" (fun () -> dispatch New)
            ]
            row [
                label "Saved" 60.0
                ComboBox.create [
                    ComboBox.dataItems model.SavedNames
                    ComboBox.width 300.0
                    ComboBox.margin margin
                    ComboBox.placeholderText (if model.SavedNames.Length = 0 then "None yet" else "Load a saved spec…")
                    ComboBox.onSelectedIndexChanged ((fun i ->
                        if i >= 0 then dispatch (LoadSaved model.SavedNames[i])), SubPatchOptions.OnChangeOf (box model.SavedNames))
                ]
                    |> View.withKey $"""saved {model.LoadCount} {System.String.Join("|", model.SavedNames)}"""
            ]
        ]

    /// Name of the frame around the world view.
    let private frameName = "worldFrame"

    /// Copies the visible part of the world to the clipboard.
    let private copyView (source : obj) =
        TopLevel.GetTopLevel(source :?> Visual)
            |> Avalonia.VisualTree.VisualExtensions.GetVisualDescendants
            |> Seq.tryPick (fun v ->
                match v with
                    | :? Border as border when border.Name = frameName -> Some border
                    | _ -> None)
            |> Option.iter Snapshot.copy

    /// Run controls.
    let private renderRunBar (model : Model) dispatch =
        row [
            ToggleButton.create [
                ToggleButton.content (if model.IsRunning then "Pause" else "Run")
                ToggleButton.isChecked model.IsRunning
                ToggleButton.isEnabled model.Error.IsNone
                ToggleButton.width 80.0
                ToggleButton.margin margin
                ToggleButton.horizontalContentAlignment HorizontalAlignment.Center
                ToggleButton.onIsCheckedChanged ((fun args ->
                    let btn = args.Source :?> ToggleButton
                    if btn.IsChecked.HasValue then dispatch (SetIsRunning btn.IsChecked.Value)), SubPatchOptions.Never)
            ]
            button "run.reset" "Reset" (fun () -> dispatch Reset)
            Button.create [
                Button.content "Copy view"
                Button.margin margin
                Button.tip "Copy the visible part of the world to the clipboard as an image, at screen resolution"
                Button.onClick ((fun args -> copyView args.Source), SubPatchOptions.Never)
            ]
            label "Seed" 35.0
            intSpinner "run.seed" 110.0 0 System.Int32.MaxValue 1 model.Seed (SetSeed >> dispatch)
            label "Sweeps per frame" 110.0
            intSpinner "run.speed" 120.0 1 100_000 50 model.Speed (SetSpeed >> dispatch)
        ]

    /// Run status.
    let private renderStatus (model : Model) =
        let run = model.Run
        let kT = Run.getTemperature run (Build.center run.World)
        let phase = Run.tryGetPhaseName run |> Option.defaultValue "-"
        let fps = if model.IsRunning then $"{model.FrameRate:F0} fps" else "paused"
        row [
            label $"Sweep {run.NumSweeps:N0}" 140.0
            label $"Phase {phase}" 110.0
            label $"kT {kT:G3}" 80.0
            label $"{run.World.Atoms.Length} atoms" 90.0
            label fps 70.0
        ]

    /// Why the spec can't be built, if it can't, or the latest status.
    let private renderMessage (model : Model) =
        match model.Error with
            | Some msg ->
                TextBlock.create [
                    TextBlock.text $"Can't build this spec: {msg}. Showing the last one that could be built."
                    TextBlock.foreground Brushes.DarkRed
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.margin margin
                ] :> IView
            | None -> note model.Status

    /// The frame around the world view that holds the given element.
    let private findFrame (source : obj) =
        let rec loop (visual : Visual) =
            match visual with
                | null -> null
                | :? Border as border when border.Name = frameName -> visual
                | _ -> loop (Avalonia.VisualTree.VisualExtensions.GetVisualParent(visual))
        loop (source :?> Visual)

    /// The world, which the mouse wheel zooms about the pointer, a drag
    /// scrolls around the torus, and a double-click fits to the view and
    /// unscrolls.
    let private renderWorld (model : Model) dispatch =
        let ox, oy = model.Offset
        let isDragging = model.DragFrom.IsSome
        Border.create [
            Border.name frameName
            Border.margin 8.0
            Border.clipToBounds true
            Border.background Brushes.Transparent   // so that the mouse is seen everywhere in it
            Border.onPointerWheelChanged ((fun args ->
                let pt = args.GetPosition(findFrame args.Source)
                dispatch (ZoomAt (pt.X, pt.Y, args.Delta.Y))
                args.Handled <- true), SubPatchOptions.Never)
            Border.onPointerPressed ((fun args ->
                let pt = args.GetPosition(findFrame args.Source)
                if args.ClickCount = 2 then dispatch ResetZoom
                else dispatch (DragStart (pt.X, pt.Y))), SubPatchOptions.Never)
            Border.onPointerMoved ((fun args ->
                if isDragging then
                    let frame = findFrame args.Source
                    let pt = args.GetPosition(frame)
                    dispatch (DragTo (pt.X, pt.Y, frame.Bounds.Width, frame.Bounds.Height))), SubPatchOptions.OnChangeOf isDragging)
            Border.onPointerReleased ((fun _ -> dispatch DragEnd), SubPatchOptions.Never)
            Border.child (
                Border.create [
                    Border.renderTransformOrigin (RelativePoint(0.0, 0.0, RelativeUnit.Absolute))
                    Border.renderTransform (MatrixTransform(Matrix(model.Zoom, 0.0, 0.0, model.Zoom, ox, oy)))
                    Border.child (WorldView.render model.Run model.FamilyColors model.Shift)
                ])
        ]

    let render (model : Model) dispatch =
        DockPanel.create [
            DockPanel.children [
                Border.create [
                    Border.dock Dock.Left
                    Border.width 520.0
                    Border.background (SolidColorBrush(Color.Parse("#F3F3F3")))
                    Border.borderBrush Brushes.LightGray
                    Border.borderThickness (Thickness(0.0, 0.0, 1.0, 0.0))
                    Border.child (
                        DockPanel.create [
                            DockPanel.children [
                                Border.create [
                                    Border.dock Dock.Top
                                    Border.padding 4.0
                                    Border.child (renderSpecBar model dispatch)
                                ]
                                Editor.render model dispatch
                            ]
                        ])
                ]
                Border.create [
                    Border.dock Dock.Top
                    Border.padding 4.0
                    Border.child (renderRunBar model dispatch)
                ]
                Border.create [
                    Border.dock Dock.Bottom
                    Border.padding 4.0
                    Border.child (column [ renderStatus model; renderMessage model ])
                ]
                renderWorld model dispatch
            ]
        ]
