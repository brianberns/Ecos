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
            button "run.restart" "Restart" (fun () -> dispatch Restart)
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
                Border.create [
                    Border.padding 8.0
                    Border.child (WorldView.render model.Run)
                ]
            ]
        ]
