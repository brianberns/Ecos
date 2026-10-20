namespace Ecos

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Controls.Shapes
open Avalonia.Layout
open Avalonia.Media

open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

open Ecos.Experiment

module ControlPanel =

    let private margin = 10

    let private renderLabel text width =
        TextBlock.create [
            TextBlock.text text
            TextBlock.width width
            TextBlock.foreground Colors.Black
            TextBlock.verticalAlignment VerticalAlignment.Center
            TextBlock.margin margin
        ]

    let private labelColumnWidth = 100
    let private valueColumnWidth = 150

    /// Names of the scenario groups, and of the scenarios in each. These
    /// are built once, so that the combo boxes see the same items on
    /// every render.
    let private groupNames =
        Registry.groups |> Array.map _.Name

    let private scenarioNames =
        Registry.groups
            |> Array.map (fun group ->
                group.Scenarios |> Array.map _.Name)

    /// A combo box of the given names. Its handler is attached only
    /// once, so it must not capture anything that changes; the model
    /// ignores a selection that is already current.
    let private renderComboBox
        key (names : string[]) (index : int) (onSelect : int -> unit) =
        ComboBox.create [
            ComboBox.dataItems names
            ComboBox.selectedIndex index
            ComboBox.width valueColumnWidth
            ComboBox.margin margin
            ComboBox.tip names[index]
            ComboBox.onSelectedIndexChanged (fun i ->
                if i >= 0 then onSelect i)   // -1 means the selection was cleared
        ]
            |> View.withKey key   // a new key gives a new box, rather than one whose items change under it

    let private renderScenario (model : Model) dispatch =
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.children [
                        renderLabel "Group:" labelColumnWidth
                        renderComboBox "groups"
                            groupNames
                            model.GroupIndex
                            (SelectGroup >> dispatch)
                    ]
                ]
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.children [
                        renderLabel "Scenario:" labelColumnWidth
                        renderComboBox $"scenarios {model.GroupIndex}"
                            scenarioNames[model.GroupIndex]
                            model.ScenarioIndex
                            (SelectScenario >> dispatch)
                    ]
                ]
            ]
        ]

    let private renderSweepCount sweepCount =
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                renderLabel "Sweeps:" labelColumnWidth
                renderLabel $"{sweepCount:N0}" valueColumnWidth
            ]
        ]

    let private renderUpdateRate frameRate =
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                renderLabel "Update rate:" labelColumnWidth
                renderLabel $"{frameRate:F1}" valueColumnWidth
            ]
        ]

    let minTemperature = 0.0m
    let maxTemperature = 5.0m

    let private renderTemperature (model : Model) dispatch =
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Horizontal
            match model.Run.Schedule with
                | Constant kT ->
                    StackPanel.children [
                        renderLabel "Temperature:" labelColumnWidth
                        NumericUpDown.create [
                            NumericUpDown.minimum minTemperature
                            NumericUpDown.maximum maxTemperature
                            NumericUpDown.increment 0.05m
                            NumericUpDown.value (decimal kT)
                            NumericUpDown.width valueColumnWidth
                            NumericUpDown.margin margin
                            NumericUpDown.horizontalContentAlignment
                                HorizontalAlignment.Right
                            NumericUpDown.onValueChanged (fun value ->
                                if value.HasValue then
                                    float value.Value
                                        |> SetTemperature
                                        |> dispatch)
                        ]
                    ]
                | _ -> ()
        ]

    let private renderSpeed (speed : int) dispatch =
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                renderLabel "Speed:" labelColumnWidth
                NumericUpDown.create [
                    NumericUpDown.minimum 1
                    NumericUpDown.maximum 1000
                    NumericUpDown.increment 1
                    NumericUpDown.value speed
                    NumericUpDown.formatString "F0"
                    NumericUpDown.width valueColumnWidth
                    NumericUpDown.margin margin
                    NumericUpDown.horizontalContentAlignment
                        HorizontalAlignment.Right
                    NumericUpDown.onValueChanged (fun value ->
                        if value.HasValue then
                            int value.Value
                                |> SetSpeed
                                |> dispatch)
                ]
            ]
        ]

    let private renderSeed (seed : int) dispatch =
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                renderLabel "Seed:" labelColumnWidth
                NumericUpDown.create [
                    NumericUpDown.increment 1
                    NumericUpDown.value seed
                    NumericUpDown.formatString "F0"
                    NumericUpDown.width valueColumnWidth
                    NumericUpDown.margin margin
                    NumericUpDown.horizontalContentAlignment
                        HorizontalAlignment.Right
                    NumericUpDown.onValueChanged (fun value ->
                        if value.HasValue then
                            int value.Value
                                |> SetSeed
                                |> dispatch)
                ]
            ]
        ]

    let private renderRun run dispatch =
        ToggleButton.create [
            ToggleButton.content "Run"
            ToggleButton.isChecked (run : bool)
            ToggleButton.width 100
            ToggleButton.margin margin
            ToggleButton.foreground Colors.Black
            ToggleButton.background Colors.DarkGray
            ToggleButton.horizontalContentAlignment
                HorizontalAlignment.Center
            ToggleButton.onIsCheckedChanged (fun args ->
                let btn = args.Source :?> ToggleButton
                if btn.IsChecked.HasValue then
                    btn.IsChecked.Value
                        |> SetRun
                        |> dispatch)
        ]

    let private renderReset dispatch =
        Button.create [
            Button.content "Reset"
            Button.width 100
            Button.margin margin
            Button.foreground Colors.Black
            Button.background Colors.DarkGray
            Button.horizontalContentAlignment
                HorizontalAlignment.Center
            Button.onClick (fun _ ->
                dispatch Reset)
        ]

    let private renderButtons model dispatch =
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                renderRun model.IsRunning dispatch
                renderReset dispatch
            ]
        ]

    let render (model : Model) dispatch =
        DockPanel.create [
            DockPanel.dock Dock.Right
            DockPanel.background Colors.LightGray
            DockPanel.lastChildFill false
            DockPanel.children [
                renderScenario model dispatch
                renderSweepCount model.Run.NumSweeps
                renderUpdateRate model.UpdateRate
                renderTemperature model dispatch
                renderSpeed model.Speed dispatch
                renderSeed model.Seed dispatch
                renderButtons model dispatch
            ]
        ]
