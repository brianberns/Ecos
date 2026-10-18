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

module View =

    let render model dispatch =
        Window.create [
            Window.title "Ecos"
            Window.width 1300
            Window.height 1000
            Window.background Brushes.WhiteSmoke
            Window.windowStartupLocation WindowStartupLocation.CenterScreen
            Window.child (
                DockPanel.create [
                    DockPanel.children [
                        ControlPanel.render model dispatch
                        WorldView.render model   // last child fills
                    ]
                ])
        ]
