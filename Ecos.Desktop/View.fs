namespace Ecos

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Layout
open Avalonia.Media

open Avalonia.FuncUI.DSL

module View =

    let private windowWidth = 1000.0
    let private windowHeight = 400.0
    let private scale = 5.0
    let private bondThickness = 1.0

    let private transformGroup =
        let group = TransformGroup()
        group.Children.Add(
            ScaleTransform(scale, scale))
        group.Children.Add(
            TranslateTransform(
                windowWidth / 2.0,
                windowHeight / 2.0))
        group

    let private brushMap =
        Map [
            Hydrophobic, Brushes.Red
            Polar, Brushes.Blue
        ]

    let private (~~)(ecosPt : Ecos.Point) =
        Avalonia.Point(ecosPt.X, ecosPt.Y)

    let renderWorld world =
        Canvas.create [
            Canvas.renderTransform transformGroup
            Canvas.renderTransformOrigin RelativePoint.TopLeft   // use world origin as transform origin
            Canvas.children [

                for i, atom in Map.toSeq world.AtomMap do
                    Ellipse.create [
                        Ellipse.width 2.0
                        Ellipse.height 2.0
                        let pt = atom.Location - Point.create (1, 1)
                        Ellipse.left pt.X
                        Ellipse.top pt.Y
                        Ellipse.fill brushMap[atom.Type]
                        Ellipse.useLayoutRounding false   // don't snap to pixels before scaling
                    ]

                let bonds =
                    world.BondMap
                        |> Map.toSeq
                        |> Seq.collect (fun (idA, ids) ->
                            Seq.choose (fun idB ->
                                if idA < idB then
                                    Some (
                                        world.AtomMap[idA],
                                        world.AtomMap[idB])
                                else None)
                                ids)

                for a, b in bonds do
                    Line.create [
                        Line.startPoint ~~a.Location
                        Line.endPoint ~~b.Location
                        Line.stroke Brushes.Black
                        Line.strokeThickness (bondThickness / scale)   // undo transform
                    ]
            ]
        ]

    let renderTemperature kT dispatch =
        DockPanel.create [
            DockPanel.dock Dock.Bottom
            DockPanel.children [
                TextBlock.create [
                    TextBlock.dock Dock.Left
                    TextBlock.text $"Temperature: %0.3f{kT}"
                    TextBlock.foreground Colors.Black
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.padding 10
                ]
                Slider.create [
                    Slider.dock Dock.Left
                    Slider.minimum 0.0
                    Slider.maximum 5.0
                    Slider.value kT
                    Slider.onValueChanged (fun value ->
                        SetTemperature value |> dispatch)
                ]
            ]
        ]

    let renderEnergy world =
        TextBlock.create [
            TextBlock.dock Dock.Bottom
            TextBlock.text $"Energy: {World.getEnergy world}"
            TextBlock.foreground Colors.Black
            TextBlock.padding 10
        ]

    let render model dispatch =
        let world = model.World
        Window.create [
            Window.title "Ecos"
            Window.width windowWidth
            Window.height windowHeight
            Window.background Brushes.WhiteSmoke
            Window.child (
                DockPanel.create [
                    DockPanel.children [
                        renderEnergy model.World
                        renderTemperature model.Temperature dispatch
                        renderWorld model.World   // last child fills
                    ]
                ])
        ]
