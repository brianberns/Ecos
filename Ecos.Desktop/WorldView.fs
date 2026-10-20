namespace Ecos

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Controls.Shapes
open Avalonia.Layout
open Avalonia.Media

open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

open Wacton.Unicolour

open Ecos.Experiment

module WorldView =

    let private brushMap =
        [
            AtomType.W, "#1F4E9E"   // blue
            AtomType.X, "#2E7D32"   // green
            AtomType.Y, "#B0006B"   // red
            AtomType.Z, "#B26A00"   // orange
            AtomType.P, "#000000"   // black
            AtomType.Q, "#808080"   // gray
        ]
            |> Seq.map (fun (typ, hex) ->
                typ, Media.Brush.Parse(hex))
            |> Map

    let private (~~)(ecosPt : Ecos.Point) =
        Avalonia.Point(ecosPt.X, ecosPt.Y)

    let renderRectangle location (brush : IBrush) =
        Rectangle.create [
            Rectangle.width 2
            Rectangle.height 2
            let pt = location - Point.create (1, 1)
            Rectangle.left pt.X
            Rectangle.top pt.Y
            Rectangle.fill brush
            Rectangle.useLayoutRounding false   // don't snap to pixels before scaling
        ] :> IView

    let gradientColor (startHex : string) (endHex : string) (distance : float) =
        let a = Unicolour(startHex)
        let b = Unicolour(endHex)
        a.Mix(b, ColourSpace.Oklab, distance).Hex

    let private renderTemperature model =

        let walls = model.Run.World.Walls
        let minWallX = walls |> Seq.map _.X |> Seq.min
        let maxWallX = walls |> Seq.map _.X |> Seq.max
        let minWallY = walls |> Seq.map _.Y |> Seq.min
        let maxWallY = walls |> Seq.map _.Y |> Seq.max

        let getBrush temp =
            let distance =
                (temp - float ControlPanel.minTemperature)
                    / float (
                        ControlPanel.maxTemperature
                            - ControlPanel.minTemperature)
            gradientColor "#CBCDF2" "#F2CBCB" distance
                |> Brush.Parse

        [
            for x = minWallX to maxWallX do
                for y = minWallY to maxWallY do
                    let loc = Point.create (x, y)
                    let brush =
                        loc
                            |> Run.getTemperature model.Run 
                            |> getBrush
                    renderRectangle loc brush
        ]

    let private renderWalls (world : World) =
        [
            for loc in world.Walls do
                let pt = loc - Point.create (1, 1)
                renderRectangle pt Brushes.Gray
        ]

    let private renderAtoms model =
        let templateIds = set model.Run.Template
        [
            for atom in model.Run.World.Atoms do
                Ellipse.create [
                    Ellipse.width 2
                    Ellipse.height 2
                    let pt = atom.Location - Point.create (1, 1)   // every atom is 2x2 centered on its location
                    Ellipse.left pt.X
                    Ellipse.top pt.Y
                    if templateIds.Contains(atom.Id) then
                        Ellipse.fill brushMap[atom.Type]
                    else
                        Ellipse.stroke brushMap[atom.Type]
                        Ellipse.strokeThickness 0.4
                    Ellipse.useLayoutRounding false   // don't snap to pixels before scaling
                ] :> IView
        ]

    let private getBonds bondType (world : World) =
        world.BondTypeMap[bondType]
            |> Map.toSeq
            |> Seq.collect (fun (idA, ids) ->
                Seq.choose (fun idB ->
                    if idA < idB then
                        Some (
                            world.Atoms[idA],
                            world.Atoms[idB])
                    else None)
                    ids)

    let private renderBonds
        bondType (brush : IImmutableBrush) thickness world =
        [
            for a, b in getBonds bondType world do
                Line.create [
                    Line.startPoint ~~a.Location
                    Line.endPoint ~~b.Location
                    Line.stroke brush
                    Line.strokeThickness thickness
                ] :> IView
        ]

    let private renderStrongBonds world =
        renderBonds Strong Brushes.Black 0.2 world

    let private renderWeakBonds world =
        renderBonds Weak Brushes.HotPink 0.4 world

    let render (model : Model) =
        let scale = 6.0
        Border.create [
            Border.clipToBounds true   // keep drawing inside the available space
            Border.child (
                Canvas.create [
                    Canvas.width 0.0
                    Canvas.height 0.0
                    Canvas.horizontalAlignment HorizontalAlignment.Center   // world origin at the center
                    Canvas.verticalAlignment VerticalAlignment.Center
                    Canvas.renderTransform (ScaleTransform(scale, scale))
                    Canvas.renderTransformOrigin RelativePoint.TopLeft
                    Canvas.children [
                        yield! renderTemperature model
                        yield! renderWalls model.Run.World
                        yield! renderAtoms model
                        yield! renderStrongBonds model.Run.World
                        yield! renderWeakBonds model.Run.World
                    ]
                ])
        ]
