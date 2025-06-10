namespace Ecos.Desktop

[<AutoOpen>]
module PointExt =
    type Ecos.Engine.Point with

        /// Converts this point to an Avalonia point.
        member this.ToAvalonia() =
            Avalonia.Point(this.X, this.Y)
