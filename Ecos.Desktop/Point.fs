namespace Ecos.Desktop

open Avalonia
open Ecos.Engine

[<AutoOpen>]
module PointExt =
    type Ecos.Engine.Point with

        /// Converts this point on an Avalonia point.
        member this.ToAvalonia() =
            Point(this.X, this.Y)
