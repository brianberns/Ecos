namespace Ecos.Desktop

open Avalonia.Media
open Ecos.Engine

module Photon =

    let radius = Atom.radius / 4.0

    /// Draws the given photon.
    let draw (ctx : DrawingContext) (photon : Photon) =
        ctx.DrawEllipse(
            Brushes.LightGray, Atom.pen,
            photon.Location.ToAvalonia(),
            radius, radius)
