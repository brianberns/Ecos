namespace Ecos.Desktop

open Avalonia.Media
open Ecos.Engine

module Photon =

    let radius = Atom.radius / 10.0

    /// Draws the given photon.
    let draw (ctx : DrawingContext) (photon : Photon) =
        ctx.DrawEllipse(
            Brushes.Silver, null,
            photon.Location.ToAvalonia(),
            radius, radius)
